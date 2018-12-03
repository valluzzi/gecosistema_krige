#-------------------------------------------------------------------------------
# Licence:
# Copyright (c) 2012-2017 Luzzi Valerio for Gecosistema S.r.l.
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
#
# Name:
# Purpose:     Kriging
#
# Author:      Luzzi Valerio
#
# Created:     05/09/2017
#-------------------------------------------------------------------------------

#SRS =  "+proj=tmerc +lat_0=0 +lon_0=9 +k=0.9996 +x_0=500092 +y_0=-3999800 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#
#Default SRS= epsg:3857 if not defined in shape
SRS = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
#------------------------------------------------------------------------------
#	string functions
#------------------------------------------------------------------------------
len<-function(arr)length(arr)
contains<-function(text,search){return(length(grep(search,text))>0)}
alltrim <- function (text) gsub("^\\s+|\\s+$", "",text)
upper<-function(text)toupper(text)
lower<-function(text)tolower(text)
left<-function(text,n)substr(text,1,n)
juststem<-function(text) sub("^([^.]*).*", "\\1", basename(text)) 
justpath<-function(text)dirname(text)
justext<-function(text) substr(text,nchar(text)-2,nchar(text))
forceext<-function(text,ext) sub("^([^.]*).*", paste("\\1",ext,sep="."), text) 
mkdirs<-function(text)dir.create(text,recursive=TRUE,showWarnings = FALSE)
isDate<-function(text){res = tryCatch(length(as.Date(text))>0,error=function(e){return(FALSE)});return(res)}
logger<-function(text){f=file("log.txt",open="at");writeLines(text,f);print(text);close(f);}
progression<-function(text,perc){f=file("interp.progress",open="at");writeLines(text,f);print(text);close(f);}



#------------------------------------------------------------------------------
#	CreateMask
#------------------------------------------------------------------------------
CreateMask<-function(piezo,buffer=200,pixelsize=10){

	bbox = bbox(piezo)
	minx = bbox[1]-buffer
	miny = bbox[2]-buffer
	maxx = bbox[3]+buffer
	maxy = bbox[4]+buffer
	x= seq(minx,maxx,pixelsize)
	y= seq(miny,maxy,pixelsize)
	
	grid = expand.grid(X=x,Y=y)
	grid$Z = 0.0
	gridded(grid)=~X+Y
	proj4string(grid)=proj4string(piezo)
	#writeGDAL(grid["Z"],"buffer.tif")
	return(grid)
}

#------------------------------------------------------------------------------
#	DetectFormulaUK -  Formula for universal Kriging
#------------------------------------------------------------------------------
DetectFormulaUK <- function( prec ){

	#Creo la formula per l'Universal Kriging
	#Cerco il nome della variabile dipendente
	VALUE = "VALUE"
	cn  = names( prec )  #Nomi dei campi 
	candidates = c("VALUE","value")
	for(varname in candidates){
		if ( isTRUE(grep(varname,cn)) ){
			VALUE = varname
			break
		}
	}
	
	#Creo la formula per l'Universal Kriging
	cn = cn[cn!=VALUE] #Rimuovo "VALUE" non deve comparire a dx nella formula
	f = paste(cn,collapse="+")
	f = paste(VALUE,"~",f)
	f = as.formula(f)
	return(f)
}


#------------------------------------------------------------------------------
#	Kriging | IDW
#------------------------------------------------------------------------------
Kriging <- function( fileshp, filetif="", sformula="VALUE~1", type ="AUTO", pixelsize=100, psill=1, range=900, nugget=1, buffer=0, RemoveNegativeValues=FALSE)
{
    prec = readOGR(fileshp,juststem(fileshp))
    if (is.na(proj4string(prec)))
        proj4string(prec) = CRS(SRS)

	print(paste("Number of points to krige",length(prec)))

	dem <- CreateMask(prec, buffer, pixelsize)
	xy  <- as.data.frame(coordinates(dem))
	dem@data$x<-xy$X
	dem@data$y<-xy$Y

	#Rinomino la banda1 in z 
	colnames(dem@data)[1]="z"

	#fieldname (in genere VALUE)
	formula   = as.formula(sformula)
	fieldname = alltrim(strsplit(sformula,"~")[[1]][1])

	#Remove na from prec$VALUE
	prec<-prec[!is.na(prec[[fieldname]]),]

	#Overlay prendo il valori nei punti delle stazioni
	ov<-over(prec,dem)
	prec$x <- ov$x
	prec$y <- ov$y
	prec$z <- ov$z	
	
	#print(prec)
	
	#UniversalKriging
	if (type =="UK"){
	  # trend model 
	  #sformula ="VALUE~X+Y+Z"
	  #formula = as.formula(sformula)    #else fm = DetectFormulaUK(prec@data)
	  lm.prec <- lm(fm, prec)
	  formula_optimal <-step(lm.prec)
	  residui = residuals(fm)
	  
	  null.vgm <- vgm(var(residui), "Sph", sqrt(areaSpatialGrid(dem))/4, nugget=0) # initial parameters
	  vgm_r    <- fit.variogram(variogram(residui~1, prec) ,model=null.vgm)
	  prec_ok  <- krige(as.formula(formula_optimal), locations=prec, newdata=dem, model=vgm_r) 
	  prediction = prec_ok[1]
	}
	
	#Automap
	if (type == "AUTO"){
	  library(automap)
		prec_ok <-autoKrige(formula,prec,dem)
		prec_ok = prec_ok$krige_output		
		prediction = prec_ok[1]
	}	
	#Ordinary Kriging
	if ( type =="OK"){
	  #psill,range,nugget are auto calculated by fitting the variogram
	  
	  #this are initial parameters to suggest an initial variogram model to fit.variogram algorithm
    psill = var(prec[[fieldname]]) #variance of prec$VALUE
    range = sqrt(areaSpatialGrid(dem))/4
    nugget = 0
	  
		null.vgm = vgm(psill, "Sph", range, nugget)
		vgm_optimal = fit.variogram(variogram(formula, prec), model=null.vgm )
		#vgm_optimal is the best variogram model that fit the empirical variogram data
		
		#print(vgm_optimal)
		#print("-----------------------")
		
		prec_ok <- krige(formula, locations=prec, newdata=dem, model=vgm_optimal)
		prediction = prec_ok[1]
	}
	#Ordinary Kriging
	if ( type =="OK-ADVANCED"){
	  #psill,range,nugget are chosen from user passed by arg
	  vgm_optimal<- vgm(psill, "Sph", range, nugget)
	  
	  print(vgm_optimal)
	  print("-----------------------")
	  
		prec_ok <- krige(formula, locations=prec, newdata=dem, model=vgm_optimal)
		prediction = prec_ok[1]
	}
	#Inverse Distance
	if( type =="IDW2"){
    prec_ok <- idw(formula, prec, dem,idp=2.0,maxdist=Inf)
    prediction = prec_ok[1]
	}
	mkdirs(justpath(filetif))
	
	#Remove negative values!!!!
	if (RemoveNegativeValues){
		prediction@data[prediction@data<0]=0}
	
	writeGDAL(prediction,filetif)	
	return(filetif)
}

#------------------------------------------------------------------------------
#	Main
#------------------------------------------------------------------------------
Main <- function(){
  #Suppress Warning
  options(warn=-1)

	#Getting command arguments
	args <- commandArgs(trailingOnly = TRUE)
	res = FALSE

	
	#load required libraries
	suppressPackageStartupMessages(library(rgdal))
	library(sp)
	library(rgdal)
	library(gstat)
	#library(tools)
	

	#Test
	filetif = ""
	if (length(args)==0){
		
		setwd("D:\\Users\\vlr20\\Projects\\GitHub\\gecosistema_feflow\\gecosistema_feflow\\FeFlow\\results")
		fileshp		= "okrige-858894-2015-08-01.shp"
		filetif   = forceext(fileshp,"tif")
	
		method = "AUTO" 
		sformula ="value~1"
		psill = 1
		range = 900
		nugget = 1
		pixelsize=10
		filetif = Kriging(fileshp, filetif, sformula, method, pixelsize, psill, range, nugget, buffer=0, RemoveNegativeValues=FALSE)
	}
	if (length(args)==10){

		Sys.getenv()

		fileshp		  = Sys.getenv("FILESHP")
		filetif		  = Sys.getenv("FILETIF")
		sformula      = Sys.getenv("SFORMULA")
		method		  = Sys.getenv("METHOD")  #AUTO|UK|UK-AutoKrige|OK|OK-ADVANCED|IDW2
		pixelsize     = as.numeric(Sys.getenv("PIXELSIZE"))
		
		psill         = as.numeric(Sys.getenv("PSILL"))
		range         = as.numeric(Sys.getenv("RANGE"))
		nugget        = as.numeric(Sys.getenv("NUGGET"))
		buffer        = as.numeric(Sys.getenv("BUFFER"))
		RemoveNegativeValues = as.logical(Sys.getenv("REMOVENEGATIVEVALUES")) #remove non-sense negative values caused by interpolation

		#print(fileshp)
		#print(method)
		#print(pixelsize)
		#print(sformula)
		#print(RemoveNegativeValues)
		#comment next 2 lines
		# ...
		#pixelsize	=50	 
		#Pay attention here!!
		filetif = Kriging(fileshp, filetif, sformula, method, pixelsize, psill, range, nugget, buffer, RemoveNegativeValues)
	}
	
	#res = paste('{"success":TRUE,"filename":"',filetif,'"}',sep="")
	return(filetif)
}

#------------------------------------------------------------------------------
#	Main-loop launch
#------------------------------------------------------------------------------
##libray = "D:\\Program Files (x86)\\SICURA\\apps\\common\\bin\\R\\R-3.3.2\\library"
##install.packages("sp",     lib=libray)
##install.packages("rgdal",  lib=libray)
##install.packages("gstat",  lib=libray)
##install.packages("automap",  lib=libray)
##install.packages("tools",  lib=libray)
print(Main())




