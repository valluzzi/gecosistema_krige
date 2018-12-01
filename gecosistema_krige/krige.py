#-------------------------------------------------------------------------------
# Licence:
# Copyright (c) 2012-2019 Valerio for Gecosistema S.r.l.
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
#
# Name:        module.py
# Purpose:
#
# Author:      Luzzi Valerio
#
# Created:
#-------------------------------------------------------------------------------
import os
from .filesystem import *
from gecosistema_erre import *
#AUTO|UK|UK-AutoKrige|OK|OK-ADVANCED|IDW2

def Kriging(fileshp, filetif=None, formula="VALUE~1", method="OK", pixelsize=10, psill=1.0, range=900,
            nugget=1.0, buffer=0, RemoveNegativeValues=False, verbose=False):
    """
    Kriging
    """
    #fileshp, filetif, sformula, method, pixelsize, psill, range, nugget, buffer = 0, RemoveNeg
    filetif = filetif if filetif else forceext(fileshp, "tif")
    env = {
        "fileshp": fileshp.replace("/", "\\"),
        "filetif": filetif.replace("/", "\\"),
        "sformula": formula,
        "method": method,
        "pixelsize": pixelsize,
        "psill": psill,
        "range": range,
        "nugget": nugget,
        "buffer": buffer,
        "RemoveNegativeValues": RemoveNegativeValues
    }

    cmd =  justpath(__file__)+"\\R\\qkrige_v4.r"
    cmd =  cmd.replace('/', '\\')

    response = Rscript(cmd, env, envAsArgs=False, verbose=verbose)
    filetif = response["data"] if response and "data" in response else ""
    return filetif

if __name__ =="__main__":
    workdir = r"D:\Program Files (x86)\GECOMAP0\apps\awesome\data\SIMS\GFS\SO2\L1HR"
    os.chdir(workdir)
    fileshp = r"conc SO2.shp"

    print Kriging(fileshp,method="IDW2",pixelsize=250,verbose=True)
