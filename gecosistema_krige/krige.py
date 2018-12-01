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
from gecosistema_erre import *

def forceext(pathname, newext):
    """
    forceext
    """
    (root, _) = os.path.splitext((pathname))
    pathname = root + ("." + newext if len(newext.strip()) > 0 else "")
    return (pathname)


#AUTO|UK|UK-AutoKrige|OK|OK-ADVANCED|IDW2
def Kriging(fileshp, filetif=None, formula="VALUE~1", method="OK", pixelsize=10, psill=1.0, range=900,
            nugget=1.0, buffer=0, RemoveNegativeValues=False, verbose=False):
    """
    Kriging
    """
    filetif = filetif if filetif else forceext(fileshp, "tif")
    env = {
        "fileshp": fileshp.replace("/", "\\"),
        "filetif": filetif.replace("/", "\\"),
        "sformula": formula,
        "method": method,
        "psill": psill,
        "range": range,
        "nugget": nugget,
        "pixelsize": pixelsize,
        "buffer": buffer,
        "RemoveNegativeValues": RemoveNegativeValues,
        "scriptdir": justpath(__file__).replace('/', '\\') + "\\R"
    }
    cmd = sformat(
        """"{scriptdir}\\qkrige_v4.r" "{fileshp}" "{filetif}" "{sformula}" "{method}" "{pixelsize}" "{psill}" "{range}" "{nugget}" "{buffer}" "{RemoveNegativeValues}" """,
        env)

    filetif = Rscript(cmd, verbose=verbose)
    filetif = filetif.strip('"')
    return filetif

if __name__ =="__main__":

    workdir = r"D:\Users\vlr20\Projects\GitHub\gecosistema_feflow\gecosistema_feflow\FeFlow\results"
    chdir(workdir)
    fileshp = "okrige-858894-2015-08-01.shp"
    Kriging(fileshp,verbose=True)
