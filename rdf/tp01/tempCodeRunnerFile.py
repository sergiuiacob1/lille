# -----------------------------------------------------------------------
# Extraction d'attributs de forme,
# Module RdF, reconnaissance de formes
# Copyleft (C) 2014, Universite Lille 1
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# -----------------------------------------------------------------------

# Chargement des fonctions externes
from rdfMoments import *

# Tests to see the image orientation
def checkRectanglesMainAxis():
    axis = calculateMainImageAxis("rdf-rectangle-horizontal.png", normalise=False)
    print ("Horizontal:")
    print(axis)
    axis = calculateMainImageAxis("rdf-rectangle-vertical.png", normalise=False)
    print ("Vertical:")
    print(axis)
    axis = calculateMainImageAxis("rdf-rectangle-diagonal.png", normalise=False)
    print ("Diagonal:")
    print(axis)
    axis = calculateMainImageAxis("rdf-rectangle-diagonal-lisse.png", normalise=False)
    print ("Diagonal Lisse:")
    print(axis)

def checkSquaresMainAxis():
# pour les deux carres ce dessous, l´inertion est la meme
    axis = calculateMainImageAxis("rdf-carre-6.png", normalise=False)
    print ("Small square:")
    print(axis)
    axis = calculateMainImageAxis("rdf-carre-10.png", normalise=False)
    print ("Big square:")
    print(axis)
    axis = calculateMainImageAxis("rdf-carre-10-30deg.png", normalise=False)
    print ("Big square, rotated:")
    print(axis)

# checkRectanglesMainAxis()
checkSquaresMainAxis()
    