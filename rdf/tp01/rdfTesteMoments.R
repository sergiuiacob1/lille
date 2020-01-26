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
library ("EBImage")
source ("rdfMoments.R")

testInertiaMatrix <- function(){
    m <- rdfInertiaMatrix("rdf-rectangle-horizontal.png")
    print ("Horizontal rectangle:")
    print (m)
    m <- rdfInertiaMatrix("rdf-rectangle-vertical.png")
    print ("Vertical rectangle:")
    print (m)
    m <- rdfInertiaMatrix("rdf-rectangle-diagonal.png")
    print ("Diagonal rectangle:")
    print (m)
    m <- rdfInertiaMatrix("rdf-rectangle-diagonal-lisse.png")
    print ("Smooth diagonal rectangle:")
    print (m)
}
testInertiaMatrixForSquares <- function(){
    m <- rdfInertiaMatrix("rdf-carre-6.png")
    print ("Small square:")
    print (m)
    m <- rdfInertiaMatrix("rdf-carre-10.png")
    print ("Big square:")
    print (m)
    m <- rdfInertiaMatrix("rdf-carre-10-30deg.png")
    print ("Big square 30deg:")
    print(m)
    m <- rdfInertiaMatrix("rdf-carre-10-45deg.png")
    print ("Big square 45deg:")
    print (m)
}
testInertiaMatrix()
# # Tests to see the image orientation
# axis <- calculateMainImageAxis("rdf-rectangle-diagonal.png", normalise=FALSE)
# print(axis)
# axis <- calculateMainImageAxis("rdf-rectangle-horizontal.png", normalise=FALSE)
# print(axis)
# axis <- calculateMainImageAxis("rdf-rectangle-vertical.png", normalise=FALSE)
# print(axis)
# # pour les deux carres ce dessous, l´inertion est la meme
# axis <- calculateMainImageAxis("rdf-carre-6.png", normalise=FALSE)
# print(axis)
# axis <- calculateMainImageAxis("rdf-carre-10.png", normalise=FALSE)
# print(axis)
# # pour ces deux carres, l´orientation est un peu different
# # ca veux dire que les vecteurs propres sont different aussi
# axis <- calculateMainImageAxis("rdf-carre-10-30deg.png", normalise=FALSE)
# print(axis)
# axis <- calculateMainImageAxis("rdf-carre-10-45deg.png", normalise=FALSE)
# print(axis)

# # Normalised moment
# # If we normalise the moment, the squares have the same moment
# mom <- calculateImageMoment("rdf-carre-6.png", 0, 0, normalise=FALSE)
# print (mom)
# mom <- calculateImageMoment("rdf-carre-6.png", 0, 0, normalise=TRUE)
# print (mom)
# mom <- calculateImageMoment("rdf-carre-10.png", 0, 0, normalise=FALSE)
# print (mom)
# mom <- calculateImageMoment("rdf-carre-10.png", 0, 0, normalise=TRUE)
# print (mom)

# mom <- calculateImageMoment("rdf-carre-10-30deg.png", 0, 0, normalise=FALSE)
# print (mom)
# mom <- calculateImageMoment("rdf-carre-10-30deg.png", 0, 0, normalise=TRUE)
# print (mom)

# # Let's see the inertion with the normalised moment
# axis <- calculateMainImageAxis("rdf-triangle-10.png", normalise=FALSE)
# print(axis)
# axis <- calculateMainImageAxis("rdf-triangle-10.png", normalise=TRUE)
# print(axis)

# axis <- calculateMainImageAxis("rdf-triangle-10-45deg.png", normalise=FALSE)
# print(axis)
# axis <- calculateMainImageAxis("rdf-triangle-10-45deg.png", normalise=TRUE)
# print(axis)

# # For the same figure, the eigen vectors stay the same, even if the normalise the moment
# # So the moment is useful for scaling and it does not affect the rotation

# # Hu
# rdfMomentsInvariants("rdf-chiffre-0.png")
# rdfMomentsInvariants("rdf-chiffre-1.png")
# rdfMomentsInvariants("rdf-chiffre-7.png")
# # values are different for digits
# rdfMomentsInvariants("rdf-triangle-10.png")
# rdfMomentsInvariants("rdf-triangle-10-45deg.png")
# # Hu invariants are also different for rotated shapes

# rdfMomentsInvariants("rdf-carre-6.png")
# rdfMomentsInvariants("rdf-carre-10.png")
# # Hu1 is different for forms of the same shape, but different scale


