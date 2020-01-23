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

# Chargement d'une image en niveaux de gris

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.image as mpimg


def rdfReadGreyImage(nom):
    return mpimg.imread(f'images/{nom}')


# Calcul de la surface d'une forme
def rdfSurface(img):
    return sum(img)


# Calcul d'un moment geometrique
def rdfMoment(img, p, q):
    x = [x ** p for x in range(1, img.shape[0] + 1)]
    y = [y ** q for y in range(1, img.shape[1] + 1)]
    return np.dot(np.dot(x, img), y)


img = rdfReadGreyImage("rdf-rectangle-vertical.png")
print (img.shape)
print(rdfMoment(img, 1, 0))


# Calcul d'un moment centre
def rdfMomentCentre(img, p, q):
    # Barycentre
    s = rdfSurface(img)
    cx = rdfMoment(img, 1, 0) / s
    cy = rdfMoment(img, 0, 1) / s
    # Initialiser les vecteurs x et y
    x = [(x-cx) ** p for x in range(1, img.shape[0] + 1)]
    y = [(y-cy) ** q for y in range(1, img.shape[1] + 1)]
    # Calcul du moment centre
    return np.dot(np.dot(x, img), y)

# mainInertionAxis < - function(im, normalise=TRUE){
#     if (normalise == TRUE)
#     f < - rdfMomentCentreNormalise
#     else
#     f < - rdfMomentCentre
#     u20 < - f(im, 2, 0)
#     u11 < - f(im, 1, 1)
#     u02 < - f(im, 0, 2)
#     I < - matrix(c(u20, u11, u11, u02), nrow=2, ncol=2)
#     ev < - eigen(I)
#     eigenValues < - ev$values
#     eigenVectors < - ev$vectors
#     tenseur < - diag(eigenValues)
#     P < - cbind(eigenVectors)
#     # ce dessous est pour tourner l´image
#     # I = solve(P) %*% I %*% P
# }

# calculateMainImageAxis < - function(nom, normalise=TRUE){
#     img < - rdfReadGreyImage(nom)
#     if (interactive()) {
#         display(image, nom, method="raster", all=TRUE)
#     }
#     axis < - mainInertionAxis(img, normalise)
# }

# rdfMomentCentreNormalise < - function(img, p, q){
#     upq = rdfMomentCentre(img, p, q)
#     u00 = rdfMomentCentre(img, 0, 0)
#     normalised = upq / (u00 ** (1 + (p + q)/2))
# }

# calculateImageMoment < - function(nom, p, q, normalise=TRUE){
#     img < - rdfReadGreyImage(nom)
#     if (interactive()) {
#         display(image, nom, method="raster", all=TRUE)
#     }
#     if (normalise == TRUE){
#         f < - rdfMomentCentreNormalise
#     }
#     else{
#         f < - rdfMomentCentre
#     }
#     f(img, p, q)
# }

# rdfMomentsInvariants < - function(nom){
#     img < - rdfReadGreyImage(nom)
#     if (interactive()) {
#         display(image, nom, method="raster", all=TRUE)
#     }

# }


# rdfMomentsInvariants < - function(nom){
#     img < - rdfReadGreyImage(nom)
#     if (interactive()) {
#         display(image, nom, method="raster", all=TRUE)
#     }

#     n02 < - rdfMomentCentreNormalise(img, 0, 2)
#     n03 < - rdfMomentCentreNormalise(img, 0, 3)
#     n11 < - rdfMomentCentreNormalise(img, 1, 1)
#     n12 < - rdfMomentCentreNormalise(img, 3, 0)
#     n20 < - rdfMomentCentreNormalise(img, 2, 0)
#     n21 < - rdfMomentCentreNormalise(img, 2, 1)
#     n30 < - rdfMomentCentreNormalise(img, 3, 0)

#     hu1 < - n20 + n02
#     hu2 < - (n20 - n02)**2 + (2*n11)**2
#     hu3 < - (n30 - 3*n12) + (3*n21 - n03)**2
#     hu4 < - (n30 + n12)**2 + (n21 + n03)**2
#     hu5 < - (n30 - 3*n12)*(n30 + n12)*((n30 + n12)**2 - 3*(n21 + n03) **
#                                        2) + (3*n21-n03)*(n21+n03)*(3*(n30 + n12)**2 - (n21 + n03)**2)

#     c(hu1, hu2, hu3, hu4, hu5)
# }
