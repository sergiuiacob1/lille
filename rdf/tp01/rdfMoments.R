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
rdfReadGreyImage <- function (nom) {
  image <- readImage (paste('images/', nom, sep=''))
  if (length (dim (image)) == 2) {
    image
  } else {
    channel (image, 'red')
  }
}

# Calcul de la surface d'une forme
rdfSurface <- function (im) {
  sum (im)
}

# Calcul d'un moment geometrique
rdfMoment <- function (im, p, q) {
  x <- (1 : (dim (im)[1])) ^ p
  y <- (1 : (dim (im)[2])) ^ q
  as.numeric (rbind (x) %*% im %*% cbind (y))
}

# Calcul d'un moment centre
rdfMomentCentre <- function (im, p, q) {
  # Barycentre
  s <- rdfSurface (im)
  cx <- rdfMoment (im, 1, 0) / s
  cy <- rdfMoment (im, 0, 1) / s
  # Initialiser les vecteurs x et y
  x <- (1 : (dim (im)[1]) - cx) ^ p
  y <- (1 : (dim (im)[2]) - cy) ^ q
  # Calcul du moment centre
  as.numeric (rbind (x) %*% im %*% cbind (y))
}

rdfInertiaMatrix <- function(nom){
  img <- rdfReadGreyImage(nom)
  u20 <- rdfMomentCentre(img, 2, 0)
  u11 <- rdfMomentCentre(img, 1, 1)
  u02 <- rdfMomentCentre(img, 0, 2)
  I <- matrix(c(u20, u11, u11, u02), nrow=2, ncol=2)
}

mainInertionAxis <- function(im, normalise=TRUE){
  if (normalise == TRUE)
    f <- rdfMomentCentreNormalise
  else
    f <- rdfMomentCentre
  u20 <- f(im, 2, 0)
  u11 <- f(im, 1, 1)
  u02 <- f(im, 0, 2)
  I <- matrix(c(u20, u11, u11, u02), nrow=2, ncol=2)
  ev <- eigen(I)
  eigenValues <- ev$values
  eigenVectors <- ev$vectors
  tenseur <- diag(eigenValues)
  P <- cbind(eigenVectors)
  # ce dessous est pour tourner l´image
  # I = solve(P) %*% I %*% P
}

calculateMainImageAxis <- function(nom, normalise=TRUE){
  img <- rdfReadGreyImage(nom)
  if (interactive ()) {
    display (img, paste('images/', nom, sep=''), method="raster", all=TRUE)
  }
  print (normalise)
  axis <- mainInertionAxis(img, normalise)
}

rdfMomentCentreNormalise <- function (img, p, q){
  upq = rdfMomentCentre(img, p, q)
  u00 = rdfMomentCentre(img, 0, 0)
  normalised = upq / (u00 ** (1 + (p + q)/2))
}

calculateImageMoment <- function(nom, p, q, normalise=TRUE){
  img <- rdfReadGreyImage(nom)
  if (interactive ()) {
    display (img, paste('images/', nom, sep=''), method="raster", all=TRUE)
  }
  if (normalise == TRUE){
    f <- rdfMomentCentreNormalise
  }
  else{
    f <- rdfMomentCentre
  }
  f(img, p, q)
}

rdfMomentsInvariants <- function(nom){
  img <- rdfReadGreyImage(nom)
  if (interactive ()) {
    display (img, paste('images/', nom, sep=''), method="raster", all=TRUE)
  }
  
}


rdfMomentsInvariants <- function(nom){
  img <- rdfReadGreyImage(nom)
  if (interactive ()) {
    display (img, paste('images/', nom, sep=''), method="raster", all=TRUE)
  }
  
  n02 <- rdfMomentCentreNormalise(img, 0, 2)
  n03 <- rdfMomentCentreNormalise(img, 0, 3)
  n11 <- rdfMomentCentreNormalise(img, 1, 1)
  n12 <- rdfMomentCentreNormalise(img, 1, 2)
  n20 <- rdfMomentCentreNormalise(img, 2, 0)
  n21 <- rdfMomentCentreNormalise(img, 2, 1)
  n30 <- rdfMomentCentreNormalise(img, 3, 0)
  
  hu1 <- n20 + n02
  hu2 <- (n20 - n02)**2 + (2*n11)**2
  hu3 <- (n30 - 3*n12)**2 + (3*n21 - n03)**2
  hu4 <- (n30 + n12)**2 + (n21 + n03)**2
  hu5 <- (n30 - 3*n12)*(n30 + n12)*((n30 + n12)**2 - 3*(n21 + n03) **
                                     2) + (3*n21-n03)*(n21+n03)*(3*(n30 + n12)**2 - (n21 + n03)**2)
  
  c(hu1, hu2, hu3, hu4, hu5)
}