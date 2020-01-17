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
  image <- readImage (nom)
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

