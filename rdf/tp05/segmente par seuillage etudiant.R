# -----------------------------------------------------------------------
# Segmente par seuillage ,
# Module RdF, reconnaissance de formes
# Copyleft (C) 2016, Universite Lille 1
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
# Répertoire de travail
# setwd("//Users//macaire//Documents//lagis-pc-serv2//enseignement//enonces//S8 Rdf//TP1")
source ("rdfSegmentation.R")

# Chargement de l'image
nom <- "2classes_100_100_8bits_2016.png"
image <- rdfReadGreyImage (nom)

# Calcul et affichage de son histogramme
nbins <- 256
h <- hist (as.vector (image), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

# Segmentation par binarisation 0.3
seuil <- 0.3
binaire30 <- (image - seuil) >= 0

# Segmentation par binarisation 0.35
seuil <- 0.35
binaire35 <- (image - seuil) >= 0

# Affichage des images binarisées
display (binaire30, "image binaire 0.3", method="raster", all=TRUE)
display (binaire35, "image binaire 0.35", method="raster", all=TRUE)