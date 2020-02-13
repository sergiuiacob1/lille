# -----------------------------------------------------------------------
# Extraction d'attributs de pixels pour la classification,
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
source ("rdfSegmentation.R")

# Chargement d'une image
nom <- "rdf-2-classes-texture-0.png"
image <- rdfReadGreyImage (nom)

# Calcul et affichage de son histogramme
nbins <- 256
h <- hist (as.vector (image), breaks = seq (0, 1, 1 / nbins))

# Segmentation par binarisation
seuil <- 0.5
binaire <- (image - seuil) >= 0

# Affichage des deux images
if (interactive ()) {
  display (image, nom, method="raster", all=TRUE)
  display (binaire, "image binaire", method="raster", all=TRUE)
}

buildHistogram <- function (nom, nbins){
  image <- rdfReadGreyImage (nom)
  h <- hist (as.vector (image), breaks = seq (0, 1, 1 / nbins), main = nom)
}

buildHistogram("rdf-2-classes-texture-1.png", 256)

buildBinaryImage <- function(nom, threshold){
  image <- rdfReadGreyImage (nom)
  binaire <- (image - threshold) >= 0
  # a "trick" to make the error calculation more precise
  bin_1 = sum(binaire)
  bin_0 = dim(binaire)[1] * dim(binaire)[2] - bin_1
  if (bin_0 < bin_1)
    binaire = 1 - binaire
  
  # reference image
  reference <- rdfReadGreyImage ("rdf-masque-ronds.png")
  # results
  error = sum(binaire != reference) / (dim(reference)[1] * dim(reference)[2])
  error = round (error, 4) * 100
  info <- sprintf("%s\nthreshold=%s\nerror=%s%%", nom, threshold, error)
  if (interactive()){
    display (binaire, "image binaire", method="raster", all=TRUE)
    legend(x=0.5, y=dim(binaire)[2]/2, info, bg = "lightgreen", box.col = "lightgreen", yjust=0.5)
  }
}

buildBinaryImage("rdf-2-classes-texture-4.png", 0.48)


buildTextureHistograms <- function (nom, taille){
  image <- rdfReadGreyImage (nom)
  image <- rdfTextureEcartType(image, taille)
  h <- hist (as.vector (image), breaks = seq (0, 1, 1 / nbins), main = nom)
}

buildTextureHistograms("rdf-2-classes-texture-0.png", 4)


buildBinaryTextureImage <- function (nom, taille, threshold){
  image <- rdfReadGreyImage (nom)
  image <- rdfTextureEcartType(image, taille)
  
  binaire <- (image - threshold) >= 0
  # a "trick" to make the error calculation more precise
  bin_1 = sum(binaire)
  bin_0 = dim(binaire)[1] * dim(binaire)[2] - bin_1
  if (bin_0 < bin_1)
    binaire = 1 - binaire
  
  # reference image
  reference <- rdfReadGreyImage ("rdf-masque-ronds.png")
  # results
  error = sum(binaire != reference) / (dim(reference)[1] * dim(reference)[2])
  error = round (error, 4) * 100
  info <- sprintf("%s\nthreshold=%s\nerror=%s%%", nom, threshold, error)
  if (interactive()){
    display (binaire, "image binaire", method="raster", all=TRUE)
    legend(x=0.5, y=dim(binaire)[2]/2, info, bg = "lightgreen", box.col = "lightgreen", yjust=0.5)
  }
}

buildBinaryTextureImage("rdf-2-classes-texture-0.png", 4, 0.5)
