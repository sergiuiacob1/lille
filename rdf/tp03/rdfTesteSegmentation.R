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

# for (i in 0:4){
#   image <- rdfReadGreyImage (sprintf("rdf-2-classes-texture-%d.png", i))
#   image <- rdfTextureEcartType(image, 2)
#   jpeg(sprintf("report/images/blur%d.png", i))
#   display (image, "image binaire", method="raster", all=TRUE)
#   dev.off()
# }

buildTextureHistograms <- function (nom, nbins, taille){
  image <- rdfReadGreyImage (nom)
  image <- rdfTextureEcartType(image, taille)
  print (length(unique(imageData(image))))
  h <- hist (as.vector (image), breaks = seq (0, 1, 1 / nbins), main = nom)
}

buildTextureHistograms("rdf-2-classes-texture-0.png", 256, 2)

for (i in 0:4){
  jpeg(sprintf("report/images/texture_levels_%d.png", i))
  buildTextureHistograms(sprintf("rdf-2-classes-texture-%d.png", i), 2)
  dev.off()
}


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

buildBinaryTextureImage("rdf-2-classes-texture-4.png", 2, 0.3)

thresholds <- c (0.55, 0.3, 0.42, 0.3, 0.3)
for (i in 0:4){
  jpeg(sprintf("report/images/texture_thresholding_%d.png", i))
  buildBinaryTextureImage(sprintf("rdf-2-classes-texture-%d.png", i), 2, thresholds[i + 1])
  dev.off()
}

buildConjointHistogram <- function(nom, vala, valb){
  grayImg <- rdfReadGreyImage(nom)
  textureImg <- rdfTextureEcartType(grayImg, 2)
  res = rdfCalculeHistogramme2D(grayImg, 256, textureImg, 256)
  display (res, "combination", method="raster", all=TRUE)
  abline(a = vala, b = valb, col = 6)
  legend(x=0.5, y=dim(res)[2]/2, sprintf("a=%f\nb=%f", vala, valb), bg = "lightgreen", box.col = "lightgreen", yjust=0.5)
  
  res
}

# for (i in 0:4){
#   jpeg(sprintf("report/images/conjoint_%d.png", i))
#   buildConjointHistogram(sprintf("rdf-2-classes-texture-%d.png", i), 3500, 50)
#   dev.off()
# }




buildFinalGrayHistogram <- function (nom, a, b){
  grayImg <- rdfReadGreyImage(nom)
  textureImg <- rdfTextureEcartType(grayImg, 2)
  final <- a * grayImg + b * textureImg
  # normalize
  final <- final / max(final)
  h <- hist (as.vector (final), breaks = seq (0, 1, 1 / 256), main = nom)
  final
}

buildFinalSegmentation <- function (nom, image, threshold){
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


a <- 120
b <- -0.4
nom <- "rdf-2-classes-texture-2.png"
hist <- buildConjointHistogram(nom, a, b)
img <- buildFinalGrayHistogram(nom, a, b)
buildFinalSegmentation(nom, img, 0.6)
