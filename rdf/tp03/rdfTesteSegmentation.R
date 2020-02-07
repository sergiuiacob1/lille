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

buildBinaryImage <- function(nom, threshold){
  image <- rdfReadGreyImage (nom)
  binaire <- (image - threshold) >= 0
  # reference image
  reference <- rdfReadGreyImage ("rdf-masque-ronds.png")
  # "normalizing" the way the pixels are classified in the images
  binaire <- binaire != 0
  reference <- reference != 1
  # error
  error <- sum(binaire != reference)/(dim(binaire)[1] * dim(binaire)[2])
  error <- round (error, 5) * 100
  # results
  info <- sprintf("%s\nthreshold=%s\nerror=%s%%", nom, threshold, error)
  if (interactive()){
    display (binaire, "image binaire", method="raster", all=TRUE)
    # legend(x=0, y=50, info, box.col = "lightblue", bg = "lightblue",
    #        title.adj=c(0,0))
    legend(0, dim(binaire)[2]/2, "Some text", box.col = "lightblue", bg = "lightblue", adj=c(0.5, 0),
           xjust=1)
  }
}

buildBinaryImage("rdf-2-classes-texture-2.png", 0.41)
