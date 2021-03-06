# -----------------------------------------------------------------------
# Extraction d'attributs de contours,
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
source ("rdfContours.R")
source ("./../tp01/rdfMoments.R")

# Chargement d'un contour
nom <- "rdf-cercle-80.txt"
cont <- rdfChargeFichierContour (nom)
print(cont)

# Afficher le contour
# plot (cont, main = nom, type = "o", asp = 1, col = "red",
#      ylim = rev (range (Im (cont))))
# 
# cont4 <- cont[seq(1, length(cont), 4)]
# points (cont4, main = nom, type = "o", asp = 1, col = "blue",
#      ylim = rev (range (Im (cont))))
# 
# cont8 <- cont[seq(1, length(cont), 8)]
# points (cont8, main = nom, type = "o", asp = 1, col = "green",
#      ylim = rev (range (Im (cont))))
# 

reconstructShapeWithFourier <- function(nom){
  cont <- rdfChargeFichierContour(nom)
  fourier <- fft(cont)/length(cont)
  print (fourier)
  inversed <- fft(fourier, inverse=TRUE)
  print ("Can we reconstruct our shape from Fourier descriptors?")
  print (all.equal(inversed, cont))
}
reconstructShapeWithFourier("rdf-carre-80.txt")


analyseFirstFourierDescriptor <- function(nom){
  cont <- rdfChargeFichierContour(nom)
  Z0 <- fourier[1]
  plot (cont, main = nom, type = "o", asp = 1, col = "red",
        ylim = rev (range (Im (cont))))
  points(Z0, col="red")
  # Z0 is the center of the form
  fourier[1] <- fourier[1] + 0.5 + 3i
  inversed <- fft(fourier, inverse=TRUE)
  points (inversed, main = nom, type = "o", asp = 1, col = "blue",
        ylim = rev (range (Im (inversed))))
  points(fourier[1], col="blue")
  # If we add another complex number to Z0, we change the form' center
  # So we make a transition
}
analyseFirstFourierDescriptor("rdf-carre-80.txt")


simplifyShape <- function(nom, ratio){
  cont <- rdfChargeFichierContour(nom)
  fourier <- fft(cont)/length(cont)
  fourier <- rdfAnnuleDescFourier(fourier, ratio)
  inversed <- fft(fourier, inverse=TRUE)
  plot (cont, main = nom, type = "o", asp = 1, col = "red",
          ylim = rev (range (Im (cont))))
  points (inversed, main = nom, type = "o", asp = 1, col = "blue",
          ylim = rev (range (Im (inversed))))
}
simplifyShape("rdf-carre-80.txt", 0.075)

findShapeContour <- function (nom, dmax){
  cont <- rdfChargeFichierContour(nom)
  cont <- rdfAlgorithmeCorde (cont, dmax)
  plot (cont, main = nom, type = "o", asp = 1, col = "red",
        ylim = rev (range (Im (cont))))
}
findShapeContour("rdf-cercle-80.txt", 1)

# cont <- rdfChargeFichierContour(nom)
# cont <- rdfAlgorithmeCorde (cont, 0)
# plot (cont, main = nom, type = "o", asp = 1, col = "red",
#       ylim = rev (range (Im (cont))))
# cont2 <- rdfAlgorithmeCorde (cont, 0.5)
# lines (cont2, main = nom, type = "o", asp = 1, col = "blue",
#       ylim = rev (range (Im (cont))))
# cont3 <- rdfAlgorithmeCorde (cont, 1)
# lines (cont3, main = nom, type = "o", asp = 1, col = "green",
#         ylim = rev (range (Im (cont))))
# cont4 <- rdfAlgorithmeCorde (cont, 3)
# lines (cont4, main = nom, type = "o", asp = 1, col = "yellow",
#         ylim = rev (range (Im (cont))))
# legend(x="topleft", y=0.92, legend=c("0", "0.5", "1", "3"),
#        col=c("red", "blue", "green", "yellow"), lty=1:2, cex=0.8, title="dmax")

compareMethods <- function(nom, ratio, dmax){
  img <- rdfReadGreyImage(nom)
  cont <- rdfContour(img)
  plot (cont, main = nom, type = "o", asp = 1, col = "red",
        ylim = rev (range (Im (cont))))
  
  fourier <- fft(cont)/length(cont)
  fourier <- rdfAnnuleDescFourier(fourier, ratio)
  inversed <- fft(fourier, inverse=TRUE)
  points (inversed, main = nom, type = "o", asp = 1, col = "blue",
        ylim = rev (range (Im (cont))))
  
  chord <- rdfAlgorithmeCorde (cont, dmax)
  points (chord, main = nom, type = "o", asp = 1, col = "green",
          ylim = rev (range (Im (cont))))
  legend(x="topleft", y=0.92, legend=c("original", sprintf("ratio=%s", ratio), sprintf("dmax=%s", dmax)),
         col=c("red", "blue", "green"), lty=1:2, cex=0.8, title="parameters")
  legend(x="bottomleft", y=0.92, legend=c(length(cont), length(fourier), length(chord)),
         col=c("red", "blue", "green"), lty=1:2, cex=0.8, title="no. points")
}

compareMethods("rdf-rectangle-horizontal.png", 0.8, 1)


ratio <- 0.075
dmax <- 0.1
cont <- rdfChargeFichierContour("rdf-cercle-80.txt")
plot (cont, main = nom, type = "o", asp = 1, col = "red",
      ylim = rev (range (Im (cont))))

fourier <- fft(cont)/length(cont)
fourier <- rdfAnnuleDescFourier(fourier, ratio)
inversed <- fft(fourier, inverse=TRUE)
points (inversed, main = nom, type = "o", asp = 1, col = "blue",
        ylim = rev (range (Im (cont))))

chord <- rdfAlgorithmeCorde (cont, dmax)
points (chord, main = nom, type = "o", asp = 1, col = "green",
        ylim = rev (range (Im (cont))))
legend(x="topleft", y=0.92, legend=c("original", sprintf("ratio=%s", ratio), sprintf("dmax=%s", dmax)),
       col=c("red", "blue", "green"), lty=1:2, cex=0.8, title="parameters")
legend(x="bottomleft", y=0.92, legend=c(length(cont), length(fourier), length(chord)),
       col=c("red", "blue", "green"), lty=1:2, cex=0.8, title="no. points")
