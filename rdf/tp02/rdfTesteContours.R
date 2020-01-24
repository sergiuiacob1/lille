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

# Chargement d'un contour
nom <- "rdf-cercle-80.txt"
cont <- rdfChargeFichierContour (nom)
print(cont)

# Afficher le contour
# plot (cont, main = nom, type = "o", asp = 1, col = "red",
#       ylim = rev (range (Im (cont))))

# cont4 <- cont[seq(1, length(cont), 4)]
# points (cont4, main = nom, type = "o", asp = 1, col = "blue",
#       ylim = rev (range (Im (cont))))

# cont8 <- cont[seq(1, length(cont), 8)]
# points (cont8, main = nom, type = "o", asp = 1, col = "green",
#       ylim = rev (range (Im (cont))))

nom <- "rdf-carre-80.txt"
cont <- rdfChargeFichierContour(nom)
fourier <- fft(cont)/length(cont)
inversed <- fft(fourier, inverse=TRUE)
print (all.equal(inversed, cont))
# So, FFT allows us to reconstruct a form

Z0 <- fourier[1]
plot (cont, main = nom, type = "o", asp = 1, col = "red",
      ylim = rev (range (Im (cont))))
points(Z0, col="red")
# Z0 is the center of the form
fourier[1] <- fourier[1] + 2 + 3i
inversed <- fft(fourier, inverse=TRUE)
points (inversed, main = nom, type = "o", asp = 1, col = "blue",
      ylim = rev (range (Im (inversed))))
points(fourier[1], col="blue")
# If we add another complex number to Z0, we change the form' center
# So we make a transition


nom <- "rdf-carre-80.txt"
cont <- rdfChargeFichierContour(nom)
fourier <- fft(cont)/length(cont)
fourier <- rdfAnnuleDescFourier(fourier, 0.8)
inversed <- fft(fourier, inverse=TRUE)
plot (cont, main = nom, type = "o", asp = 1, col = "red",
        ylim = rev (range (Im (cont))))
points (inversed, main = nom, type = "o", asp = 1, col = "blue",
        ylim = rev (range (Im (inversed))))
