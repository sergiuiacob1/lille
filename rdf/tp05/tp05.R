library ("EBImage")
source ("rdfSegmentation.R")

nom <- "2classes_100_100_8bits_2016.png"
image <- rdfReadGreyImage (nom)

binaire50 <- (image - 0.5) >= 0
display (binaire50, "image binaire 0.35", method="raster", all=TRUE)
binaire55 <- (image - 0.55) >= 0
display (binaire55, "image binaire 0.35", method="raster", all=TRUE)
binaire60 <- (image - 0.6) >= 0
display (binaire60, "image binaire 0.35", method="raster", all=TRUE)


calculateClassAPrioriProbabilities <- function(){
  combined <- rdfReadGreyImage ("2classes_100_100_8bits_2016.png")
  above <- rdfReadGreyImage ("2classes_100_100_8bits_omega1_2016.png")
  below <- rdfReadGreyImage ("2classes_100_100_8bits_omega2_2016.png")
  
  # nbins <- 256
  # hCombined <- hist (as.vector (combined), freq=FALSE, breaks = seq (0, 1, 1 / nbins), col=rgb(1,0,0,0.5),  main="combined histogram", xlab="Gray values")
  # h1 <- hist (as.vector (above), freq=FALSE, breaks = seq (0, 1, 1 / nbins), col=rgb(1,0,0,0.5),  main="omega1&omega2 histograms", xlab="Gray values")
  # h2 <- hist (as.vector (below), freq=FALSE, breaks = seq (0, 1, 1 / nbins), col=rgb(0,0,1,0.5), add=T)
  
  noTotalPixels = dim(combined)[1] * dim(combined)[2]
  pw1 = (dim(above)[1] * dim(above)[2]) / noTotalPixels
  pw2 = (dim(below)[1] * dim(below)[2]) / noTotalPixels
  c(pw1, pw2)
}

probs <- calculateClassAPrioriProbabilities()
print (probs)

calculateConditionalProbability <- function (grayLevel){
  
}
