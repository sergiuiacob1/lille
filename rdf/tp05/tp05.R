library ("EBImage")
source ("rdfSegmentation.R")

# loading some data only once
image <- rdfReadGreyImage ("2classes_100_100_8bits_2016.png")
above <- rdfReadGreyImage ("2classes_100_100_8bits_omega1_2016.png")
below <- rdfReadGreyImage ("2classes_100_100_8bits_omega2_2016.png")

nbins <- 256
h <- hist (as.vector (image), freq=FALSE, breaks = seq (0, 1, 1 / nbins))
h1 <- hist (as.vector (above), freq=FALSE, breaks = seq (0, 1, 1 / nbins))
h2 <- hist (as.vector (below), freq=FALSE, breaks = seq (0, 1, 1 / nbins))


# Fixed Threshold

# take a look at the histogram
plot(h)

# for each threshold, calculate the binary image
# image has values between [0, 1] so our threshold has to be in the same range
# for each pixel, the expression (pixel_value - threshold) >= 0 will assign it to one of two classes: w1 or w2
binaire50 <- (image - 0.5) >= 0
display (binaire50, "image binaire 0.35", method="raster", all=TRUE)
binaire55 <- (image - 0.55) >= 0
display (binaire55, "image binaire 0.35", method="raster", all=TRUE)
binaire60 <- (image - 0.6) >= 0
display (binaire60, "image binaire 0.35", method="raster", all=TRUE)


calculateClassAPrioriProbabilities <- function(){
  # pw1 = (number_of_pixels_from_the_first_image) / (number_of_total_pixels)
  # pw1 = (number_of_pixels_from_the_second_image) / (number_of_total_pixels)
  noTotalPixels = dim(image)[1] * dim(image)[2]
  pw1 = (dim(above)[1] * dim(above)[2]) / noTotalPixels
  pw2 = (dim(below)[1] * dim(below)[2]) / noTotalPixels
  c(pw1, pw2)
}

# nbins <- 256
# hCombined <- hist (as.vector (combined), freq=FALSE, breaks = seq (0, 1, 1 / nbins), col=rgb(1,0,0,0.5),  main="combined histogram", xlab="Gray values")
# h1 <- hist (as.vector (above), freq=FALSE, breaks = seq (0, 1, 1 / nbins), col=rgb(1,0,0,0.5),  main="omega1&omega2 histograms", xlab="Gray values")
# h2 <- hist (as.vector (below), freq=FALSE, breaks = seq (0, 1, 1 / nbins), col=rgb(0,0,1,0.5), add=T)

probs <- calculateClassAPrioriProbabilities()
print (probs)

calculateConditionalProbability <- function (X, probs){
  # let's take a look at the histogram again
  # we can notice that h$counts[X + 1] gives us the number of pixels that have the gray value of X
  # the "+ 1" above is because in R arrays start from 1

  # so, P(X | I) = the probability of a pixel from the merged image to have the gray value of X
  px <- h$counts[X + 1] / sum(h$counts)
  # P(X | w) = the probability of a pixel from the merged image to have the gray value of X
  # IF that pixel is from the w class
  # Using bayes theorem, we get:
  # P(X|w) = (P(w | X) * P(X)) / P(w)
  # and P(w | X) = how many pixels of value X are in the image corresponding to w / the total number of pixels with a value of X
  
  p_w1_if_x <- h1$counts[X + 1] / h$counts[X + 1]
  p_x_and_w1 <- p_w1_if_x * px
  p_x_if_w1 <- p_x_and_w1 / probs[1]
  
  p_w2_if_x <- h2$counts[X + 1] / h$counts[X + 1]
  p_x_and_w2 <- p_w2_if_x * px
  p_x_if_w2 <- p_x_and_w2 / probs[2]
  
  c(px, p_x_if_w1, p_x_if_w2)
}

print (calculateConditionalProbability(141, probs))



# Automatic segmentation

automaticSegmentationUsingBayes <- function(){
  # calculate these probabilities only once
  probs <- calculateClassAPrioriProbabilities()
  # only get the imageData, it will be faster for comparisons
  image <- imageData(image)
  # calculate these dimensions only once
  dimImage <- dim(image)
  dimAbove <- dim(above)
  dimBelow <- dim(below)
  
  # build our reference segmentation
  perfect <- matrix (ncol=dim(image)[1], nrow=dim(image)[2])
  # dim(image)[1] = number of columns
  for (i in 1:dim(image)[1])
    for (j in 1:dim(above)[2]){
      perfect[i, j] <- FALSE
    }
  for (i in 1:dim(image)[1])
    for (j in dim(above)[2]:dim(image)[2]){
      perfect[i, j] <- TRUE
    }
  display(perfect, method="raster", all=TRUE)
  
  # search for the best threshold
  # max error is (hypotethically) when all probabilities are 1 and all pixels are missclassified
  min_error <- 2 * dimImage[1] * dimImage[2]
  best_threshold <- 0
  
  # precalculate all probabilities
  condProbs <- array(dim=c(dimImage[1], dimImage[2], 3))
  for (i in 1:dimImage[1])
    for (j in 1:dimImage[2]){
      res <- calculateConditionalProbability(image[i, j] * 255, probs)
      condProbs[i, j, 1] <- res[1]
      condProbs[i, j, 2] <- res[2]
      condProbs[i, j, 3] <- res[3]
    }
      
  for (X in 0:255){
    binary <- (image - X/255) >= 0
    # only get the data, otherwise the comparisons will be really slow
    binary <- imageData(binary)
    
    error <- 0
    for (i in 1:dimImage[1])
      for (j in 1:dimAbove[2])
        if (perfect [i, j] != binary [i, j])
          error <- error + condProbs[2] * probs[1]
    for (i in 1:dimImage[1])
      for (j in dimAbove[2]:dimImage[2])
        if (perfect [i, j] != binary [i, j])
          error <- error + condProbs[3] * probs[2]
      
    if (error < min_error){
      min_error <- error
      best_threshold <- X
    }
  }
  
  print (c (min_error, best_threshold))
  # segment using best_threshold
  binary <- (image - best_threshold/255) >= 0
  display(binary, method="raster", all=TRUE)
}

automaticSegmentationUsingBayes()







