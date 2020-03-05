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

nbins <- 256
h1 <- hist (as.vector (above), freq=FALSE, breaks = seq (0, 1, 1 / nbins), col=rgb(1,0,0,1), main="omega1&omega2 histograms", xlab="Gray values")
h2 <- hist (as.vector (below), freq=FALSE, breaks = seq (0, 1, 1 / nbins), col=rgb(0,0,1,1), add=T)

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
  
  # build our reference segmentation
  perfect <- matrix (nrow=dim(image)[1], ncol=dim(image)[2])
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
  
  # precalculate all probabilities
  condProbs <- matrix(nrow = 256, ncol = 2)
  for (X in 0:255){
    res <- calculateConditionalProbability(X, probs)
    condProbs[X + 1, 1] <- res[2]
    condProbs[X + 1, 2] <- res[3]
  }
  
  min_error = 2 * dim(image)[1] * dim(image)[2]
  for (X in 0:255){
    binary <- (image - X/255) >= 0
    # only get the data, otherwise the comparisons will be really slow
    binary <- imageData(binary)
    
    error <- 0
    # these pixels should be in w1 class
    for (i in 1:dim(image)[1])
      for (j in 1:dim(image)[2])
        if (perfect [i, j] != binary [i, j]){
          # if it should be in the first class
          if (j < dim(above)[2])
            error <- error + condProbs[image[i, j] * 255 + 1, 1] * probs[1]
          else
            error <- error + condProbs[image[i, j] * 255 + 1, 2] * probs[2]
        }
      
    if (error < min_error){
      min_error <- error
      best_threshold <- X
    }
  }
  print (c(min_error, best_threshold))
  
  # segment using best_threshold
  binary <- (image - best_threshold/255) >= 0
  display(binary, method="raster", all=TRUE)
}

automaticSegmentationUsingBayes()


classify0Digit <- function(){
  # calculate a priori probabilities
  # P(w) = probability of a pixel to be in the w class
  # the pixels that are in w class have a gray value of 0 in the image corresponding to w
  probs <- c(h1$counts[1] / sum(h$counts), h2$counts[1] / sum(h$counts))
  
  # only get the imageData, it will be faster for comparisons
  image <- imageData(image)
  img_w1 <- imageData(img_w1)
  img_w2 <- imageData(img_w2)
  
  # build our reference segmentation
  perfect <- matrix (nrow=dim(image)[1], ncol=dim(image)[2])
  # dim(image)[1] = number of columns
  for (i in 1:dim(image)[1])
    for (j in 1:dim(image)[2]){
      if (img_w1[i, j] == 0){
        perfect[i, j] <- TRUE
      }
      else{
        perfect[i, j] <- FALSE
      }
    }
  display(perfect, method="raster", all=TRUE)
  
  # precalculate all conditional probabilities
  condProbs <- matrix(nrow = 256, ncol = 2)
  for (X in 0:255){
    px <- h$counts[X + 1] / sum(h$counts)
    # P(X | w) = probability of a pixel having the gray value of X if the pixel is from class w
    # Using bayes theorem, we get:
    # P(X|w) = (P(w | X) * P(X)) / P(w)
    # P(w | X) = the prob. of the pixel being from the w class, if its value is X
    # 
    
    # h1$counts[X + 1] + h2$counts[X + 1] = h$counts[X + 1]
    # if h1$counts[X + 1] - h$counts[X + 1] is different than 0, then those pixels were NOT assigned to w1
    
    if (px == 0){
      condProbs[X + 1, 1] <- 0
      condProbs[X + 1, 2] <- 0
      next
    }
    p_w1_if_x <- (h$counts[X + 1] - h1$counts[X + 1]) / h$counts[X + 1]
    p_x_and_w1 <- p_w1_if_x * px
    p_x_if_w1 <- p_x_and_w1 / probs[1]
    p_w2_if_x <- (h$counts[X + 1] - h2$counts[X + 1]) / h$counts[X + 1]
    p_x_and_w2 <- p_w2_if_x * px
    p_x_if_w2 <- p_x_and_w2 / probs[2]
    
    condProbs[X + 1, 1] <- p_x_if_w1
    condProbs[X + 1, 2] <- p_x_if_w2
  }
  
  probSum <- 0
  for (X in 0:255){
    if(is.nan(condProbs[X + 1, 1]) == FALSE)
      probSum <- probSum + probs[1] * condProbs[X + 1, 1] + probs[2] * condProbs[X + 1, 2]
  }
  
  print ('Making sure that the sum of conditional probabilities is 1:')
  print (probSum)

  # search for the best threshold
  min_error <- 2 * dim(image)[1] * dim(image)[2]
  best_threshold <- 0
  for (X in 0:255){
    binary <- (image - X/255) >= 0
    # only get the data, otherwise the comparisons will be really slow
    binary <- imageData(binary)
    
    error <- 0
    for (i in 1:dim(image)[1])
      for (j in 1:dim(image)[2])
        if (perfect [i, j] != binary [i, j]){
          # if it should have been in w1
          if (img_w1[i, j] == 0)
            error <- error + condProbs[image[i, j] * 255 + 1, 1] * probs[1]
          else
            # it should have been in w1
            error <- error + condProbs[image[i, j] * 255 + 1, 2] * probs[2]
        }
    
    if (error < min_error){
      min_error <- error
      best_threshold <- X
    }
  }
  print (c (min_error, best_threshold))
  # segment using best_threshold
  binary <- (image - best_threshold / 255) >= 0
  display(binary, method="raster", all=TRUE)
  best_threshold
}

# get img, w1 and w2 histograms
image <- rdfReadGreyImage("rdf-chiffre-0-8bits.png")
img_w1 <- rdfReadGreyImage("rdf-chiffre-0-8bits_omega1.png")
img_w2 <- rdfReadGreyImage("rdf-chiffre-0-8bits_omega2.png")
nbins <- 256
h <- hist (as.vector (image), freq=FALSE, breaks = seq (0, 1, 1 / nbins))
h1 <- hist (as.vector (img_w1), freq=FALSE, breaks = seq (0, 1, 1 / nbins))
h2 <- hist (as.vector (img_w2), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

best_threshold <- classify0Digit()

image <- rdfReadGreyImage("rdf-chiffre-1-8bits.png")
binary <- (image - best_threshold/255) < 0
display(binary, method="raster", all=TRUE)


# Calculate error rate
image <- rdfReadGreyImage("rdf-chiffre-0-8bits.png")
best_threshold <- classify0Digit()
binary <- (image - best_threshold / 255) >= 0
# build our reference segmentation
perfect <- matrix (nrow=dim(image)[1], ncol=dim(image)[2])
# dim(image)[1] = number of columns
for (i in 1:dim(image)[1])
  for (j in 1:dim(image)[2]){
    if (img_w1[i, j] == 0){
      perfect[i, j] <- TRUE
    }
    else{
      perfect[i, j] <- FALSE
    }
  }
sprintf("Error for digit 0: %f", sum(abs(perfect - binary)) / (dim(perfect)[1] * dim(perfect)[2]))

# calculate error rate for 1
image <- rdfReadGreyImage("rdf-chiffre-1-8bits.png")
binary <- (image - best_threshold / 255) < 0
perfect <- rdfReadGreyImage("rdf-chiffre-1-8bits_classe_a_trouver.png")
display(binary, method="raster", all=TRUE)
sprintf("Error for digit 1: %f", sum(abs(perfect - binary)) / (dim(perfect)[1] * dim(perfect)[2]))






