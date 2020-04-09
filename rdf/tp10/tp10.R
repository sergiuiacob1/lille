
# classification d'images g?ographiques - SIG


installer <- function ()
{
  install.packages("party")
  install.packages("raster");
  install.packages('rgdal');
  install.packages('rasterVis');
  install.packages("dismo");
  
}

data_library <- function()
{
  library(raster);
  library(sp);
  library(rgdal);
  library(rasterVis);
  library(dismo); 
  library(party);
  library(rpart);
  
}

data_preparation <- function()
{
  # IMAGE PROPERTIES
  data_library();
  setwd("~/work/lille/rdf/tp10")

  # ultra-blue
  b1 <- raster('data/rs/LC08_044034_20170614_B1.tif');
  # Blue
  b2 <- raster('data/rs/LC08_044034_20170614_B2.tif');
  # Green
  b3 <- raster('data/rs/LC08_044034_20170614_B3.tif');
  # Red
  b4 <- raster('data/rs/LC08_044034_20170614_B4.tif');
  # Near Infrared (NIR)
  b5 <- raster('data/rs/LC08_044034_20170614_B5.tif');
  # SWIR 1
  b6 <- raster('data/rs/LC08_044034_20170614_B6.tif');
  #SMIR 2
  b7 <- raster('data/rs/LC08_044034_20170614_B7.tif');
  #SWIR3
  b8 <- raster('data/rs/LC08_044034_20170614_B8.tif');
  
  # IMAGE INFORMATION AND STATISTICS
  # coordinate reference system (CRS)
  crs(b2)
  
  ncell(b2)
  
  dim(b2)
  
  res(b2)
  
  nlayers(b2)
  
  # Do the bands have the same extent, number of rows and columns, projection, resolution, and origin
  compareRaster(b2,b3)
  
  
  # IMAGE INFORMATION AND STATISTICS
  landsat <- stack(b8, b7, b6, b5, b4, b3, b2,b1)
  # Check the properties of the RasterStack
  landsat;
  
  par(mfrow = c(2,2))
  plot(b1, main = "Ultra-Blue", col = gray(0:100 / 100))
  plot(b2, main = "Blue", col = gray(0:100 / 100))
  plot(b3, main = "Green", col = gray(0:100 / 100))
  plot(b4, main = "Red", col = gray(0:100 / 100))
  plot(b5, main = "NIR", col = gray(0:100 / 100))
  plot(b6, main = "SWIR1", col = gray(0:100 / 100))
  plot(b7, main = "SWIR2", col = gray(0:100 / 100))
  # plot(b8, main = "SWIR3", col = gray(0:100 / 100))
  
  
  landsatRGB <- stack(b8, b7, b6, b5, b4, b3, b2, b1);
  plotRGB(landsatRGB, axes = TRUE, stretch = "lin", main = "Landsat True Color Composite")
  
  par(mfrow = c(1,2))
  plotRGB(landsatRGB, axes=TRUE, stretch="lin", main="Landsat True Color Composite")
  landsatFCC <- stack(b8, b7, b6, b5, b4, b3, b2, b1)
  plotRGB(landsatFCC, axes=TRUE, stretch="lin", main="Landsat False Color Composite")
  
  names(landsat) <- c('ultra-blue', 'blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2', 'SWIR3')
  names(landsat)
  
  
  x <- writeRaster(landsat, filename="data/rs/landsat.tif", overwrite=TRUE)
  
  # EXTRACT PIXEL VALUES
  
  # load the polygons with land use land cover information
  samp <- readRDS('data/rs/samples.rds')
  # generate 300 point samples from the polygons
  ptsamp <- spsample(samp, 300, type="regular")
  # add the land cover class to the points
  ptsamp$class <- over(ptsamp, samp)$class
  # extract values with points
  df <- extract(landsat, ptsamp)
  # To see some of the reflectance values
  head(df)
  
  
  ## SPECTRAL PROFILES
  
  ms <- aggregate(df, list(ptsamp$class), mean)
  # instead of the first column, we use row names
  rownames(ms) <- ms[,1]
  ms <- ms[,-1]
  ms
  
  
  # Create a vector of color for the land cover classes for use in plotting
  mycolor <- c('darkred', 'yellow', 'burlywood', 'cyan', 'blue', 'red','azure', 'burlywood')
  #transform ms from a data.frame to a matrix
  ms <- as.matrix(ms)
  # First create an empty plot
  plot(0, ylim=c(0,0.6), xlim = c(1,8), type='n', xlab="Bands", ylab = "Reflectance")
  # add the different classes
  for (i in 1:nrow(ms)){
    lines(ms[i,], type = "l", lwd = 3, lty = 1, col = mycolor[i])
  }
  # Title
  title(main="Spectral Profile from Landsat", font.main = 2)
  # Legend
  legend("topleft", rownames(ms), cex=0.8, col=mycolor, lty = 1, lwd =3, bty = "n")
  
}

data_reference <- function (nom_partiel, chemin)
{
  setwd(chemin);
  nom_complet <-paste('data/rs/',nom_partiel,'.tif',sep="");
  nlcd <- brick(nom_complet);
  
  #fichiers: nlcd-L1.tif, landsat.tif, centralvalley-2001LE7.tif
  
  plot(nlcd);
  #names(nlcd) <- c("landsat.1", "landsat.2", "landsat.3", "landsat.4", "landsat.5", "landsat.6", "landsat.7", "landsat.8");
  names(nlcd) <- c("nlcd2001", "nlcd2011");
  
  
  # The class names and colors for plotting
  nlcdclass <- c("Water", "Developed", "Barren", "Forest", "Shrubland", "Herbaceous", "Planted/Cultivated", "Wetlands")
  classdf <- data.frame(classvalue1 = c(1,2,3,4,5,7,8,9), classnames1 = nlcdclass)
  
  # Hex codes of colors
  classcolor <- c("#5475A8", "#B50000", "#D2CDC0", "#38814E", "#AF963C", "#D1D182", "#FBF65D", "#C8E6F8")
  
  # Now we ratify (RAT = "Raster Attribute Table") the ncld2011 (define RasterLayer as a categorical variable). This is helpful for plotting.
  nlcd2011 <- nlcd[[2]]
  nlcd2011 <- ratify(nlcd2011)
  rat <- levels(nlcd2011)[[1]]
  #
  rat$landcover <- nlcdclass
  levels(nlcd2011) <- rat
  
  # Generate Sample site
  
  # Load the training sites locations
  # Set the random number generator to reproduce the results
  set.seed(99)
  # Sampling
  samp2011 <- sampleStratified(nlcd2011, size = 200, na.rm = TRUE, sp = TRUE)
  samp2011
  
  table(samp2011$nlcd2011)
  
  plt <- levelplot(nlcd2011, col.regions = classcolor, main = 'Distribution of Training Sites')
  print(plt + layer(sp.points(samp2011, pch = 3, cex = 0.5, col = 1)))
  
  list(samp2011, classdf, classcolor, nlcdclass);
}

data_extract_values <- function (chemin, nom_partiel,samp2011)
{
  setwd(chemin);
  nom_complet <-paste('data/rs/',nom_partiel,'.tif',sep="");
  
  
  landsat5 <- stack(nom_complet)
  names(landsat5) <- c('blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2');
  
  # Extract the layer values for the locations
  sampvals <- extract(landsat5, samp2011, df = TRUE)
  # sampvals no longer has the spatial information. To keep the spatial information you use `sp=TRUE` argument in the `extract` function.
  # drop the ID column
  sampvals <- sampvals[, -1]
  # combine the class information with extracted values
  sampdata <- data.frame(classvalue = samp2011@data$nlcd2011, sampvals)
  list(sampdata, landsat5);
}

data_train <- function (sampdata)
{
  cart <- rpart(as.factor(classvalue)~., data=sampdata, method = 'class', minsplit = 5);  
  cart
}

data_ratif <- function (pr2011, classde)
{
  
  pr2011 <- ratify(pr2011)
  rat <- levels(pr2011)[[1]]
  rat$legend <- classdf$classnames
  levels(pr2011) <- rat
  levelplot(pr2011, maxpixels = 1e6,
            col.regions = classcolor,
            scales=list(draw=FALSE),
            main = "Decision Tree classification of Landsat 5")
}

model_evaluation <- function (sampdata)
{
  
  set.seed(99)
  j <- kfold(sampdata, k = 5, by=sampdata$classvalue)
  table(j)
  ## j
  ##   1   2   3   4   5
  ## 320 320 320 320 320
  x <- list()
  for (k in 1:5) {
    train <- sampdata[j!= k, ]
    test <- sampdata[j == k, ]
    cart <- rpart(as.factor(classvalue)~., data=train, method = 'class', minsplit = 5)
    pclass <- predict(cart, test, type='class')
    # create a data.frame using the reference and prediction
    x[[k]] <- cbind(test$classvalue, as.integer(pclass))
    
    
    #Confusion Matrix
    
    y <- do.call(rbind, x)
    y <- data.frame(y)
    colnames(y) <- c('observed', 'predicted')
    conmat <- table(y)
    # change the name of the classes
    colnames(conmat) <- classdf$classnames
    rownames(conmat) <- classdf$classnames
    conmat
    ##                     predicted
    ## observed             Water Developed Barren Forest Shrubland Herbaceous
    ##   Water                175         6      0      3         0          0
    ##   Developed              2        90     51      8        10         22
    ##   Barren                 7        39     82      4        19         38
    ##   Forest                 0         2      1    106        57          1
    ##   Shrubland              0         3      5     59       102         12
    ##   Herbaceous             0         9     36     10        27        109
    ##   Planted/Cultivated     0         7     11     34        42         19
    ##   Wetlands              18        10      6     36        29          5
    ##                     predicted
    ## observed             Planted/Cultivated Wetlands
    ##   Water                               7        9
    ##   Developed                          11        6
    ##   Barren                              5        6
    ##   Forest                              6       27
    ##   Shrubland                          12        7
    ##   Herbaceous                          8        1
    ##   Planted/Cultivated                 69       18
    ##   Wetlands                           33       63
  }
  
  # OVERALL ACCURACY
  # number of cases
  n <- sum(conmat)
  n
  ## [1] 1600
  # number of correctly classified cases per class
  diag <- diag(conmat)
  # Overall Accuracy
  OA <- sum(diag) / n
  OA
  ## [1] 0.4975
  
  # KAPPA
  # observed (true) cases per class
  rowsums <- apply(conmat, 1, sum)
  p <- rowsums / n
  # predicted cases per class
  colsums <- apply(conmat, 2, sum)
  q <- colsums / n
  expAccuracy <- sum(p*q)
  kappa <- (OA - expAccuracy) / (1 - expAccuracy)
  kappa
  ## [1] 0.4257143
  
  # Producer accuracy
  PA <- diag / colsums
  # User accuracy
  UA <- diag / rowsums
  outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)
  outAcc
  ##                    producerAccuracy userAccuracy
  ## Water                     0.8663366        0.875
  ## Developed                 0.5421687        0.450
  ## Barren                    0.4270833        0.410
  ## Forest                    0.4076923        0.530
  ## Shrubland                 0.3566434        0.510
  ## Herbaceous                0.5291262        0.545
  ## Planted/Cultivated        0.4569536        0.345
  ## Wetlands                  0.4598540        0.315
  
}

data_classif <- function (nom_partiel)
{
  nom_partiel = "nlcd-L1";
  data_library();
  
  # REFERENCE DATA
  
  dr<- data_reference(nom_partiel, "C:/Users/djeraba/Desktop/Enseignements-2019-2020/Master-IVI/Ma Version/creneau 3/");
  samp2011 <- dr[[1]];classdf <- dr[[2]]; classcolor <- dr[[3]];nlcdclass <- dr[[4]];
  
  # EXTRACT VALUES FOR SITE
  evd <- data_extract_values("C:/Users/djeraba/Desktop/Enseignements-2019-2020/Master-IVI/Ma Version/creneau 3/",'centralvalley-2011LT5',samp2011);
  sampdata <- evd[[1]];
  landsat5 <- evd[[2]]; 
  # TRAINING
  cart <- data_train (sampdata);
  # Train the model
  
  
  # Plot the trained classification tree
  plot(cart, uniform=TRUE, main="Classification Tree")
  text(cart, cex = 0.8)
  
  # CLASSIFY
  # Now predict the subset data based on the model; prediction for entire area takes longer time
  
  pr2011 <- predict(landsat5, cart, type='class')
  pr2011
  
  data_ratif (pr2011, classdf);
  
  # MODEL EVALUATION
  
  model_evaluation(sampdata);
  
}




