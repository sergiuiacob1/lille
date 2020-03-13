# plots are saved in the directory below; create it if it doesn't exist
dir.create(file.path(".", "report/images"), showWarnings = FALSE)

# 1 - Visualising the data
# 1.1

# load data
load(file = 'x_app.data')
load(file = 'classe_app.data')
load(file = 'x_test.data')
load(file = 'classe_test.data')

noClasses = length(table(classe_app))
possibleColors = c('red', 'green', 'blue')

# visualise train data
trainColors = rep('red', length(x_app))
for (i in 1:noClasses)
  trainColors[classe_app == i] = possibleColors[i] 

# plot the data
plot (x_app, col = trainColors)
# visualise means
trainMeans = array(dim = c(noClasses, dim(x_app)[2]))
for (i in 1:noClasses){
  trainMeans[i, 1] = mean(x_app[classe_app == i, 1])
  trainMeans[i, 2] = mean(x_app[classe_app == i, 2])
}
points (trainMeans, col = possibleColors, p = 19, cex = 2)
# save plot
dev.copy(png, "./report/images/train_data.png")
dev.off()

# visualise test data
testColors = rep('red', length(x_test))
for (i in 1:noClasses)
  testColors[classe_test == i] = possibleColors[i] 

# plot the data
plot (x_test, col = testColors)
# visualise means
testMeans = array(dim = c(noClasses, dim(x_test)[2]))
for (i in 1:noClasses){
  testMeans[i, 1] = mean(x_test[classe_test == i, 1])
  testMeans[i, 2] = mean(x_test[classe_test == i, 2])
}
points (testMeans, col = possibleColors, p = 19, cex = 2)
# save plot
dev.copy(png, "./report/images/test_data.png")
dev.off()

# ===== Visualising the data =====

# 2 - PCA
# 2.1 PCA applied on training data

# calculate covariance
covariance <- cov(x_app)
# get the eigen vectors and values
Vp <- eigen(covariance)
pente <- Vp$vectors[2, 1] / Vp$vectors[1, 1]
# visualise the best axis to project the data on
plot (x_app, col = trainColors)
abline(a = 0, b = pente, col = "orange")
dev.copy (png, "./report/images/projection_line.png")
dev.off()

# calculate the  projections
ScalarProduct_app_ACP <- x_app %*% (Vp$vectors[, 1]) / sqrt(sum(Vp$vectors[, 1] * Vp$vectors[, 1]))
appProjections <- array(dim = dim(x_app))
appProjections[, 1] <- ScalarProduct_app_ACP * Vp$vectors[1, 1]
appProjections[, 2] <- ScalarProduct_app_ACP * Vp$vectors[2, 1]
# visualise the projections
points(appProjections, col = trainColors)
dev.copy (png, "./report/images/train_projections.png")
dev.off()


# 2.2 - Classifying projected data using ALD
library("MASS")
library("lattice")

# first calculate the projections for test data
ScalarProduct_test_ACP <- x_test %*% (Vp$vectors[, 1]) / sqrt(sum(Vp$vectors[, 1] * Vp$vectors[, 1]))
testProjections <- array(dim = dim(x_app))
testProjections[, 1] <- ScalarProduct_app_ACP * Vp$vectors[1, 1]
testProjections[, 2] <- ScalarProduct_app_ACP * Vp$vectors[2, 1]
# visualize the result
plot (x_test, col = testColors)
abline(a = 0, b = pente, col = "orange")
points (testProjections, col = testColors)
# save plot
dev.copy (png, "./report/images/test_projections.png")
dev.off()

# train the LDA
x_app_ACP.lda <- lda(ScalarProduct_app_ACP, classe_app)
assigne_test <- predict(x_app_ACP.lda,  newdata = ScalarProduct_test_ACP)
# Estimation des taux de bonnes classifications
table_classification_test <- table(classe_test, assigne_test$class)
# table of correct class vs. classification
diag(prop.table(table_classification_test, 1))
# total percent correct
taux_bonne_classif_test <- sum(diag(prop.table(table_classification_test)))
# forme de la classe 1 LABEL ASSIGNATION
possibleShapes <- c (15, 16, 17)
shape <- rep(1, length(x_test))
for (i in 1:noClasses)
  shape[assigne_test$class == i] = possibleShapes[i]
# Affichage des projections apprentissage class´ees
plot(x_test, col = testColors, pch = shape, xlab = "X1", ylab = "X2")
abline(a = 0, b = pente, col = "orange")
points (testProjections, col = testColors)
# save plot
dev.copy (png, "./report/images/test_predictions.png")
dev.off()
