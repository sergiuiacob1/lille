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
dev.copy (png, "./report/images/projection_line_acp.png")
dev.off()

# calculate the  projections
ScalarProduct_app_ACP <- x_app %*% (Vp$vectors[, 1]) / sqrt(sum(Vp$vectors[, 1] * Vp$vectors[, 1]))
appProjections <- array(dim = dim(x_app))
appProjections[, 1] <- ScalarProduct_app_ACP * Vp$vectors[1, 1]
appProjections[, 2] <- ScalarProduct_app_ACP * Vp$vectors[2, 1]
# visualise the projections
points(appProjections, col = trainColors)
dev.copy (png, "./report/images/train_projections_acp.png")
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
dev.copy (png, "./report/images/test_projections_acp.png")
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
possibleShapes <- c (0, 1, 2)
shape <- rep(1, length(x_test))
for (i in 1:noClasses)
  shape[assigne_test$class == i] = possibleShapes[i]
# Affichage des projections apprentissage classees
plot(x_test, col = testColors, pch = shape, xlab = "X1", ylab = "X2")
abline(a = 0, b = pente, col = "orange")
points (testProjections, col = testColors)
# save plot
dev.copy (png, "./report/images/test_predictions_lda_acp.png")
dev.off()

# ========== 3. Analyse Factorielle Discriminante ==========
# 3.1

# I already calculated the means above - trainMeans and testMeans - except the total mean
mean = colMeans(trainMeans)
# calculate covariances
S1 <- cov(x_app[classe_app==1,])
S2 <- cov(x_app[classe_app==2,])
S3 <- cov(x_app[classe_app==3,])

Sw = S1 + S2 + S3
# covariance inter-classe
Sb <- 0
for (i in 1:3)
  Sb <- Sb + (trainMeans[i,] - mean) %*% t(trainMeans[i,] - mean)

# 3.2

# résolution equation
invSw <- solve(Sw)
invSw_by_Sb <- invSw %*% Sb
Vp <- eigen(invSw_by_Sb)
# show the best axis
plot(x_app, col = trainColors, xlab = "X1", ylab = "X2")
pente <- Vp$vectors[2,1] / Vp$vectors[1,1]
abline(a = 0, b = pente, col = "orange")
dev.copy (png, "./report/images/projection_line_afd.png")
dev.off()

# calculate the  projections
ScalarProduct_app_AFD <- x_app %*% (Vp$vectors[, 1]) / sqrt(sum(Vp$vectors[, 1] * Vp$vectors[, 1]))
appProjections <- array(dim = dim(x_app))
appProjections[, 1] <- ScalarProduct_app_AFD * Vp$vectors[1, 1]
appProjections[, 2] <- ScalarProduct_app_AFD * Vp$vectors[2, 1]
# visualise the projections
points(appProjections, col = trainColors)
dev.copy (png, "./report/images/train_projections_afd.png")
dev.off()

# 3.3 Training LDA using AFD
ScalarProduct_test_AFD <- x_test %*% (Vp$vectors[, 1]) / sqrt(sum(Vp$vectors[, 1] * Vp$vectors[, 1]))
testProjections <- array(dim = dim(x_app))
testProjections[, 1] <- ScalarProduct_app_AFD * Vp$vectors[1, 1]
testProjections[, 2] <- ScalarProduct_app_AFD * Vp$vectors[2, 1]

x_app_AFD.lda <- lda(ScalarProduct_app_AFD, classe_app)
assigne_test <- predict(x_app_AFD.lda, newdata = ScalarProduct_test_AFD)
# Estimation des taux de bonnes classifications
table_classification_test <- table(classe_test, assigne_test$class)
# table of correct class vs. classification
diag(prop.table(table_classification_test, 1))
# total percent correct
taux_bonne_classif_test <- sum(diag(prop.table(table_classification_test)))
# forme de la classe 1 LABEL ASSIGNATION
possibleShapes <- c (0, 1, 2)
shape <- rep(1, length(x_test))
for (i in 1:noClasses)
  shape[assigne_test$class == i] = possibleShapes[i]
# Affichage des projections apprentissage classees
plot(x_test, col = testColors, pch = shape, xlab = "X1", ylab = "X2")
abline(a = 0, b = pente, col = "orange")
points (testProjections, col = testColors)
# save plot
dev.copy (png, "./report/images/test_predictions_lda_afd.png")
dev.off()


# 3.5
# Train the LDA on the original data
x_app.lda <- lda(x_app, classe_app)
assigne_test <- predict(x_app.lda, newdata = x_test)
# Estimation des taux de bonnes classifications
table_classification_test <- table(classe_test, assigne_test$class)
# table of correct class vs. classification
diag(prop.table(table_classification_test, 1))
# total percent correct
taux_bonne_classif_test <- sum(diag(prop.table(table_classification_test)))
# forme de la classe 1 LABEL ASSIGNATION
possibleShapes <- c (0, 1, 2)
shape <- rep(1, length(x_test))
for (i in 1:noClasses)
  shape[assigne_test$class == i] = possibleShapes[i]
# Affichage des projections apprentissage classees
plot(x_test, col = testColors, pch = shape, xlab = "X1", ylab = "X2")

# now also plot the decision boundaries
# using TP06 for this
xp1 <- seq(min(x_app[,1]), max(x_app[,1]), length=50)
xp2 <- seq(min(x_app[,2]), max(x_app[,2]), length=50)
grille <- expand.grid(x1=xp1, x2=xp2)
grille <- cbind(grille[,1], grille[,2])

Zp <- predict(x_app.lda, grille)
zp<-Zp$post[,3]-pmax(Zp$post[,2],Zp$post[,1])
contour(xp1,xp2,matrix(zp,50), add=TRUE, levels=0, drawlabels=FALSE)
zp<-Zp$post[,2]-pmax(Zp$post[,3],Zp$post[,1])
contour(xp1,xp2,matrix(zp,50), add=TRUE, levels=0, drawlabels=FALSE)
# save plot
dev.copy (png, "./report/images/test_predictions_lda_contours.png")
dev.off()
