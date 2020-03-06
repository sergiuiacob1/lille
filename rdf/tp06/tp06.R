data <- load(file='simul-2017.Rdata')
data

# 1.1
# for training data: assign colors to classes
class_colors = c('red', 'green', 'blue')
couleur <- rep('red', n_app)
for (class_no in 1:3)
  couleur[classe_app == class_no] = class_colors[class_no]
# plot the training data and see how the classes look
plot (x_app, col=couleur, main='Training data', xlab='First feature', ylab='Second feature')

# plot test data
for (class_no in 1:3)
  couleur[classe_test == class_no] = class_colors[class_no]
plot (x_test, col=couleur, main='Test data', xlab='First feature', ylab='Second feature')

# For training data: see a priori probabilities
# P(w) = probability of an instance to be in the class "w"
class_probs <- 1:3
for (class_no in 1:3)
  class_probs[class_no] = sum(couleur == class_colors[class_no]) / length(couleur)
class_probs

# 1.2
# calculate means
means <- array(dim = c(3, 2))
for (class_no in 1:3){
  means[class_no, 1] <- mean (x_app[classe_app == class_no, 1])
  means[class_no, 2] <- mean (x_app[classe_app == class_no, 2])
}
# plot the means for train data
for (class_no in 1:3)
  couleur[classe_app == class_no] = class_colors[class_no]
plot (x_app, col=couleur, main='Means for train data', xlab='First feature', ylab='Second feature')
points (means, col=class_colors, p=8, cex=4)

# calculate co-variance
sigma <- c()
for (class_no in 1:3){
  # number of instances = n_app = dim(x_app)[1]
  # number of features = dim(x_app)[2]
  sigma[[class_no]] <- matrix(0, dim(x_app)[2], dim(x_app)[2])
  for (i in 1:dim(x_app)[2])
    for (j in 1:dim(x_app)[2])
      sigma[[class_no]][i, j] <- cov(as.vector(x_app[classe_app == class_no, i]), as.vector (x_app[classe_app == class_no, j]))
}
# analyse sigma
sigma

# 1.3
library("MASS")
library("lattice")
# Grille d'estimation de la densit´e de probabilit´e en 50 intervalles selon 1er attribut
xp1 <- seq(min(x_app[,1]), max(x_app[,1]), length=50)
# Grille d'estimation de la densit´e de probabilit´e en 50 intervalles selon 2eme attribut
xp2 <- seq(min(x_app[,2]), max(x_app[,2]), length=50)
grille <- expand.grid(x1=xp1,x2=xp2)
plot (grille)

x_app.lda <- lda(x_app, classe_app)
# Estimation des densit´es de probabilit´es a posteriori dans Zp
grille = cbind(grille[,1], grille[,2])
Zp <- predict(x_app.lda, grille)


plot(grille)
zp<-Zp$post[,3]-pmax(Zp$post[,2],Zp$post[,1])
contour(xp1,xp2,matrix(zp,50),add=TRUE,levels=0,drawlabels=FALSE)

zp<-Zp$post[,2]-pmax(Zp$post[,3],Zp$post[,1])
contour(xp1,xp2,matrix(zp,50),add=TRUE,levels=0,drawlabels=FALSE)
points(x_app, col=couleur, p=19)

# 1.4

assigne_test<-predict(x_app.lda, newdata=x_test)
# Estimation des taux de bonnes classifications
table_classification_test <-table(classe_test, assigne_test$class)
# table of correct class vs. classification
diag(prop.table(table_classification_test, 1))
# total percent correct
taux_bonne_classif_test <-sum(diag(prop.table(table_classification_test)))

forms <- 1:n_test
possible_forms <- c(15, 17, 19)
for (i in 1:n_test)
  forms[i] = possible_forms[assigne_test$class[i]]
points(x_test, col=couleur, p=forms)
