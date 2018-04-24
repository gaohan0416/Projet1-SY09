#Projet 1

# 1 - Cuisine
recettes <- read.csv("Projet1/donnees/recettes-pays.data", header=T, row.names=1)
cor.recettes <- cor(recettes)

#ACP :
Y <- as.matrix(recettes)
n <- dim(Y)[1]

#centrage et reduction
X <- Y - matrix(1,n,1) %*% apply(Y,2,mean)
X <- X/matrix(1,n,1) %*% apply(X,2,sd)

#matrice de covariance
V <- (1/n)*t(X) %*% X

#calcul des valeurs propres et des axes d'inertie
eigen <- eigen(V, symmetric = T)
L <- diag(eigen$values)
U <- eigen$vectors

#calcul des composantes principales
C <- X %*% U

comp1 <- C[,1]
comp2 <- C[,2]
comp3 <- C[,3]
plot(comp1, comp3)
