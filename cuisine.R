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

#extraction des deux premières composantes
comp1 <- C[,1]
comp2 <- C[,2]
comp3 <- C[,3]

#plan de représentations 1,2
plot(comp1, comp2, type ="n")
text(comp1, comp2,rownames(X))
abline(h=0);abline(v=0)



#Classification hiérarchique ascendante

#matrice de distances manhattan
md.dist <- dist(recettes, method = "manhattan")
md <- as.matrix(md.dist)
#dendogramme, methode d'agréfation de Ward
arbre <- hclust(md.dist, method = "ward.D2")
plot(arbre)

#Algorithme des K-Means
#k = 3
k3.means <- kmeans(recettes, centers = 3)
plot(comp1, comp2, col=c("red","blue","green")[k3.means$cluster])

#k = 5
k5.means <- kmeans(recettes, centers = 5)
plot(comp1, comp2, col=c("red","blue","green", "black", "orange")[k5.means$cluster])


