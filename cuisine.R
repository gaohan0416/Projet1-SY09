#Projet 1
library(cluster)
library(philentropy)

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

k10.means <- kmeans(recettes, centers = 10)

#deuxième jeu de données
recipes <- read.csv("Projet1/donnees/recettes-echant.data", header=T)
origin <- recipes[,1]
recipes <- recipes[,-1]

#matrice disimilarité sur les ingrédients
ingredients <- t(recipes)
colnames(ingredients) <- origin
md.ing <- distance(ingredients, method = "jaccard")
colnames(md.ing) <- rownames(ingredients)
rownames(md.ing) <- rownames(ingredients)
dist.ing <- as.dist(md.ing)
#avec la méthode d'agrégation de ward
arbre.ing <- hclust(dist.ing, method = "ward.D2")
plot(arbre.ing)
#avec la méthode d'agrégation complete
arbre2.ing <- hclust(dist.ing)
#plot(arbre2.ing)

#Algorithme des K-médoides
k2.medoides <- pam(dist.ing, 2)
clusplot(k2.medoides, main = "2-médoides sur les ingrédients", labels = 3, color = T, col.p="black")

k3.medoides <- pam(dist.ing, 3)
clusplot(k3.medoides, main = "3-médoides sur les ingrédients", labels = 3, color = T, col.p="black")

k5.medoides <- pam(dist.ing, 5)
#clusplot(k5.medoides, main = "5-médoides sur les ingrédients", labels = 3, color = T, col.p="black")
