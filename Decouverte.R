# Mise en mémoires des packages 
library(tidyverse)

# Addition
2 + 8

# Puissance
3 ^ 4


## --- Les Objets ----

# 1. Les vecteurs ----

# Création d'un vecteur numérique
vec <- c(3, 5, 6, 3, 2, 1, 0)

# Opérations sur les vecteurs
vec * 2
mean(vec)
var(vec)         
sort(vec, decreasing = TRUE)

# Création d'un vecteur logiques (i.e. de vrais/faux)
vf <- c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)

# Appliquer des fonctins arithmétiques aux vecteurs logiques les 
# transforme en vecteurs numérique : FALSE = 0, TRUE = 1
vf * 2
sum(vf)  # Nombre de vrais
mean(vf) # Proportion de vrais
summary(vf)

# Progressions régulières
seq(from = 0, to = 23, by = 4)
seq(from = 0, to = 4, by = 0.2)
seq(from = 4, to = 0, by = -1)
seq(from = 0, to = 23, length.out = 10)

# Séquences régulières avec un pas de 1
1:10
4:32
0.2:8.2

# Les vecteurs "mixtes" : le type le plus générique (caractères) est utilisé
c(3, 2, 1, FALSE, TRUE, "rouge", "bleu")

# La notion de recyclage
a <- c(1, 2, 3, 4)
b <- c(5, 6, 7)
d <- c(4, 3)

a + b
a + d
d + a

# 2. Les facteurs ----

# Variables catégorielles
# Création d'un vecteur
coul <- c("rouge", "bleu", "vert", "vert", "rouge", "rouge")
coul
class(coul)

# Transformation en facteur
factor(coul)
coul_fac <- factor(coul, levels = c("vert", "rouge", "bleu", "jaune"))
class(coul_fac)

# Changement de l'ordre des niveaux/modalités du facteur
factor(coul, levels = c("vert", "rouge", "jaune"))


# 3. Les tableaux (data.frames et tibbles) ----
library(palmerpenguins)
penguins
summary(penguins)

# 4. Indexation
# Méthode pour accéder à des éléments spécifiques dans les objets qui en contiennent plusieurs

# Ligne 122 et colonne 3 du tableau penguins
penguins[122, 3]

# Lignes 122 et 132 et colonnes 3 et 8 du tableau penguins
penguins[c(122, 132), c(3, 8)]

# Lignes de 1 à 100 et toutes les colonnes du tableau penguins
penguins[1:100, ]

# Toutes les lignes pour lesquelles la longueur du bec est < à 39.23 mm
# Plus de details sur les filtres dans le script "Jour2.R"
penguins[ penguins$bill_length_mm < 39.23 ,  ]

penguins |> 
  filter(bill_length_mm < 39.23)

# Création d'un data.frame
tab <- data.frame(a = 1:100, b = rnorm(100))
class(tab)

# Transformation en tibble
tab2 <- as_tibble(tab)
tab2

