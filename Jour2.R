# Mise en mémoire des packages nécessaires ----
library(tidyverse)
library(palmerpenguins)   # Pour accéder au jeu de données penguins
library(readxl)           # Pour importer des fichiers excel
library(janitor)          # Pour "nettoyer" les noms de variables

# Importation des données dauphin ----

# Le tableau dauphin : avec la fonction read.table()
dauphin <- read.table(file = "Data/Benoit/dauphin.csv",
                      header = TRUE, sep = ";", dec = ",")

# L'objet obtenu est un data.frame
class(dauphin)

# Transformation en tibble
dauphin <- as_tibble(dauphin)
class(dauphin)

# Affichage des données
dauphin

# Importation avec l'assistant d'importation ----
# Code généré par l'assistant :
dauphin <- read_delim("Data/Benoit/dauphin.csv", 
                      delim = ";", escape_double = FALSE,
                      locale = locale(decimal_mark = ","), 
                      trim_ws = TRUE)

# L'objet obtenu est un tibble
class(dauphin)


# La fonction summary() est une fonction générique
# La nature des résultats renvoyés dépend de la nature de l'objet fourni
summary(1:10)
summary(factor(c("a", "a", "b")))
summary(dauphin)

# C'est la même chose pour la fonction print()
print(dauphin)

# Importation d'un fichier excel : données de Hugues
dat <- read_excel("Data/Hugues/6170_formation_stats.xlsx", 
                  na = "NA", n_max = 2783) |> 
  clean_names()   # Pour corriger les pb de noms de variables

dat

# Importation des données de Camille
cam_raw <- read_excel("Data/Camille/2022_NGUYEN (1).xlsx") |> 
  clean_names()

View(cam_raw)
cam_raw


# Représentations graphiques ----

## 1. Les nuages de points ----

dauphin

# Nuage de points. Il s'agit d'un bubble plot : la taille des points
# est proportionnelle à une variable numérique (ici, le Cd)
ggplot(data = dauphin, mapping = aes(x = Age, y = Taille)) +
  geom_point(aes(size = Cd), alpha = 0.5, color = "blue") + 
  geom_smooth(color = "green")


ggplot(data = dauphin, mapping = aes(x = Age, y = Taille, size = Cd)) +
  geom_point(aes(fill = Sexe), color = "grey", shape = 24)

## 2. Visualiser les distributions ----

### Histogrammes ----

ggplot(dauphin, aes(x = Taille)) +
  geom_histogram(bins = 20)

# Modification des classes et des couleurs
ggplot(dauphin, aes(x = Taille)) +
  geom_histogram(binwidth = 30, fill = "white", color = "grey20")

# Diagramme de densité : pour éviter d'avoir à choisir un nombre de classes
ggplot(dauphin, aes(x = Taille)) +
  geom_density(fill = "white")

# Association d'une variable à la couleur de remplissage
ggplot(dauphin, aes(x = Taille)) +
  geom_histogram(aes(fill = Sexe), binwidth = 30, color = "grey20")

### Création de sous-figures ---- 
# avec facet_wrap()
ggplot(dauphin, aes(x = Taille)) +
  geom_histogram(binwidth = 30, fill = "white", color = "grey20") +
  facet_wrap(~ Sexe, ncol = 1, scales = "free_y")

# avec facet_grid()
ggplot(dauphin, aes(x = Taille)) +
  geom_histogram(aes(fill = Sexe), binwidth = 30,  color = "grey20") +
  facet_grid(Repro ~ Sexe)

### Stripcharts : geom_jitter() ----
ggplot(dauphin, aes(x = Sexe, y = Taille)) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.3)

### Boîtes à moustaches (et stripcharts) ----
# Attention à la couleur des outliers
ggplot(dauphin, aes(x = Sexe, y = Taille)) +
  geom_boxplot(color = "red", notch = TRUE) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.3)

ggplot(dat, aes(x = classe_alt, y = pente_moy)) +
         geom_boxplot(outlier.color = NA, notch = TRUE) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.2)

### Diagrammes bâtons ----
# Avec geom_bar() : quand les données ne sont pas "pré-comptées"
ggplot(dat, aes(x = classe_alt)) +
  geom_bar(alpha = 0.5, 
           color = "purple")

# Avec geom_col() : quand les données sont "pré-comptées"
dat |> 
  count(classe_alt) |> 
  ggplot(aes(x = classe_alt, y = n)) +
  geom_col()

# Ré-agencement des modalités sur l'axe des x
ggplot(dat, aes(x = fct_rev(fct_infreq(classe_alt)))) +
  geom_bar()

# Diagramme bâtons empilé
ggplot(dat, aes(fill = classe_alt, x = classe_exposition)) +
  geom_bar(alpha = 0.8)

# Diagramme bâtons juxtaposé
ggplot(dat, aes(fill = classe_alt, x = classe_exposition)) +
  geom_bar(alpha = 0.8, position = "dodge")

# Comparaison de proportions (et non plus de fréquences)
ggplot(dat, aes(x = pol_ss_type1, fill = pol_ss_type2)) +
  geom_bar(position = "fill") +
  facet_wrap(~ classe_alt)

# Améliorer l'aspect des graphiques ----
# La fonction labs() pour modifier les labels
ggplot(data = dauphin,
       mapping = aes(x = Age, y = Taille,
                     size = Cd)) +
  geom_point(aes(fill = Sexe), color = "grey",
             shape = 24) +
  labs(x = "Âge (années)",
       y = "Taille (cm)",
       size = "[Cadmium]")

# Les fonctions scales_... et theme_... pour modifier les échelles et les thèmes
ggplot(dauphin, aes(x = Age, y = Taille, fill = Sexe)) +
  geom_point(shape = 21, alpha = 0.5, color = "grey20") +
  geom_smooth(fill = "grey80") +
  labs(fill = "Le sexe", 
       title = "Mon titre",
       subtitle = "Mon sous-titre", 
       caption = "La source des données") +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  scale_fill_manual(values = c("orange", "purple")) +
  theme_bw(base_size = 12, base_family = "Gill Sans")
#  theme(legend.position = "top", 
        # panel.background = element_rect(fill = "red"),
        # panel.grid.major = element_line(linewidth = 5)
        # )
  

# Manipulation de tableaux ----

## Le pipe ----
  
a <- c(23, 43, 2, 1)

# Ces deux commandes sont équivalentes
log(a)
a |> log()

## Filtrer des lignes : filter()  ----

# Pour ne garder que les lignes des mâles
dauphin |> 
  filter(Sexe == "m")

# Pour ne garder que les lignes des mâles âgés de 2 ans ou plus
dauphin |> 
  filter(Sexe == "m", Age >= 2)

# Strictement équivalent à ceci :
dauphin |> 
  filter(Sexe == "m" & Age >= 2)

# Pour garder les lignes des mâles ou des individus âgés de 2 ans ou plus
# Ou logique : |
dauphin |> 
  filter(Sexe == "m" | Age >= 2)

# Tous les individus dont le statut repro est imm ou pnl
# Les lignes pour lesquelles Repro est "imm" et 
# les lignes pour lesquelles Repro est "pnl"
dauphin |> 
  filter(Repro == "imm" | Repro == "pnl")

# une autre façon de faire, plus compacte et plus claire (selon moi)
dauphin |> 
  filter(!(Repro %in% c("imm", "pnl")))

# Éliminer les lignes de dat qui correspondent à la classe d'altitude 500-1000m
dat |> 
  filter(classe_alt != "500-1000m") |> 
  ggplot(aes(x = classe_alt, y = pente_moy)) +
  geom_boxplot()

# la fonction is.na() pour éliminer les données manquantes pour une variable
# spécifique
dat |> 
  filter(!is.na(pol_ss_type1))

## Trier des tableaux : arrange() ----
# Tri par ordre croissant
dauphin |> 
  arrange(Cd, Hg)

# Tri par ordre décroissant
dauphin |> 
  arrange(desc(Cd), desc(Hg))

# Attention, trier les données ne permet pas de changer l'ordre des modalités
# d'un facteur
dauphin |> 
  mutate(Sexe = factor(Sexe)) |> 
  arrange(desc(Sexe)) |> 
  ggplot(aes(x = Sexe, y = Taille)) +
  geom_boxplot()

dauphin |> 
  mutate(Sexe = factor(Sexe)) |> 
  ggplot(aes(x = fct_rev(Sexe), y = Taille)) +
  geom_boxplot()

## Sélectionner des variables : select() ----
dauph2 <- dauphin |> 
  select(Cd, Sexe, Stat = Repro)

dauphin |> 
  select(-Id)

# Utilisation des "helper functions"
dat |> 
  select(id_obj, starts_with("pol"))

dat |> 
  select(id_obj, ends_with("moy"))

dat |> 
  select(id_obj, contains("classe"), alt_min)

dat |> 
  select(contains("tpi"), everything())


## Créer ou modifier des variables : mutate() ----
# Création d'une variable ratio : longueur / hauteur du bec
penguins |> 
  mutate(ratio = bill_length_mm / bill_depth_mm) |> 
  relocate(ratio, .after = island) |> 
  filter(ratio >= 3) |> 
  ggplot(aes(x = species, y = ratio)) +
  geom_jitter(width = 0.2, height = 0)

# Création d'une variable logique : les individus ont-ils des nageoires plus
# longues ou plus courtes que la moyenne ?
penguins |> 
  mutate(long_finn = flipper_length_mm > mean(flipper_length_mm, na.rm = TRUE)) |> 
  relocate(long_finn)

# Pour chaque espèce, identifier les individus qui ont des nageoires plus 
# longues ou plus courtes que la moyenne de l'espèce
penguins |> 
  group_by(species) |> 
  mutate(
    long_finn = if_else(flipper_length_mm > mean(flipper_length_mm, na.rm = TRUE),
                        "big",
                        "small")) |> 
  relocate(long_finn)

# Modification de la variable Taille : changement d'unité
dauph3 <- dauphin |> 
  mutate(Taille = Taille / 100) 

# Utilisation de la fonction lag() pour calculer des différences entre lignes
# successives
tb <- tibble(year = 1980 : 2020, var = rnorm(41))
tb

tb |> 
  mutate(var2 = lag(var), 
         diff = var - var2)

# Calcul de somme cumulée
tb |> 
  ggplot(aes(x = year, y = var)) +
  geom_point() +
  geom_line()

tb |> 
  mutate(somme = cumsum(var)) |> 
  ggplot(aes(x = year, y = somme)) +
  geom_point() +
  geom_line()


## Résumer les données : summarise() et group_by() ----
dauphin |> 
  summarise(moyenne = mean(Taille),
            variance = var(Taille),
            et = sd(Taille),
            n = n(),
            .by = Sexe)

# Calculs de statstiques descriptives pour plusieurs sous-groupes
dauphin |> 
  group_by(Sexe, Repro) |> 
  summarise(moyenne = mean(Taille),
            variance = var(Taille),
            et = sd(Taille),
            n = n(),
            se = et/sqrt(n))


# Changement de format des tableaux avec le package tidyr ----

# Passer d'un tableau long à un tableau large : pivot_wider()
dauphin |> 
  group_by(Sexe, Repro) |> 
  summarise(moyenne = mean(Taille)) |> 
  pivot_wider(names_from = Repro, values_from = moyenne)


# Passer d'un tableau large à un tableau long : pivot_longer()
cam_raw |> 
  pivot_longer(cols = -c(type, semis, origine, taxons),
               names_to = "year",
               values_to = "abondance")
