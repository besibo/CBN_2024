# Mise en mémoire des packages ---- 
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)   # Pour les résumés statistiques
library(rstatix) # Pour faire les tests statistiques
library(pwr)     # Pour faire les tests de puissance
library(effectsize) # Pour calculer automatiquement le "d" de Cohen

# Importation et mise en forme des données ----
cam_raw <- read_excel("Data/Camille/2022_NGUYEN (1).xlsx") |> 
  clean_names()

cam <- cam_raw |> 
  pivot_longer(cols = -c(type, semis, origine, taxons),
               names_to = "year",
               values_to = "abondance") |> 
  mutate(year = str_remove(year, "x")) |> 
  separate(year, into = c("annee", "replicat"), sep = "_", remove = FALSE) |> 
  mutate(annee = as.numeric(annee)) |>    # Transformation en numérique
  mutate(replicat = row_number(), .by = c(taxons, annee)) |> 
  mutate(abondance = str_replace(abondance, "\\+", "6"),
         abondance = case_when(
           abondance == 0 ~ 0,
           abondance == 1 ~ 0.025,
           abondance == 2 ~ 0.15,
           abondance == 3 ~ 0.375,
           abondance == 4 ~ 0.625,
           abondance == 5 ~ 0.875,
           abondance == 6 ~ 0.002
         ),
         replicat = factor(replicat),
         annee = factor(annee)) |> 
  mutate_if(is.character, as.factor)
  
# Exploration statistique des données ----
summary(cam)

cam |> 
  skim()

cam |> 
  group_by(type, semis) |> 
  skim(abondance)

cam |> 
  group_by(semis) |> 
  skim(abondance)


# Exploration graphique des données ----
cam |> 
  ggplot(aes(x = type, y = abondance)) +
  geom_boxplot(notch = TRUE) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.4) +
  scale_y_log10()

cam |> 
  ggplot(aes(x = annee, y = abondance)) +
  geom_violin() +
  scale_y_log10() +
  facet_grid(type ~ semis)

# Comparaison de la moyenne (des rangs) entre les 3 types
# Test de Kruskal-Wallis (équivalent non paramétrique de l'ANOVA 1 facteur)
kruskal.test(abondance ~ type, data = cam)

# Calcul de la richesse spécifique pour chaque année
# et chaque réplicat : n_distinct()
cam_R <- cam |> 
  filter(abondance != 0) |> 
  summarise(R = n_distinct(taxons),
            .by = c(replicat, annee))

cam_R |> 
  ggplot(aes(x = R)) +
  geom_density() +
  facet_wrap(~annee)

cam_R |> 
  ggplot(aes(x = annee, y = R)) +
  geom_jitter(width = 0.2, height = 0)


# Calcul de la richesse spécifique pour chaque année
# chaque réplicat, et chaque niveau de semis : n_distinct()
cam_R2 <- cam |> 
  filter(abondance != 0) |> 
  summarise(R = n_distinct(taxons),
            .by = c(replicat, annee, semis))

cam_R2 |> 
  ggplot(aes(x = R)) +
  geom_density() +
  facet_grid(semis~annee)


# Tests de comparaison de moyennes : une population ---- 
# Normalité des données ?
# Test de Shapiro-Wilk
# H0 : les données suivent la loi Normale
# H1 : les données ne suivent pas la loi Normale
cam_R |> 
  filter(annee == 2024) |> 
  shapiro_test(R)

# p < 0.05 : on rejette H0

# Pour comparer la moyenne à la valeur théorique 15,
# on utilise le test de Wilcoxon (non paramétrique)
# H0 : la médiane vaut 15 dans la population générale (mu_r = 15)
# H1 : la médiane est différente de 15 dans la population générale (mu_r != 15)
cam_R |> 
  filter(annee == 2024) |> 
  wilcox_test(R ~ 1, mu = 15, detailed = TRUE)

# Si on avait eu le droit de faire le test de Student
# H0 : la moyenne vaut 15 dans la population générale (mu_r = 15)
# H1 : la moyenne est différente de 15 dans la population générale (mu_r != 15)
cam_R |> 
  filter(annee == 2024) |> 
  t_test(R ~ 1, mu = 15, detailed = TRUE)


# Tests de comparaison de moyennes : deux populations, données appariées ----
cam_R |> 
  filter(annee != 2024)

# Condition de normalité : la différence entre les 2 séries, doit suivre la loi normale.
cam_R |> 
  filter(annee != 2024) |> 
  pivot_wider(names_from = annee, values_from = R) |> 
  mutate(diff = `2023` - `2022`) |> 
  shapiro_test(diff)

# p > 0.05 : la différences suit la loi Normale.
# On a le droit de faire un test paramétrique (Student) pour comparer les moyennes
# H0 : mu_2022 == mu_2023
# H1 : mu_2022 != mu_2023
cam_R |> 
  filter(annee != 2024) |> 
  pivot_wider(names_from = annee, values_from = R) |> 
  mutate(diff = `2022` - `2023`) |> 
  t_test(diff~1, mu = 0, detailed = TRUE)

cam_R |> 
  filter(annee != 2024) |> 
  t_test(R ~ annee, paired = TRUE, detailed = TRUE)

# p > 0.05. On ne peut pas rejeter H0.
# Au seuil alpha de 5%, un test de de Student n'a pas permis de rejeter l'égalité des moyennes (paired t-test : t = 0.652, df = 5, p = 0.54)


# Tests de comparaison de moyennes : deux populations, données indépendantes ----
# Conditions d'applications :
# - chaque série doit suivre la loi Normale
# - les deux séries doivent avoir même variance (variances homogènes)

# Condition de normalité
cam_R |> 
  filter(annee != 2024) |> 
  group_by(annee) |> 
  shapiro_test(R)
# Pour chaque groupe p > 0.05 : normalité respectée

# Homoscédasticité
# H0 : les groupes ont même variance
# H1 : les groupes n'ont pas même variance
cam_R |> 
  filter(annee != 2024) |> 
  levene_test(R ~ annee)
# p > 0.05 : les deux séries ont même variance

# Test de Student
cam_R |> 
  filter(annee != 2024) |> 
  t_test(R ~ annee, detailed = TRUE, var.equal = TRUE)

# Si on n'avait pas eu le droit de faire un test paramétrique de Student :
cam_R |> 
  filter(annee != 2024) |> 
  wilcox_test(R ~ annee, detailed = TRUE)


# Générer des données qui suivent une distribution donnée
cam_R |> 
  filter(annee == 2024) |> 
  skim(R)

ech <- rnorm(100, mean = 16.7, sd = 2)
ech

# Un exemple de bootstrap pour calculer, par exemple, l'intervalle de confiance
# à 95% de l'écart-type des richesses spécifiques
# 1. on récupère les valeurs de richesses specifiques observées dans un vecteur
Richesse <- cam_R |> 
  filter(annee == 2024) |> 
  pull(R)
Richesse

# On calcule l'écart-type observé
sd(Richesse)

# On crée un nouvel échantillon de même taille en tirant avec remise le même
# nombre de valeurs (ici, 10) dans le vecteur des richesses spécifiques et on 
# re-calcule l'écart-type
sd(sample(Richesse, size = 10, replace = TRUE))

# On fait tout ça 5000 fois pour obtenir 5000 valeurs d'écart-types
sd_tab <- tibble(rep = 1:5000) |> 
  mutate(sd_rep = map_dbl(rep,
                          ~sd(sample(Richesse, size = 10, replace = TRUE))))

sd_tab

# On récupère les quantiles 0.025 et 0.975 de sd : ce sont les bornes de 
# l'intervalle de confiance à 95% de l'écart-type
sd_tab |> 
  reframe(sd_IC = quantile(sd_rep, c(0.025, 0.975)))

# On peut faire la même chose avec n'importe quel indice statistique : moyenne,
# médiane, variance, quartiles... Il suffit de remplacer la fonction sd() de la
# ligne 213 ci-dessus



# Tests de puissance : packages pwr et effectsize ----
?pwr.t.test()

# Calcul de l'écart-type de R en 2022 et 2023 (données prises ensemble)
cam_R |> 
  filter(annee != 2024) |> 
  summarise(et = sd(R))

# La différence de richesse moyenne entre 2022 et 2023 vaut 0.667 (voir résultat)
# du test de Student

# Calcul manuel du d de Cohen (différence de moyennes / écart-type)
d <- 0.667 / 1.68
d

# Calcul automatique du d de Cohen avec le package effectsize
cohens_d(R ~ annee, data = cam_R |> filter(annee != 2024))
# L'avantage d'utiliser cette fonction : on obtient aussi un intervalle de 
# confiance à 95% pour le d de Cohen.


# Calcul de la puissance statistique. Nous avons :
# - 6 observations dans chaque groupe
# - une valeur de d
# - un seuil de significativité de 0.05
pwr.t.test(n = 6, d = d, sig.level = 0.05, 
           type = "two.sample", alternative = "two.sided")
# La puissance statistique d'un test de Student ne sera que de 0.096, soit moins
# de 10%. Autrement dit, s'il y a une différence réelle de richesse spécifique
# de 0.667 ou moins entre nos 2 groupes, nous n'aurons que 10% de chances qu'un
# test de Student bi-latéral sur données indépendantes ne la détecte.

pwr.t.test(n = 6, d = d, sig.level = 0.05, 
           type = "paired", alternative = "two.sided")
# Avec des données appariées (ça n'est pas le cas ici), on gagnerait 3% de puissance.

# Calcul des tailles d'échantillons nécessaires pour atteindre une puissance 
# donnée, par exemple 80%
pwr.t.test(d = d, sig.level = 0.05, power = 0.8,
           type = "two.sample", alternative = "two.sided")
# Il faudrait 100 réplicats dans chaque groupe pour atteindre une puissance de 
# 80%, avec un d de Cohen toujours égal à 0.397 (soit une différence de moyennes
# de richesse spécifique entre groupes de 0.667)

# En revanche, si seules les différences de richesses de 2 unités ou plus entre 
# les groupes nous intéresse :
pwr.t.test(d = 2 / 1.68, sig.level = 0.05, power = 0.8,
           type = "two.sample", alternative = "two.sided")
# il ne faut "plus que" 13 réplicats dans chaque groupe pour atteindre une 
# puissance de 80%. Une autre façon de le dire : avec des échantillons de taille
# n = 13, s'il existe une différence de richesse spécifique d'au moins 2 entre
# les groupes, nous aurons 80% de chances qu'un test de Student bilatéral soit 
# en mesure de le détecter.


# Enfin, dernière possibilité : si on sait déjà combien d'échantillons on sera
# en mesure de collecter (par exemple, 20 dans chaque groupe), et la puissance 
# qu'on souhaite atteindre (ici, 80%), on peut déterminer le d de Cohen :
pwr.t.test(n = 20, sig.level = 0.05, power = 0.8,
           type = "two.sample", alternative = "two.sided")
# Ici, d = 0.909, soit, compte tenue de l'écart-type calculé plus haut,  une
# différence de moyenne entre les 2 groupes de :
0.909 * 1.68

# Autrement dit, avec 20 réplicats dans chaque groupe, une différence de moyenne
# de richesse spécifique de 1.52 (ou plus) entre les groupes sera détectable dans
# 80% des cas par un test de Student bilatéral. Si la différence de moyennes est
# plus faible, la puissance (donc la probabilité que le test de Student ne la 
# détecte) sera plus faible.

cam_R |> 
  filter(annee != 2024) |> 
  ggplot(aes(x = annee, y = R)) +
  geom_boxplot(notch = TRUE)
