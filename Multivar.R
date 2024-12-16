# Mise en mémoire des packages ----
library(tidyverse)
library(ade4)      # ACP, AFC, ...
library(vegan)     # Analyse des communautés, indices de diversité
library(readxl)
library(janitor)
library(ggrepel)   # Pour les étiquettes
library(plotly)    # Graphiques interactifs
library(colorspace) # Gestion de palettes de couleurs
library(patchwork)  # Pour intégrer un graphique dans un graphique
library(ggforce)    # Pour créer manuellement le cercle des corrélations


# Importation des données ----
messi_raw <- read_excel("Data/Gaelle/donnees_messicoles_formationGB.xlsx") |> 
  clean_names()

messi_raw |> 
  count(zone_releve)

messi_raw |> 
  count(stellaria_graminea_204, stellaria_graminea_251)

# Mise en forme des données ----
messi <- messi_raw |> 
  mutate(stellaria_graminea = stellaria_graminea_204 + stellaria_graminea_251) |>
  select(-stellaria_graminea_204, -stellaria_graminea_251)

# Création d'un tableau de "variables supplémentaires"
messi_fac <- messi |> 
  select(id_parcelle, zone_releve)

# Création de la table de contingence
messi_sp <- messi |> 
  select(-id_parcelle, -zone_releve)

# messi_sp |> 
#   select(contains("stellaria"))


# Calcul de la diversité de Shannon et de la richesse spécifique
messi_fac <- messi_fac |> 
  mutate(div = diversity(messi_sp), # Diversité de Shannon (vegan)
         R = specnumber(messi_sp))  # Richesse spécifique  (vegan)

messi_fac


# Analyse Factorielle des Correspondances (AFC) ----

# Réalisation de l'analyse
afc <- dudi.coa(messi_sp, scannf = FALSE, nf = 4)

# Quel pourcentage d'info est portée par chaque axe factoriel ?
100 * afc$eig / sum(afc$eig)

# Visualisation des résultats
messi_fac |> 
  bind_cols(afc$li) |> 
  ggplot(aes(x = Axis1, y = Axis2)) +
  geom_point(aes(color = zone_releve))

messi_fac |> 
  bind_cols(afc$li) |> 
  ggplot(aes(x = Axis1, y = Axis2)) +
  geom_point(aes(fill = factor(id_parcelle)), size = 3,
             shape = 21, color = "grey20") +
  geom_point(data = afc$co, aes(x = Comp1, y = Comp2)) +
  geom_text_repel(data = afc$co, aes(x = Comp1, 
                                     y = Comp2,
                                     label = rownames(afc$co)))

# Graphique interactif
graph <- messi_fac |> 
  bind_cols(afc$li) |> 
  ggplot(aes(x = Axis1, y = Axis2)) +
  geom_point(aes(fill = factor(id_parcelle)), size = 3,
             shape = 21, color = "grey20") +
  geom_point(data = afc$co, aes(x = Comp1, y = Comp2, 
                                text = rownames(afc$co))) 
# text : pour afficher le taxon sur la graphique interactif
ggplotly(graph)

# messi_fac |> 
#   bind_cols(afc$li) |> 
#   ggplot(aes(x = Axis3, y = Axis4)) +
#   geom_point(aes(fill = zone_releve), size = 3,
#              shape = 21, color = "grey20")


# Analyse en Composantes Principales (ACP) ----
data(doubs)
dat <- doubs$env
dat

# Réalisation de l'analyse
nvar <- ncol(dat)
acp <- dudi.pca(dat, scannf = FALSE, nf = nvar)
acp

aide <- inertia.dudi(acp, col.inertia = TRUE)

# Affichage des tableaux et interprétation
# 1. données brutes
dat

# 2. Matrice des corrélations entre variables
cor(dat) |> 
  round(3)

# 3. Tableau des valeurs propres
aide$tot.inertia |> 
  round(2)
# On retient les axes 1, 2 et 3 (inertie > 1)

# 4. Affichage des contributions absolues des variables
aide$col.abs |> 
  round(2)
# Quelles sont les variables qu'on aura le droit d'interpréter sur chaque axe factoriel ?
100 / nvar # Valeur seuil
# F1 : dfs, alt, slo, flo, pho, nit, amm, oxy, dbo, har
# F2 : alt, flo, pho, amm, dbo
# F3 : pH

# 5. Affichage des contributions relatives des variables
# Quelle axe porte la plus grande part de l'information des variables de départ ?
# Utile uniquement pour savoir sur quel axe interpréter les variables qui ne sont
# pas encore retenue sur les premiers axes factoriels
aide$col.rel |> 
  round(2) |> 
  abs()

# 6. Cercle des corrélations
# Orientation des axes
s.corcircle(acp$co, xax = 1, yax = 2)
s.corcircle(acp$co, xax = 1, yax = 3)

# 7. Graphique des individus
s.label(acp$li, xax = 1, yax = 2)
s.label(acp$li, xax = 1, yax = 3)

# Un autre exemple de graphique plus "présentable"
acp$li |> 
  ggplot(aes(Axis1, Axis2)) +
  geom_point(aes(fill = dat$dfs / 10, size = dat$amm/100), 
             shape = 21, color = "grey20") +
  geom_text_repel(aes(label = rownames(acp$li))) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(fill = "Distance à\nla source (km)",
       size = "Ammonium\n(mg/l)",
       x    = paste0("Axe F1 (", round(aide$tot.inertia[1,1] * 100 / nvar,1)," %)"),
       y    = paste0("Axe F2 (", round(aide$tot.inertia[2,1] * 100 / nvar,1)," %)")) +
  scale_fill_binned_sequential(palette = "BluYl") +
  theme_bw()

# On visualise bien mieux le fait que :
# - L'axe F1 renseigne sur le comportement normal d'un cours d'eau de l'amont
#   (à gauche) vers l'aval (à droite)
# - L'axe F2 renseigne sur une pollution plus locale en ammonium (et nitrates)


# Pour ajouter le cercle des corrélations dans un coin, c'est plus compliqué :
# 1. On enregistre le graphique precédent dans un objet (ici : indiv)
indiv <- acp$li |> 
  ggplot(aes(Axis1, Axis2)) +
  geom_point(aes(fill = dat$dfs / 10, size = dat$amm/100), 
             shape = 21, color = "grey20") +
  geom_text_repel(aes(label = rownames(acp$li))) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(fill = "Distance à\nla source (km)",
       size = "Ammonium\n(mg/l)",
       x    = paste0("Axe F1 (", round(aide$tot.inertia[1,1] * 100 / nvar,1)," %)"),
       y    = paste0("Axe F2 (", round(aide$tot.inertia[2,1] * 100 / nvar,1)," %)")) +
  scale_fill_binned_sequential(palette = "BluYl") +
  theme_bw()


# 2. On récupère les coordonnées des flèches et on ajoute le nom des variables
correl <- acp$co |> 
  rownames_to_column("var")

# 3. On crée un objet qui contient les caractéristiques du cercle (centré sur
# l'origine et de rayon 1)
cercle <- data.frame(x0 = 0, y0 = 0, r = 1)

# 4. On utilise ggplot2 et le package ggforce pour créer le cercle des corrélations
# à la main et on stocke le résultat dans un objet (ici : correl_circle)
correl_circle <- correl |> 
  ggplot() +
  geom_circle(data = cercle, aes(x0 = x0, y0 = y0, r = r), color = "grey") +
  geom_vline(xintercept = 0, linetype = 2, color = "grey") +
  geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  geom_segment(aes(x = 0, y = 0, xend = Comp1, yend = Comp2),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text_repel(aes(x = Comp1, y = Comp2, label = var)) +
  coord_fixed() +  # Pour que le cercle n'apparaisse pas comme une ellipse
  theme_void() +  # Pour retirer les axes, la grille, les graduations, les titres, etc.
  theme(panel.border = element_rect(fill = NA, color = "black"), # Pour ajouter un contour au graphique
        panel.background = element_rect(fill = "grey98"))        # Pour griser très légèrement le graphique

# On dispose donc de 2 graphiques :
indiv
correl_circle

# 5. On les combine enfin grâce au package patchwork
indiv + inset_element(correl_circle, 0.6, 0.01, 1.1, 0.4)
# Les 4 chiffres correspondent à la position des bords gauche, bas, droit et haut
# du graphique à insérer par rapport au premier graphique. Ici, le bord gauche
# du cercle des corrélations est placé à 60% de l'axe horizontal du graphique
# des individus. Le bas du cercle des corrélations est placé à 1% de l'axe vertical
# de graphique des individus, et ainsi de suite

# 6. Sauvegarde de la figure dans le répertoire de travail
ggsave("acp.png")
