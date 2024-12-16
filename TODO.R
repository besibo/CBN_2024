# TODO

- Finir de nettoyer le tableau de Camille :
  * retirer le "x" des années : str_remove()
  * séparer year en year et replicat : separate()
  * remplacer les numéros de réplicats : row_number()
  * remplacer les abondances "\\+" par 6 : str_replace()
  * remplacer les 0 par NA : na_if()
  * transformer abondance en numérique : as.numeric()
  * retirer les NA de lavariable abondance : filter(!is.na))
  * donner un nouveau nom au tableau

- Calculer la richesse spécifique : n_distinct()
- Calculer la diversité

- Tests de puissance
- Tests de comparaisons de moyennes
-> à chaque fois, rappler les conditions d'application et les alternatives non paramétrqiues
-> montrer les tests unilatéraux 
  * Un échantillon ()
  * Deux échantillons appariés (données de Camille)
  * Deux échantillons indépendants (données de Gaëlle)
  * Plus de deux échantillons (données de Gaëlle)
  
  * Expliquer l'intérêt du design apparié

- AFC (données de Gaëlle)
- ACP ( pas de données dispo : olympic)
