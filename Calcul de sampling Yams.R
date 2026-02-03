#####################By
# La formule de Morgan et Krejcie est utilisée pour déterminer la taille d'un échantillon à partir d'une population donnée. Elle est définie par la formule suivante :
# S = taille de l'échantillon
#  x2= la valeur critique de chi-carré pour un niveau de confiance donné (généralement 1,96 pour un niveau de confiance de 95 %)
# N = taille de la population
# P = proportion estimée de la population (par défaut souvent 0,5, car c'est le cas le plus conservateur)
# d = marge d'erreur acceptée (exprimée en proportion, par exemple 0,05 pour 5 %)
# Fonction pour calculer la taille de l'échantillon avec la formule de Morgan et Krejcie
taille_echantillon <- function(N, P = 0.5, d = 0.05, niveau_confiance = 0.95) {
  # Valeur critique du chi-carré pour le niveau de confiance (Z-score)
  Z <- qnorm(1 - (1 - niveau_confiance) / 2)
  
  # Calcul de la taille de l'échantillon
S <- ((Z^2 * N * P * (1 - P)) / (d^2 * (N - 1) + Z^2 * P * (1 - P)))
  
  return(ceiling(S))
}

# Exemple d'utilisation
N <- 16826  # Taille de la population
P <- 0.5    # Proportion estimée (par défaut 0.5)
d <- 0.07   # Marge d'erreur (5 %)
niveau_confiance <- 0.95  # Niveau de confiance (95 %)
taille_echantillon(N, P, d, niveau_confiance)

# la méthode de Leslie Kish qui ajuste la taille de l’échantillon pour les enquêtes en grappes à l’aide de la variance intercluster. Kish propose de prendre en compte la corrélation intraclasse ou la variance entre les grappes lorsqu’on échantillonne des groupes (grappes) au lieu d’individus. Cette méthode est utilisée pour ajuster la taille de l’échantillon en fonction du degré de similarité au sein des grappes.
taille_echantillon_ajustee_kish <- function(N, P = 0.5, d = 0.05, niveau_confiance = 0.95, m = 10, rho = 0.02) {
  # Valeur critique du chi-carré pour le niveau de confiance (Z-score)
  Z <- qnorm(1 - (1 - niveau_confiance) / 2)
  
  # Taille de l'échantillon initiale sans ajustement
  n_initial <- (Z^2 * N * P * (1 - P)) / (d^2 * (N - 1) + Z^2 * P * (1 - P))
  
  # Ajustement avec l'effet de grappe basé sur la taille moyenne des grappes (m) et la corrélation intraclasse (rho)
  n_ajustee <- n_initial * (1 + (m - 1) * rho)
  
  return(ceiling(n_ajustee))  # Retourne la taille ajustée arrondie à l'entier supérieur
}

# Exemple d'utilisation
N <- 16826   # Taille de la population
P <- 0.5     # Proportion estimée (par défaut 0.5)
d <- 0.07    # Marge d'erreur (5 %)
niveau_confiance <- 0.95  # Niveau de confiance (95 %)
m <- 8      # Taille moyenne des grappes
rho <- 0.06  # Corrélation intraclasse 

taille_echantillon_ajustee_kish(N, P, d, niveau_confiance, m, rho)


