#' moyenne d’un vecteur
#' Une fonction pour faire une moyenne en enlevant les valeurs manquantes
#'
#' @param x un vecteur numerique
#'
#' @return la fonction renvoie la moyenne d'un vecteur
#' @importFrom stats na.omit
#' @export
moyenne <- function(x){
  x <- x %>% na.omit()
  sum(x)/length(x)
}
