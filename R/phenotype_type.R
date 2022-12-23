#' @title
#' Find the type of Phenotype for the given phenotype_col
#'
#' @param df a dataframe
#' @param phenotype_col a character specifying the column to look for
#'
#' @return return a character specifying the type of Phenotype
#'
#' @noRd
phenotype_type <- function(df = NULL, phenotype_col = "Phenotype") {
  if (length(unique(df[,phenotype_col])) < 2) {
    stop("Phenotype column have less than 2 values")
  }
  phenotype_type <- "Continuous"
  if (typeof(df[,phenotype_col]) == "logical") {
    phenotype_type <- "Cases/Controls"
  } else if (typeof(df[,phenotype_col]) == "character") {
    if (length(unique(df[,phenotype_col])) == 2) {
      phenotype_type <- "Cases/Controls"
    } else if  (length(unique(df[,phenotype_col])) > 2) {
      phenotype_type <- "Categorical"
    }
  } else if (1 %in% unique(df[,phenotype_col]) & 0 %in% unique(df[,phenotype_col])) {
    phenotype_type <- "Cases/Controls"
  }

  return(phenotype_type)
}
