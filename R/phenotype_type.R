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
  values <- unique(df[, phenotype_col])
  phenotype_type <- "unknown"
  if (length(values) < 2) {
    stop("Phenotype column have less than 2 valuess")
  } else if (class(df[, phenotype_col]) == "logical" | length(values) == 2) {
    phenotype_type <- "Cases/Controls"
  } else if (class(df[, phenotype_col]) %in% c("character", "factor") & length(values) > 2) {
    phenotype_type <- "Categorical"
    df[, phenotype_col] <- as.factor(df[, phenotype_col])
  } else if (class(df[, phenotype_col]) %in% c("numeric", "integer", "double")) {
    phenotype_type <- "Continuous"
    df[, phenotype_col] <- as.numeric(df[, phenotype_col])
  } else if (phenotype_type == "unknown") {
    stop("Unable to identify Phenotype type, please provide a Continuous, Categorical or Cases/Controls Phenotype")
  }

  return(phenotype_type)
}
