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
    stop(paste("Phenotype column", phenotype_col, "have less than 2 valuess"))
  } else if (class(df[, phenotype_col]) == "logical" | length(values) == 2) {
    phenotype_type <- "Cases/Controls"
  } else if (class(df[, phenotype_col]) %in% c("character", "factor") & length(values) > 2) {
    phenotype_type <- "Categorical"
    df[, phenotype_col] <- as.factor(df[, phenotype_col])
  } else if (class(df[, phenotype_col]) %in% c("numeric", "integer", "double")) {
    #check first if the variable follow normal distribution
    #if we have n > 5000, we need to run shapiro multiple times
    n_pheno <- sum(!is.na(df[, phenotype_col]))
    if (n_pheno>5000) {
      st <- 0
      for (i in 1:10) {
        t <- shapiro.test(df[sample(1:n_pheno, 5000, replace = T), phenotype_col])$p.value
        st <- st+(t>0.05)
      }
      if (st < 3) {
        warning(paste("Phenotype column", phenotype_col, "is continuous and not normal, please normalise prior to run association"))
      }
    } else {
      t <- shapiro.test(df[sample(1:n_pheno, n_pheno, replace = T), phenotype_col])$p.value
      if (t <= 0.05) {
        warning(paste("Phenotype column", phenotype_col, "is continuous and not normal, please normalise prior to run association"))
      }
    }
    phenotype_type <- "Continuous"
    df[, phenotype_col] <- as.numeric(df[, phenotype_col])
  } else if (phenotype_type == "unknown") {
    stop(paste("Unable to identify phenotype type for", phenotype_col, "Please provide a Continuous, Categorical or Cases/Controls Phenotype"))
  }

  return(phenotype_type)
}
