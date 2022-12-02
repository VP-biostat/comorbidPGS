#' take a distribution of PRS and multiple Phenotypes, the name of the analysis and eventual confounders
#' return a data frame showing the regression of the unique PRS on the list of Phenotypes
#'
#' @param df a dataframe with individuals on each row, at least one column PRS
#'(continuous variable) and one with phenotype (continuous or categorical)
#' @param prs_col a character specifying the PRS column name
#' @param phenotype_col a character specifying the Phenotype column name
#' @param scale a boolean specifying if scaling of PRS should be done before testing
#' @param covar_col a character vector specifying the covariate column names (facultative)
#'
#' @return return a data frame showing the regression of the PRS on the Phenotypes
#' with 'PRS','Phenotype','N_cases','N_controls','N','OR','SE','lower_CI','upper_CI','P_value'
#' @import data.table
#' @importFrom stats na.omit
#' @export
multiassoc <- function(df = NA, prs_col = "SCORESUM", phenotype_col = "Phenotype",
                  scale = TRUE, covar_col = NA) {
  ## Checking inputs
  cat("\nComorbidPRS - Launching multi-association testing...\n")
  #checking inputs
  if (is.na(df)) {
    stop("Error: Please provide a data frame (that includes PRS values with at least
         3 columns: individual_id, PRS, Phenotype)")
  } else if (ncol(df<3)) {
    stop("Error: Please provide a data frame (that includes at least 3 columns:
         individual_id, PRS, Phenotype)")
  }

  ##Creating the score table
  n_pheno <- length(phenotype_col)
  scores_table <- data.frame(matrix(nrow = n_pheno,ncol = 10))
  names(scores_table) <- c('PRS','Phenotype','N_cases','N_controls','N','OR','SE','lower_CI','upper_CI','P_value')


  ## For loop of assoc function
  for (i in 1:n_pheno) {
    score_table[i,] <- assoc(df = df, prs_col = prs_col, phenotype_col =
                         phenotype_col[i], scale = scale, covar_col = covar_col)
  }

  #returning the result
  return(score_table)

}
