#' @title
#' Multiple PRS Associations from different Phenotypes
#'
#' @description
#' `multiphenassoc()` take a distribution of PRS and multiple Phenotypes and eventual confounders
#' return a data frame showing the association results
#'
#' @param df a dataframe with individuals on each row, at least one ID column,
#' one column PRS (continuous variable) and one with phenotype (continuous or discrete)
#' @param prs_col a character specifying the PRS column name
#' @param phenotype_col a character vector specifying the Phenotype column names
#' @param scale a boolean specifying if scaling of PRS should be done before testing
#' @param covar_col a character vector specifying the covariate column names (facultative)
#' @param log 	a connection, or a character string naming the file to print to.
#' If "" (by default), it prints to the standard output connection, the console unless redirected by sink.
#'
#' @return return a data frame showing the association of the PRS on the Phenotypes
#' with 'PRS','Phenotype','Covar','N_cases','N_controls','N','OR','SE','lower_CI','upper_CI','P_value'
#' @importFrom stats na.omit
#' @export
multiphenassoc <- function(df = NULL, prs_col = "SCORESUM", phenotype_col = "Phenotype",
                           scale = TRUE, covar_col = NA, log = "") {
  ## Checking inputs
  n_pheno <- length(phenotype_col)
  cat("\n\n------\nMultiple  phenotypes associations (", n_pheno, ") testing:", file = log, append = F)
  # checking inputs (done in assoc that calls df_checker)
  if (n_pheno <= 1) {
    warning("No multiple Phenotypes given, preferably use assoc() function")
  } else if (!class(log)[1] %in% c("character","url","connection")) {
    stop("Please provide a connection, or a character string naming the file to print to for 'log'")
  }

  ## Creating progress bar
  cat("\n", file = log, append = T)
  progress <- txtProgressBar(min = 0, max = n_pheno, initial = 0, style = 3)

  ## Creating the score table
  scores_table <- data.frame(matrix(nrow = 0, ncol = 11))
  names(scores_table) <- c(
    "PRS", "Phenotype", "Covar", "N_cases", "N_controls",
    "N", "OR", "SE", "lower_CI", "upper_CI", "P_value"
  )


  ## For loop of assoc function
  for (i in 1:n_pheno) {
    scores_table <- rbind(scores_table, assoc(
      df = df, prs_col = prs_col,
      phenotype_col = phenotype_col[i],
      scale = scale, covar_col = covar_col,
      log = log
    ))

    cat("\n", file = log, append = T)
    setTxtProgressBar(progress, i)
    cat("\n", file = log, append = T)
  }

  # returning the result
  return(scores_table)
}
