#' @title
#' Multiple PRS Associations from a Data Frame
#'
#' @description
#' `multiassoc()` take a data frame with distribution(s) of PRS and Phenotype(s),
#' and a table of associations to make from this data frame
#' return a data frame showing the association results
#'
#' @param df a dataframe with individuals on each row, at least one column PRS
#'(continuous variable) and one with phenotype (continuous or categorical)
#' @param assoc_table a dataframe or matrix specifying the associations to
#' make from df. Each row should have the format 'PRS', 'Phenotype'
#' @param scale a boolean specifying if scaling of PRS should be done before testing
#' @param covar_col a character vector specifying the covariate column names (facultative)
#'
#' @return return a data frame showing the association of the PRS(s) on the Phenotype(s)
#' with 'PRS','Phenotype','Covar','N_cases','N_controls','N','OR','SE','lower_CI','upper_CI','P_value'
#'
#' @examples
#' assoc_table <- expand.grid(c("PRS_1","PRS_2"),
#'                            c("Phenotype_1","Phenotype_2",
#'                            "Phenotype_3","Phenotype_4"))
#' names(assoc_table) <- c("PRS","Phenotype")
#' results <- multiassoc(df = comorbidExample, assoc_table = assoc_table, covar = c("Age",
#' "Sex","Covariate"))
#' print(results)
#'
#' @importFrom stats na.omit
#' @export
multiassoc <- function(df = NULL, assoc_table = NULL, scale = TRUE, covar_col = NA) {
  ## Checking inputs
  #checking inputs
  if (is.null(df)) {
    stop("Please provide a data frame (that includes PRS values with at least
         columns individual_id, PRS, Phenotype)")
  } else if (ncol(df)<3) {
    stop("Please provide a data frame (that includes at least 3 columns such
         as individual_id, PRS, Phenotype)")
  } else if(is.null(assoc_table)) {
    stop("Please provide a data frame or a matrix for assoc_table parameter")
  } else if (ncol(assoc_table) != 2) {
    stop("Please provide for assoc_table a data frame or a matrix with 2
           columns representing PRS and Phenotype (in this order)")
  } else {
    n_assoc <- nrow(assoc_table)
    if (n_assoc == 1) {
      warning("Only one association given, preferably use assoc() function")
    }
  }
  cat("\n\n------\nMultiple associations (",n_assoc,") testing:")

  ##Creating progress bar
  cat("\n")
  progress = txtProgressBar(min = 0, max = n_assoc, initial = 0, style = 3)

  ##Creating the score table
  scores_table <- data.frame(matrix(nrow = 0, ncol = 11))
  names(scores_table) <- c('PRS','Phenotype','Covar','N_cases','N_controls',
                           'N','OR','SE','lower_CI','upper_CI','P_value')

  ## QC assoc table
  names(assoc_table) <- c("PRS","Phenotype")


  ## For loop of assoc function
  for (i in 1:n_assoc) {
    scores_table <- rbind(scores_table, assoc(df = df, prs_col = as.character(assoc_table[i,1]),
                                              phenotype_col = as.character(assoc_table[i,2]),
                                              scale = scale, covar_col =
                                                covar_col))

    cat("\n")
    setTxtProgressBar(progress,i)
  }

  #returning the result
  return(scores_table)

}
