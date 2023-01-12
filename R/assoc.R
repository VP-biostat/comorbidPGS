#' @title
#' Association of a PRS distribution with a Phenotype
#'
#' @description
#' `assoc()` take a distribution of PRS, a Phenotype and eventual Confounders
#' return a data frame showing the association of PRS on the Phenotype
#'
#' @param df a dataframe with individuals on each row, at least one ID column,
#' one column PRS (continuous variable) and one with phenotype (continuous or discrete)
#' @param prs_col a character specifying the PRS column name
#' @param phenotype_col a character specifying the Phenotype column name
#' @param scale a boolean specifying if scaling of PRS should be done before testing
#' @param covar_col a character vector specifying the covariate column names (facultative)
#' @param log 	a connection, or a character string naming the file to print to.
#' If "" (by default), it prints to the standard output connection, the console unless redirected by sink.
#'
#' @return return a data frame showing the association of the PRS on the Phenotype
#' with 'PRS','Phenotype','Covar','N_cases','N_controls','N','OR','SE','lower_CI','upper_CI','P_value'
#' @importFrom stats na.omit glm binomial lm coef confint median
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
assoc <- function(df = NULL, prs_col = "SCORESUM", phenotype_col = "Phenotype",
                  scale = TRUE, covar_col = NA, log = "") {
  ## Checking inputs
  col_names <- df_checker(df, prs_col, phenotype_col, scale, covar_col)
  prs_col <- col_names$prs_col
  phenotype_col <- col_names$phenotype_col
  if (!is.logical(scale)) {
    stop("Please provide a logical for 'scale' (TRUE by default)")
  } else if (!class(log)[1] %in% c("character","url","connection")) {
    stop("Please provide a connection, or a character string naming the file to print to for 'log'")
  }

  cat("\n\n---\nAssociation testing:", file = log, append = T)

  # communicate the columns selected
  cat("\n  PRS: ", prs_col, file = log, append = T)
  cat("\n  Phenotype: ", phenotype_col, file = log, append = T)
  cat("\n  Covariate: ", covar_col, file = log, append = T)


  ## QCing df
  if (is.na(covar_col[1])) {
    df <- df[, c(prs_col, phenotype_col)] # cropping the dataset to only 2 columns
  } else {
    df <- df[, c(prs_col, phenotype_col, covar_col)] # cropping the dataset to 2+(covar length) columns
  }
  df <- na.omit(df) # excluding rows with NAs
  if (scale) {
    df[, prs_col] <- scale(df[, prs_col]) # scaling if scale = T
  }
  if (nrow(df) < 2) {
    stop("After NA removal, not enough samples/individuals to test")
  }


  ## Doing regression
  # check Phenotype continuous or discrete aspect
  phenotype_type <- phenotype_type(df = df, phenotype_col = phenotype_col)
  cat("\n  Phenotype type: ", phenotype_type, file = log, append = T)

  # create the regression formula based on phenotype, prs and covariate(s)
  if (length(covar_col) > 1 & !is.na(covar_col[1])) {
    # create the regression formula
    regress_formula <- paste(phenotype_col, "~", prs_col)
    for (covar in covar_col) {
      regress_formula <- paste(regress_formula, "+", covar)
    }
  } else {
    # create the regression formula
    regress_formula <- paste(phenotype_col, "~", prs_col)
  }
  cat("\n   ", regress_formula, file = log, append = T)

  # doing regression according to phenotype type
  if (phenotype_type == "Cases/Controls") {
    regress <- glm(regress_formula, family = "binomial"(link = "logit"), data = df)
  } else if (phenotype_type == "Categorical") {
    cat("\n   Phenotype is categorical, 'cases' are interpreted as the factor not having the first level", file = log, append = T)
    regress <- glm(regress_formula, family = "binomial"(link = "logit"), data = df)
  } else if (phenotype_type == "Continuous") {
    regress <- lm(regress_formula, data = df)
  }


  ## Wrapping up the results
  cases <- NA
  controls <- NA
  if (phenotype_type == "Cases/Controls") {
    cases <- sum(as.logical(df[, phenotype_col]) == T)
    controls <- sum(as.logical(df[, phenotype_col]) == F)
    cat("\n  Cases: ", cases, file = log, append = T)
    cat("\n  Controls: ", controls, file = log, append = T)
  }
  sample_size <- nrow(df)
  cat("\n  Sample Size: ", sample_size, file = log, append = T)
  beta <- coef(summary(regress))[2, 1]
  se <- coef(summary(regress))[2, 2]
  p_val <- coef(summary(regress))[2, 4]
  or <- exp(beta)
  ci <- suppressMessages(exp(confint(regress)))
  lower_ci <- ci[2, 1]
  upper_ci <- ci[2, 2]
  cat("\n   OR ( 95% CI ): ", or, " (", lower_ci, "-", upper_ci, ")", file = log, append = T)
  cat("\n   P-value: ", p_val, "\n", file = log, append = T)

  # creating the score_table
  score_table <- data.frame(
    "PRS" = prs_col, "Phenotype" = phenotype_col,
    "Covar" = paste(covar_col, collapse = "+"),
    "N_cases" = cases, "N_controls" = controls,
    "N" = sample_size, "OR" = or, "SE" = se,
    "lower_CI" = lower_ci, "upper_CI" = upper_ci,
    "P_value" = p_val
  )

  # returning the result
  return(score_table)
}
