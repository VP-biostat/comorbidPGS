#' @title
#' Association of a PRS distribution with a Phenotype
#'
#' @description
#' `assoc()` take a distribution of PRS, a Phenotype and eventual Confounders
#' return a data frame showing the association of PRS on the Phenotype
#'
#' @param df a dataframe with individuals on each row, and at least the following
#' columns:
#'
#'  * one ID column,
#'  * one PRS column, with numerical continuous values following a normal distribution,
#'  * one Phenotype column, can be numeric (Continuous Phenotype), character, boolean or factors (Discrete Phenotype)
#' @param prs_col a character specifying the PRS column name
#' @param phenotype_col a character specifying the Phenotype column name
#' @param scale a boolean specifying if scaling of PRS should be done before testing
#' @param covar_col a character vector specifying the covariate column names (facultative)
#' @param log a connection, or a character string naming the file to print to.
#' If "" (by default), it prints to the standard output connection, the console unless redirected by sink.
#'
#' @return return a data frame showing the association of the PRS on the Phenotype
#' with the following columns:
#'
#' * PRS: the name of the PRS
#' * Phenotype: the name of Phenotype
#' * Phenotype_Type: either 'Continuous', 'Categorical' or 'Cases/Controls'
#' * Covar: list all the covariates used for this association
#' * N_cases: if Phenotype_Type is Cases/Controls, gives the number of cases
#' * N_controls: if Phenotype_Type is Cases/Controls, gives the number of controls
#' * N: the number of individuals/samples
#' * Effect: if Phenotype_Type is Continuous, it represents the Beta coefficient of linear regression, OR of logistic regression otherwise
#' * SE: standard error of the related Effect (Beta or OR)
#' * lower_CI: lower confidence interval of the related Effect (Beta or OR)
#' * upper_CI: upper confidence interval of the related Effect (Beta or OR)
#' * P_value: associated P-value
#'
#' @importFrom stats na.omit glm binomial lm coef confint median
#' @importFrom MASS polr
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
    cat("\n   Using a binary logistic regression", file = log, append = T)
    regress <- glm(regress_formula, family = "binomial"(link = "logit"), data = df)
  } else if (phenotype_type == "Categorical") {
    cat("\n   Using an ordinal logistic regression", file = log, append = T)
    regress <- polr(regress_formula, method = c("logistic"), data = df, Hess = T)
  } else if (phenotype_type == "Continuous") {
    cat("\n   Using a linear regression", file = log, append = T)
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
  ctable <- coef(summary(regress))
  if (phenotype_type == "Categorical") {
    # adding pval
    ptemp <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = ptemp)
  }
  beta_or <- ifelse((phenotype_type == "Continuous"), ctable[2, 1], exp(ctable[2, 1]))
  se <- ctable[2, 2]
  p_val <- ctable[2, 4]
  if (phenotype_type == "Continuous") {
    ci <- suppressMessages(confint(regress))
  } else {
    ci <- suppressMessages(exp(confint(regress)))
  }
  lower_ci <- ci[2, 1]
  upper_ci <- ci[2, 2]
  if (phenotype_type == "Continuous") {
    cat("\n   Beta ( 95% CI ): ", beta_or, " (", lower_ci, "-", upper_ci, ")", file = log, append = T)
  } else {
    cat("\n   OR ( 95% CI ): ", beta_or, " (", lower_ci, "-", upper_ci, ")", file = log, append = T)
  }
  cat("\n   P-value: ", p_val, "\n", file = log, append = T)

  # creating the score_table
  score_table <- data.frame(
    "PRS" = prs_col, "Phenotype" = phenotype_col,
    "Phenotype_Type" = phenotype_type,
    "Covar" = paste(covar_col, collapse = "+"),
    "N_cases" = cases, "N_controls" = controls,
    "N" = sample_size, "Effect" = beta_or, "SE" = se,
    "lower_CI" = lower_ci, "upper_CI" = upper_ci,
    "P_value" = p_val
  )

  # returning the result
  return(score_table)
}
