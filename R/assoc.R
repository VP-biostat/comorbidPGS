#' take a distribution of PRS and a Phenotype, the name of the analysis and eventual confounders
#' return a data frame showing the regression of the PRS on the Phenotype
#'
#' @param df a dataframe with individuals on each row, at least one column PRS
#'(continuous variable) and one with phenotype (continuous or categorical)
#' @param prs_col a character specifying the PRS column name
#' @param phenotype_col a character specifying the Phenotype column name
#' @param scale a boolean specifying if scaling of PRS should be done before testing
#' @param covar_col a character vector specifying the covariate column names (facultative)
#'
#' @return return a data frame showing the regression of the PRS on the Phenotype
#' with 'PRS','Phenotype','N_cases','N_controls','N','OR','SE','lower_CI','upper_CI','P_value'
#' @import data.table
#' @importFrom stats na.omit
#' @export
assoc <- function(df = NA, prs_col = "SCORESUM", phenotype_col = "Phenotype",
                  scale = TRUE, covar_col = NA) {
  cat("\nComorbidPRS - Launching association testing...\n")
  #checking inputs
  if (is.na(df)) {
    stop("Error: Please provide a data frame (that includes PRS values with at least
         columns individual_id, PRS, Phenotype)")
  } else if (ncol(df<3)) {
    stop("Error: Please provide a data frame (that includes at least 3 columns such
         as individual_id, PRS, Phenotype)")
  }


  ## Checking what is in the data frame df
  #if no SCORESUM column found, assume 2nd column is PRS
  if (!prs_col %in% names(df)) {
    prs_col <- names(df)[2]
  }
  #if no Phenotype column found, assume 3rd column is PRS
  if (!phenotype_col %in% names(df)) {
    phenotype_col <- names(df)[3]
  }
  #find id column name and store it, it  is mandatory to be the 1st line
  id_col <- names(df)[1]
  #communicate the columns selected
  cat("\nID column selected: ",id_col)
  cat("\nPRS column selected: ",prs_col)
  cat("\nPhenotype column selected: ",phenotype_col)
  cat("\nCovariate: ",covar_col)


  ## QCing df
  df <- df[,c(id_col,prs_col,phenotype_col)] #cropping the dataset to only 3 columns
  df <- na.omit(df) #excluding rows with NAs
  if (scale) {
    df[,prs_col] <- scale(df[,prs_col]) #scaling if scale = T
  }


  ## Doing regression
  #check Phenotype continuous or discrete aspect
  if (length(unique(df[,phenotype_col])) < 2) {
    stop("Error: Phenotype column have less than 2 values")
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
  cat("\nPhenotype type: ",phenotype_type)

  #create the regression formula based on phenotype, prs and covariate(s)
  if (length(covar_col) > 1 & !is.na(covar_col[1])) {
    #create the regression formula
    regress_string <- paste0(phenotype_col,"~",prs_col)
    for (covar in covar_col) {
      regress_string <- paste0(regress_string,"+",covar)
    }
    regress_formula <- formula(regress_string)
  } else {
    #create the regression formula
    regress_formula <- paste0(phenotype_col,"~",prs_col)
  }

  #doing regression according to phenotype type
  if (phenotype_type == "Cases/Controls") {
    regress <- glm(regress_formula, family="binomial"(link="logit"), data=df)
  } else if (phenotype_type == "Categorical") {
    regress <- glm(regress_formula, data=df)
  } else if (phenotype_type == "Continuous") {
    regress <- lm(regress_formula, data=df)
  }


  ## Wrapping up the results
  cases <- NA
  controls <- NA
  if (phenotype_type == "Cases/Controls") {
    cases <- sum(as.logical(df[,phenotype_col]))
    controls <- sum(as.logical(df[,phenotype_col]))
    cat("\nCases: ",cases)
    cat("\nControls: ",controls)
  }
  sample_size <- nrow(df)
  cat("\nSample Size: ", sample_size)
  beta <- coef(summary(regress))[2,1]
  se <- coef(summary(regress))[2,2]
  p_val <- coef(summary(regress))[2,4]
  or <- exp(beta)
  lower_ci <- exp(confint(regress))[2,1]
  upper_ci <- exp(confint(regress))[2,2]
  cat("OR (95% CI): ",or," (",lower_ci,"-",upper_ci,")")
  cat("P-value: ",p_val)

  #creating the score_table
  score_table <- data.frame("PRS" = prs_col, "Phenotype" = phenotype_col,
                            "N_cases" = cases, "N_controls" = controls,
                            "N" = sample_size, "OR" = or, "SE" = se,
                            "lower_CI" = lower_ci, "upper_CI" = upper_ci,
                            "P_value" = p_val)

  #returning the result
  return(score_table)

}
