#' @title
#' Check the format of the data frame
#'
#' @param df a dataframe
#'
#' @return proper prs_col and phenotype_col
#'
#' @noRd
df_checker <- function(df = NULL, prs_col = NA, phenotype_col = NA, scale = NA,
                       covar_col = NA) {
  ## Checking df object
  if (is.null(df)) {
    stop("Please provide for 'df' a data frame (with at least 3 columns:
    ID, PRS and a continuous or discrete Phenotype)")
  } else if (!(class(df)[1] %in% c("data.frame"))) {
    stop("Please provide for 'df' a data frame (with at least 3 columns:
    ID, PRS and a continuous or discrete Phenotype)")
  } else if (ncol(df) < 3) {
    stop("Please provide for 'df' a data frame with at least 3 columns:
    ID, PRS and a continuous or discrete Phenotype")
  } else if (!is.logical(scale)) {
    stop("Please provide a logical for 'scale' (TRUE by default)")
  } else if (nrow(unique(df)) != nrow(df)) {
    warning("Duplicate(s) found in df! Removing them")
    df <- unique(df)
  }



  ## Checking what is in the data frame df
  # if no SCORESUM column found, assume 2nd column is PRS
  if (is.null(prs_col)) {
    warning("Missing prs_col, using by default the second column of df")
    prs_col <- names(df)[2]
  } else if (is.na(prs_col)) {
    warning("Missing prs_col, using by default the second column of df")
    prs_col <- names(df)[2]
  } else if (!prs_col %in% names(df)) {
    warning("Wrong prs_col, using by default the second column of df")
    prs_col <- names(df)[2]
  } else if (!class(df[, prs_col]) %in% c("numeric", "integer", "double")) {
    stop("Please provide numeric values in the PRS column")
  }
  # if no Phenotype column found, assume 3rd column is PRS
  if (is.null(phenotype_col)) {
    warning("Missing phenotype_col, using by default the third column of df")
    phenotype_col <- names(df)[3]
  } else if (is.na(phenotype_col)) {
    warning("Missing phenotype_col, using by default the third column of df")
    phenotype_col <- names(df)[3]
  } else if (!phenotype_col %in% names(df)) {
    warning("Wrong phenotype_col, using by default the third column of df")
    phenotype_col <- names(df)[3]
  }
  # if no Covariate column found, assume covar_col is NA
  if (is.null(covar_col[1]) | is.na(covar_col[1])) {
    covar_col <- NA
  } else {
    for (covar in covar_col) {
      if (!covar %in% names(df)) {
        stop(paste("Wrong covar_col provided,", covar, "does not exist"))
      }
    }
  }

  return(list("prs_col" = prs_col, "phenotype_col" = phenotype_col))
}
