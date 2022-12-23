#' Fake dataset for comorbidPRS package
#'
#' A dataset with sets of PRS, Phenotype and Covariate to demo the
#' comorbidPRS package
#'
#' @format ## `who`
#' A data frame with 50,000 rows (individuals) and 8 columns:
#' \describe{
#'   \item{ID}{Individual's identifier}
#'   \item{PRS_1, PRS_2}{Two distributions of PRS}
#'   \item{Phenotype_1, Phenotype_2}{Two Cases/Controls Phenotypes}
#'   \item{sex, age, Covariate}{Tree potential Covariates}
#'   ...
#' }
#' @source <https://github.com/VP-biostat/comorbidPRS>
"comorbidExample"
