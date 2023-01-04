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
#'   \item{Phenotype_1}{A continuous Phenotype associated to PRS_1}
#'   \item{Phenotype_2}{A cases/controls Phenotype associated to PRS_1}
#'   \item{Phenotype_3}{A cases/controls Phenotype associated to PRS_2}
#'   \item{Phenotype_4}{A categorical Phenotype associated to PRS_2}
#'   \item{Sex, Age, Covariate}{Three potential Covariates}
#'   ...
#' }
#' @source <https://github.com/VP-biostat/comorbidPRS>
"comorbidExample"
