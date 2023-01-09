test_that("Only one Phenotype", {
  expect_warning(multiphenassoc(df = comorbidExample, prs_col = "PRS_1", phenotype_col = "Phenotype_1"))
})

prs <- grep("PRS", names(comorbidExample), value = T)[1]
phenotype <- grep("Phenotype", names(comorbidExample), value = T)

test_that("Test of first PRS and several Phenotypes using an assoc_table matrix and covariates", {
  expect_s3_class(
    object = multiphenassoc(df = comorbidExample, prs_col = prs, phenotype_col = phenotype, covar_col = c("Age", "Sex", "Covariate")),
    class = "data.frame"
  )
})
