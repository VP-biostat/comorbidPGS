test_that('Missing df', {
  expect_error(assoc())
})

test_that('Null prs_col', {
  expect_warning(assoc(df = comorbidExample, prs_col = NULL, phenotype_col = "Phenotype_1"))
})

test_that('Wrong prs_col', {
  expect_warning(assoc(df = comorbidExample, prs_col = "WRONG_PRS", phenotype_col = "Phenotype_1"))
})

test_that('Null phenotype_col', {
  expect_warning(assoc(df = comorbidExample, prs_col = "PRS_1", phenotype_col = NULL))
})

test_that('Wrong phenotype_col', {
  expect_warning(assoc(df = comorbidExample, prs_col = "PRS_1", phenotype_col = "WRONG_PHENOTYPE"))
})

test_that('Wrong scale', {
  expect_error(assoc(df = comorbidExample, prs_col = "PRS_1", phenotype_col = "Phenotype_1", scale = "WRONG_SCALE"))
})

test_that('Wrong covar_col', {
  expect_error(assoc(df = comorbidExample, prs_col = "PRS_1", phenotype_col = "Phenotype_1", scale = "WRONG_COVARIATE"))
})

for (prs in grep("PRS",names(comorbidExample), value = T)) {
  for (phenotype in grep("Phenotype",names(comorbidExample), value = T)) {

    test_that(paste("Test of", prs, "on", phenotype), {
      expect_s3_class(
        object = assoc(df = comorbidExample, prs_col = prs, phenotype_col = phenotype),
        class = "data.frame"
      )
    })

  }
}
