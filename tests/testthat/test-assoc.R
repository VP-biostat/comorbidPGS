test_that('Missing df', {
  expect_error(assoc())
})

test_that('Wrong df', {
  expect_error(assoc(df = "WRONG_DF"))
})

test_that('Insufficient number of Column df', {
  expect_error(assoc(df = data.frame(matrix(NA, nrow = 10, ncol = 2))))
})

test_that('Null prs_col', {
  expect_warning(assoc(df = comorbidExample, prs_col = NULL, phenotype_col = "Phenotype_1"))
})

test_that('NA prs_col', {
  expect_warning(assoc(df = comorbidExample, prs_col = NA, phenotype_col = "Phenotype_1"))
})


test_that('Wrong prs_col', {
  expect_warning(assoc(df = comorbidExample, prs_col = "WRONG_PRS", phenotype_col = "Phenotype_1"))
})

test_that('Null phenotype_col', {
  expect_warning(assoc(df = comorbidExample, prs_col = "PRS_1", phenotype_col = NULL))
})

test_that('NA phenotype_col', {
  expect_warning(assoc(df = comorbidExample, prs_col = "PRS_1", phenotype_col = NA))
})

test_that('Wrong phenotype_col', {
  expect_warning(assoc(df = comorbidExample, prs_col = "PRS_1", phenotype_col = "WRONG_PHENOTYPE"))
})

test_that('Wrong scale', {
  expect_error(assoc(df = comorbidExample, prs_col = "PRS_1", phenotype_col = "Phenotype_1", scale = "WRONG_SCALE"))
})

test_that('Wrong Phenotype: only one value', {
  expect_error(assoc(df = cbind(comorbidExample, data.frame("WRONG_PHENO" = rep(0, nrow(comorbidExample)))), prs_col = "PRS_1", phenotype_col = "WRONG_PHENO"))
})

test_that('Wrong covar_col', {
  expect_error(assoc(df = comorbidExample, prs_col = "PRS_1", phenotype_col = "Phenotype_1", scale = "WRONG_COVARIATE"))
})

test_that('Not enough sample/individuals', {
  expect_error(assoc(df = cbind(comorbidExample, data.frame("WRONG_PHENO" = rep(NA, nrow(comorbidExample)))), prs_col = "PRS_1", phenotype_col = "WRONG_PHENO"))
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
