test_that("Missing assoc_table", {
  expect_error(multiassoc(df = comorbidExample))
})

test_that("Wrong assoc_table", {
  expect_error(multiassoc(df = comorbidExample, assoc_table = "WRONG_ASSOC_TABLE"))
})

test_that("Wrong assoc_table", {
  expect_error(multiassoc(df = comorbidExample, assoc_table = "WRONG_ASSOC_TABLE"))
})

test_that("Not enough column for assoc_table", {
  expect_error(multiassoc(df = comorbidExample, assoc_table = matrix(NA, nrow = 2, ncol = 1)))
})

test_that("Only one association", {
  expect_warning(multiassoc(df = comorbidExample, assoc_table = data.frame(
    PRS = "PRS_1",
    Phenotype = "Phenotype_1"
  )))
})

prs <- grep("PRS", names(comorbidExample), value = T)
phenotype <- grep("Phenotype", names(comorbidExample), value = T)
assoc <- cbind(prs, phenotype)
assoc <- na.omit(assoc)

test_that("Null log", {
  expect_error(multiassoc(df = comorbidExample, assoc_table = assoc, log = NULL))
})

test_that("Wrong log", {
  expect_error(multiassoc(df = comorbidExample, assoc_table = assoc, log = 1))
})

test_that("Test of several PRS and Phenotype using an assoc_table matrix and covariates", {
  expect_s3_class(
    object = multiassoc(df = comorbidExample, assoc_table = assoc, covar_col = c("Age", "Sex", "Covariate")),
    class = "data.frame"
  )
})
