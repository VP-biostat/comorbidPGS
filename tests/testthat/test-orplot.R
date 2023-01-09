test_that("Missing score_table", {
  expect_error(orplot())
})

test_that("Wrong class score_table", {
  expect_error(orplot(score_table = "WRONG_DF"))
})

prs <- grep("PRS", names(comorbidExample), value = T)
phenotype <- grep("Phenotype", names(comorbidExample), value = T)
assoc <- cbind(prs, phenotype)
assoc <- na.omit(assoc)
score_table <- invisible(multiassoc(comorbidExample, assoc))

# 'PRS'	'Phenotype'  'OR'	'lower_CI'	'upper_CI'	'P_value'

test_that("No column Phenotype in score_table", {
  expect_error(orplot(score_table[, -"Phenotype"]))
})

test_that("No column OR in score_table", {
  expect_error(orplot(score_table[, -"OR"]))
})

test_that("No column lower_CI in score_table", {
  expect_error(orplot(score_table[, -"lower_CI"]))
})

test_that("No column upper_CI in score_table", {
  expect_error(orplot(score_table[, -"upper_CI"]))
})

test_that("No column PRS in score_table", {
  expect_error(orplot(score_table[, -"PRS"]))
})

test_that("No column P-value in score_table", {
  expect_error(orplot(score_table[, -"P-value"]))
})

test_that("Missing axis", {
  expect_warning(orplot(score_table, axis = NULL))
})

test_that("Wrong axis", {
  expect_warning(orplot(score_table, axis = "WRONG_AXIS"))
})

test_that("Missing pval", {
  expect_error(orplot(score_table, pval = NULL))
})

test_that("Wrong pval", {
  expect_error(orplot(score_table, pval = "WRONG_PVAL"))
})

for (ax in c("horizontal", "vertical")) {
  for (pvalue in c(0.05, 0.5, T, F)) {
    test_that(paste("Test with axis =", ax, "; pval =", pvalue), {
      expect_s3_class(
        object = orplot(score_table, ax, pvalue),
        class = "ggplot"
      )
    })
  }
}
