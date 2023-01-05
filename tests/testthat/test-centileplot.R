test_that('Wrong continuous_metric', {
  expect_error(centileplot(df = comorbidExample, prs_col = "PRS_1",
                           phenotype_col = "Phenotype_2",
                           continuous_metric = "WRONG_CONTINUOUS_METRIC"))
})

test_that('Not enough sample/individual warning for centiles', {
  expect_warning(centileplot(df = comorbidExample[1:500,], prs_col = "PRS_1",
                             phenotype_col = "Phenotype_1",
                             decile = F))
})

test_that('Categorical Phenotype', {
  expect_error(centileplot(df = comorbidExample, prs_col = "PRS_2",
                             phenotype_col = "Phenotype_4"))
})

for (deciling in c(T, F)) {
  for (cont_met in c(NA, 'mean', 'median')) {

    test_that(paste("Test with centile =",deciling,"; continuous_metric =",cont_met), {
      expect_s3_class(
        object = centileplot(df = comorbidExample, prs_col = "PRS_1",
                       phenotype_col = ifelse(!is.na(cont_met), "Phenotype_1", "Phenotype_2"),
                       decile = deciling, continuous_metric = cont_met
        ),
        class = "ggplot"
      )
    })

  }
}
