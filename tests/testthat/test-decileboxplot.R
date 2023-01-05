test_that('Cases/Controls Phenotype', {
  expect_error(decileboxplot(df = comorbidExample, prs_col = "PRS_1",
                           phenotype_col = "Phenotype_2"))
})

test_that('Categorical Phenotype', {
  expect_error(decileboxplot(df = comorbidExample, prs_col = "PRS_2",
                             phenotype_col = "Phenotype_4"))
})

test_that('Not enough sample/individual warning for centiles', {
  expect_warning(decileboxplot(df = comorbidExample[1:500,], prs_col = "PRS_1",
                             phenotype_col = "Phenotype_1"))
})

test_that('Test of decileboxplot using a Continuous Phenotype from comorbidExample', {
  expect_s3_class(
    object = decileboxplot(df = comorbidExample, prs_col = "PRS_1",
                         phenotype_col = "Phenotype_1"),
    class = "ggplot")
})
