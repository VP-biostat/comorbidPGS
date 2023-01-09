test_that("Wrong threshold", {
  expect_warning(densityplot(
    df = comorbidExample, prs_col = "PRS_1",
    phenotype_col = "Phenotype_1",
    threshold = "WRONG_THRESHOLD"
  ))
})

for (scaling in c(T, F)) {
  test_that(paste("Test with scaling =", scaling, "; threshold = NA ; Cases/Controls Phenotype"), {
    expect_s3_class(
      object = densityplot(
        df = comorbidExample, prs_col = "PRS_1",
        phenotype_col = "Phenotype_2",
        scale = scaling
      ),
      class = "ggplot"
    )
  })

  test_that(paste("Test with scaling =", scaling, "; threshold = NA ; Categorical Phenotype"), {
    expect_s3_class(
      object = densityplot(
        df = comorbidExample, prs_col = "PRS_1",
        phenotype_col = "Phenotype_4",
        scale = scaling
      ),
      class = "ggplot"
    )
  })

  test_that(paste("Test with scaling =", scaling, "; threshold = 1.5 ; Continuous Phenotype"), {
    expect_s3_class(
      object = densityplot(
        df = comorbidExample, prs_col = "PRS_1",
        phenotype_col = "Phenotype_1",
        scale = scaling, threshold = 1.5
      ),
      class = "ggplot"
    )
  })
}
