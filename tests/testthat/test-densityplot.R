test_that('Wrong threshold', {
  expect_warning(densityplot(df = comorbidExample, prs_col = "PRS_1",
                           phenotype_col = "Phenotype_1",
                           threshold = "WRONG_THRESHOLD"))
})

for (scaling in c(T, F)) {
  for (thres in c(NA, 1.5)) {

    test_that(paste("Test with scaling =",scaling,"; threshold =",thres), {
      expect_s3_class(
        object = densityplot(df = comorbidExample, prs_col = "PRS_1",
                             phenotype_col = ifelse(!is.na(thres), "Phenotype_1", "Phenotype_2"),
                             scale = scaling, threshold = thres
        ),
        class = "ggplot"
      )
    })

  }
}
