#' comorbidExample

#number of samples
n <- 50000
#random seed
set.seed(7)

#add ID column
id <- paste0("ind_", seq(1:n))

#build a fake PRS with mean of 4, sd of 3
prs_1 <- rnorm(50000, mean = 4, sd = 3)
#build a fake PRS with mean of 6, sd of 1
prs_2 <- rnorm(50000, mean = 6, sd = 1)

#build a continuous phenotype associated with prs_1
#it is a bernoulli trial with an increase chance when having high PRS
phenotype_1 <- (sample(c(0,2), 50000, replace = T)+prs_1/max(prs_1))
#build a cases/controls phenotype associated with prs_1 using binomial law
phenotype_2 <- rbinom(50000, 1, (prs_1-min(prs_1))/(max(prs_1)-min(prs_1)))
#build a cases/controls phenotype associated with prs_2 using binomial law
phenotype_3 <- rbinom(50000, 1, (prs_2-min(prs_2))/(max(prs_2)-min(prs_2)))
phenotype_3 <- as.logical(phenotype_3)
#build a categorical phenotype associated with prs_2 using 3 summed binomial laws
phenotype_4_int <- rbinom(50000, 1, (prs_2-min(prs_2))/(max(prs_2)-min(prs_2))) +
  rbinom(50000, 1, (prs_2-min(prs_2))/(max(prs_2)-min(prs_2))) +
  rbinom(50000, 1, (prs_2-min(prs_2))/(max(prs_2)-min(prs_2)))
phenotype_4 <- factor(phenotype_4_int,levels = 0:3, label = c("A","B","C","D"))

#add covariates
sex <- sample(c(0,1), 50000, replace = T)
age <- sample(c(25, 70), 50000, replace = T)
covar <- rnorm(50000, mean = 0, sd = 10)

#gather them all in a data.frame
comorbidExample <- data.frame(ID = id,
                              PRS_1 = prs_1,
                              PRS_2 = prs_2,
                              Phenotype_1 = phenotype_1,
                              Phenotype_2 = phenotype_2,
                              Phenotype_3 = phenotype_3,
                              Phenotype_4 = phenotype_4,
                              Sex = sex,
                              Age = age,
                              Covariate = covar)

usethis::use_data(comorbidExample, overwrite = T)

