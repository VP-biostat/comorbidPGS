#' take a distribution of PRS and a Phenotype, the name of the analysis and eventual confounders
#' return a data frame showing the regression of the PRS on the Phenotype
#' PRS_snps	Phenotype	Sex_specificity	N_cases	N_controls	OR	lower_CI	upper_CI	P_value
#'
#' @param x un vecteur numerique
#'
#' @return la fonction renvoie la moyenne d'un vecteur
#' @import data.table
#' @export
assoc_testing <- function(file = NA, outcome = NA, pheno = pheno, prsice = F, double_reg = F) {
  if (is.na(file)|is.na(outcome)) {
    print("Please provide a file name (that includes PRS values with at least columns FID, IID, PRS) and an outcome name (without extension)")
    next
  }
  print('----------------------------------------')
  print(paste('Working now on',file,'...'))
  # setTxtProgressBar(pb,j)

  #create a score table with 9 column and len of profile files (+n that needs to be adjusted if we want to do association sex_specific with brc and prc)
  scores_table <- data.frame(matrix(nrow = 1+as.numeric(double_reg),ncol = 9))
  colnames(scores_table) <- c('PRS_snps','Outcome','Sex_specificity','N_cases','N_controls','OR','lower_CI','upper_CI','P_value')

  scores <- read.table(file, header=TRUE, sep="",stringsAsFactors = F)
  #remove "missing" ind
  scores <- scores[which(!grepl(pattern = "missing", x = scores$FID)), ]
  if (prsice) scores$SCORESUM_norm <- scale(scores$PRS)
  else scores$SCORESUM_norm <- scale(scores$SCORESUM)

  # merge em
  df <- merge(scores, pheno, by.x = "IID", by.y = "gen_id1") # used IID as best way to merge them all, gen_id1 used for own computation, cancer_id2 used for t2d_prs from lz
  df <- df[!duplicated(df$IID), ]

  # check the number of cases and controls & choose the right column of pheno using the scores name
  if (grepl('pros_adj_betas',outcome)|grepl('_prc_',outcome)) {

    double_reg <- TRUE

    # writing outcome in final table
    scores_table$Outcome[i] <- "Prostate Cancer"
    scores_table$Outcome[i+1] <- "Prostate Cancer"

    # exclude all rows where phenotype column is NA
    df$prc <- as.numeric(df$prc)
    df <- df[-which(df$bc == 1 | df$panc == 1 | df$crc == 1 | df$lung2 == 1), ]
    df <- df[!is.na(df$prc), ]
    # df$prc_male <- as.numeric(df$prc_male)
    df2 <- df[!is.na(df$prc)&(df$sex == 0), ]

    cases <- sum(df$prc == 1,rm.na=TRUE)
    controls <- sum(df$prc == 0,rm.na=TRUE)
    scores_table$N_cases[i] <- cases
    scores_table$N_controls[i] <- controls
    print(paste('There is',scores_table$N_cases[i],'prc cases'))
    print(paste('There is',scores_table$N_controls[i],'prc controls'))
    scores_table$Sex_specificity[i] <- 'all'

    cases_2 <- sum(df2$prc == 1,rm.na=TRUE)
    controls_2 <- sum(df2$prc == 0,rm.na=TRUE)
    scores_table$N_cases[i+1] <- cases_2
    scores_table$N_controls[i+1] <- controls_2
    print(paste('There is',scores_table$N_cases[i+1],'prc_male cases'))
    print(paste('There is',scores_table$N_controls[i+1],'prc_male controls'))
    scores_table$Sex_specificity[i+1] <- 'male'

    m1 <- glm(prc ~ SCORESUM_norm+array+age+PC1+PC2+PC3+PC4+PC5+PC6, family="binomial"(link="logit"), data=df)
    m2 <- glm(prc ~ SCORESUM_norm, family="binomial"(link="logit"), data=df2)
  } else if (grepl('brea_adj_betas',outcome)|grepl('_brc_',outcome)) {

    double_reg <- TRUE

    # writing outcome in final table
    scores_table$Outcome[i] <- "Breast Cancer"
    scores_table$Outcome[i+1] <- "Breast Cancer"

    # exclude all rows where phenotype column is NA
    df$bc <- as.numeric(df$bc)
    # df$bc_fem <- as.numeric(df$bc_fem)
    df <- df[-which(df$prc == 1 | df$crc == 1 | df$panc == 1 | df$lung2 == 1), ]
    df <- df[!is.na(df$bc),]
    df2 <- df[!is.na(df$bc)&(df$sex == 1), ]

    cases <- sum(df$bc == 1,rm.na=TRUE)
    controls <- sum(df$bc == 0,rm.na=TRUE)
    scores_table$N_cases[i] <- cases
    scores_table$N_controls[i] <- controls
    print(paste('There is',scores_table$N_cases[i],'cases'))
    print(paste('There is',scores_table$N_controls[i],'controls'))
    scores_table$Sex_specificity[i] <- 'all'

    cases_2 <- sum(df2$bc == 1,rm.na=TRUE)
    controls_2 <- sum(df2$bc == 0,rm.na=TRUE)
    scores_table$N_cases[i+1] <- cases_2
    scores_table$N_controls[i+1] <- controls_2
    print(paste('There is',scores_table$N_cases[i+1],'brea_fem cases'))
    print(paste('There is',scores_table$N_controls[i+1],'brea_fem controls'))
    scores_table$Sex_specificity[i+1] <- 'female'

    m1 <- glm(bc ~ SCORESUM_norm+array+age+PC1+PC2+PC3+PC4+PC5+PC6, family="binomial"(link="logit"), data=df)
    m2 <- glm(bc ~ SCORESUM_norm, family="binomial"(link="logit"), data=df2)
  } else if (grepl('_crc_',outcome)) {

    # writing outcome in final table
    scores_table$Outcome[i] <- "Colorectal Cancer"

    # exclude all rows where phenotype column is NA
    df$crc <- as.numeric(df$crc)
    df <- df[-which(df$bc == 1 | df$prc == 1 | df$panc == 1 | df$lung2 == 1), ]
    df <- df[!is.na(df$crc), ]

    cases <- sum(df$crc == 1,rm.na=TRUE)
    controls <- sum(df$crc == 0,rm.na=TRUE)
    scores_table$N_cases[i] <- cases
    scores_table$N_controls[i] <- controls
    print(paste('There is',scores_table$N_cases[i],'cases'))
    print(paste('There is',scores_table$N_controls[i],'controls'))
    scores_table$Sex_specificity[i] <- 'all'

    m1 <- glm(crc ~ SCORESUM_norm+array+age+sex+PC1+PC2+PC3+PC4+PC5+PC6, family="binomial"(link="logit"), data=df)
  } else if (grepl('_panc_',outcome)) {

    # writing outcome in final table
    scores_table$Outcome[i] <- "Pancreatic Cancer"

    # exclude all rows where phenotype column is NA
    df$panc <- as.numeric(df$panc)
    df <- df[-which(df$bc == 1 | df$prc == 1 | df$crc == 1 | df$lung2 == 1), ]
    df <- df[!is.na(df$panc), ]

    cases <- sum(df$panc == 1,rm.na=TRUE)
    controls <- sum(df$panc == 0,rm.na=TRUE)
    scores_table$N_cases[i] <- cases
    scores_table$N_controls[i] <- controls
    print(paste('There is',scores_table$N_cases[i],'cases'))
    print(paste('There is',scores_table$N_controls[i],'controls'))
    scores_table$Sex_specificity[i] <- 'all'

    m1 <- glm(panc ~ SCORESUM_norm+array+age+sex+PC1+PC2+PC3+PC4+PC5+PC6, family="binomial"(link="logit"), data=df)
  } else if (grepl('_lung_',outcome)) {

    # writing outcome in final table
    scores_table$Outcome[i] <- "Lung Cancer"

    # exclude all rows where phenotype column is NA
    df$lung2 <- as.numeric(df$lung2)
    df <- df[!(df$bc == 1 | df$prc == 1 | df$crc == 1 | df$panc == 1), ]
    df <- df[!is.na(df$lung2), ]

    cases <- sum(df$lung2 == 1,rm.na=TRUE)
    controls <- sum(df$lung2 == 0,rm.na=TRUE)
    scores_table$N_cases[i] <- cases
    scores_table$N_controls[i] <- controls
    print(paste('There is',scores_table$N_cases[i],'cases'))
    print(paste('There is',scores_table$N_controls[i],'controls'))
    scores_table$Sex_specificity[i] <- 'all'

    m1 <- glm(lung2 ~ SCORESUM_norm+array+smoking+age+sex+PC1+PC2+PC3+PC4+PC5+PC6, data=df) #adjusted by smoking also
  } else if (grepl('_t2d_',outcome)) {

    # writing outcome in final table
    scores_table$Outcome[i] <- "T2D"

    # exclude all rows where phenotype column is NA
    df$T2D.all <- as.numeric(df$T2D.all)
    # df <- df[!(df$bc == 1 | df$prc == 1 | df$crc == 1), ]
    df <- df[!is.na(df$T2D.all), ]

    cases <- sum(df$T2D.all == 1,rm.na=TRUE)
    controls <- sum(df$T2D.all == 0,rm.na=TRUE)
    scores_table$N_cases[i] <- cases
    scores_table$N_controls[i] <- controls
    print(paste('There is',scores_table$N_cases[i],'cases'))
    print(paste('There is',scores_table$N_controls[i],'controls'))
    scores_table$Sex_specificity[i] <- 'all'

    m1 <- glm(T2D.all ~ SCORESUM_norm+array+age+sex+PC1+PC2+PC3+PC4+PC5+PC6, data=df)
  } else if (grepl('_sbp_',outcome)) {

    # writing outcome in final table
    scores_table$Outcome[i] <- "Systolic Blood Pressure"

    # exclude all rows where phenotype column is NA
    df$sbp_auto <- as.numeric(df$sbp_auto)
    df <- df[!is.na(df$sbp_auto), ]

    scores_table$N_controls[i] <- sum(!is.na(df$sbp_auto))
    print(paste('There is',scores_table$N_controls[i],'controls (NO CASES)'))
    scores_table$Sex_specificity[i] <- 'all'

    m1 <- lm(sbp_auto ~ SCORESUM_norm+array+age+sex+PC1+PC2+PC3+PC4+PC5+PC6, data=df)
  } else if (grepl('_dbp_',outcome)) {

    # writing outcome in final table
    scores_table$Outcome[i] <- "Diastolic Blood Pressure"

    # exclude all rows where phenotype column is NA
    df$dbp_auto <- as.numeric(df$dbp_auto)
    df <- df[!is.na(df$dbp_auto), ]

    scores_table$N_controls[i] <- sum(!is.na(df$dbp_auto))
    print(paste('There is',scores_table$N_controls[i],'controls (NO CASES)'))
    scores_table$Sex_specificity[i] <- 'all'

    m1 <- lm(dbp_auto ~ SCORESUM_norm+array+age+sex+PC1+PC2+PC3+PC4+PC5+PC6, data=df)
  } else if (grepl('_pp_',outcome)) {

    # writing outcome in final table
    scores_table$Outcome[i] <- "Pulse Pressure"

    # exclude all rows where phenotype column is NA
    df$pp_auto <- as.numeric(df$pp_auto)
    df <- df[!is.na(df$pp_auto), ]

    scores_table$N_controls[i] <- sum(!is.na(df$pp_auto))
    print(paste('There is',scores_table$N_controls[i],'controls (NO CASES)'))
    scores_table$Sex_specificity[i] <- 'all'

    m1 <- lm(pp_auto ~ SCORESUM_norm+array+age+sex+PC1+PC2+PC3+PC4+PC5+PC6, data=df)
  }

  print(summary(m1))
  print('Beta value (odds ratio):')
  OR <- exp(coef(m1))[2]
  print(OR)
  scores_table$OR[i] <- OR
  print('2.5 and 97.5% int:')
  CI <- exp(confint(m1))
  print(paste(CI[2,1],CI[2,2]))
  scores_table$lower_CI[i] <- CI[2,1]
  scores_table$upper_CI[i] <- CI[2,2]

  # when p values are below <2e-16, you need to get the precise p-value using this
  print("Exact p-val:")
  pval <- coef(summary(m1))[2,4]
  print(pval)
  scores_table$P_value[i] <- pval

  # plot(m1)

  if (double_reg) {
    print(summary(m2))
    print('2nd Beta value:')
    OR_2 <- exp(coef(m2))[2]
    print(OR_2)
    scores_table$OR[i+1] <- OR_2
    print('2nd 2.5 and 97.5% int:')
    CI_2 <- exp(confint(m2))
    print(paste(CI_2[2],CI_2[4]))
    scores_table$lower_CI[i+1] <- CI_2[2]
    scores_table$upper_CI[i+1] <- CI_2[4]

    # when p values are below <2e-16, you need to get the precise p-value using this
    print("2nd Exact p-val:")
    pval_2 <- coef(summary(m2))[2,4]
    print(pval_2)
    scores_table$P_value[i+1] <- pval_2

    # plot(m2)
  }

  #writing PRS snps in final table
  if (grepl('sbp_UKB',outcome)) scores_table$PRS_snps[i] <- 'SBP'
  if (grepl('dbp_UKB',outcome)) scores_table$PRS_snps[i] <- 'DBP'
  if (grepl('pp_UKB',outcome)) scores_table$PRS_snps[i] <- 'PP'
  if (grepl('sbp_ICBP',outcome)) scores_table$PRS_snps[i] <- 'SBP'
  if (grepl('dbp_ICBP',outcome)) scores_table$PRS_snps[i] <- 'DBP'
  if (grepl('pp_ICBP',outcome)) scores_table$PRS_snps[i] <- 'PP'
  if (grepl('t2d_snps',outcome)) scores_table$PRS_snps[i] <- 'T2D'
  if (grepl('lung_snps',outcome)) scores_table$PRS_snps[i] <- 'Lung cancer'
  if (grepl('prc_269_snps',outcome)) scores_table$PRS_snps[i] <- 'Prostate cancer'
  if (grepl('panc_22_snps',outcome)) scores_table$PRS_snps[i] <- 'Pancreatic cancer'
  if (grepl('crc_143_snps',outcome)) scores_table$PRS_snps[i] <- 'Colorectal cancer'
  if (grepl('brc_212_snps',outcome)) scores_table$PRS_snps[i] <- 'Breast cancer'
  #checking if need to double these line due to double reg
  if (double_reg) {
    if (grepl('sbp_UKB',outcome)) scores_table$PRS_snps[i+1] <- 'SBP'
    if (grepl('dbp_UKB',outcome)) scores_table$PRS_snps[i+1] <- 'DBP'
    if (grepl('pp_UKB',outcome)) scores_table$PRS_snps[i+1] <- 'PP'
    if (grepl('sbp_ICBP',outcome)) scores_table$PRS_snps[i+1] <- 'SBP'
    if (grepl('dbp_ICBP',outcome)) scores_table$PRS_snps[i+1] <- 'DBP'
    if (grepl('pp_ICBP',outcome)) scores_table$PRS_snps[i+1] <- 'PP'
    if (grepl('t2d_snps',outcome)) scores_table$PRS_snps[i+1] <- 'T2D'
    if (grepl('lung_snps',outcome)) scores_table$PRS_snps[i+1] <- 'Lung cancer'
    if (grepl('prc_269_snps',outcome)) scores_table$PRS_snps[i+1] <- 'Prostate cancer'
    if (grepl('panc_22_snps',outcome)) scores_table$PRS_snps[i+1] <- 'Pancreatic cancer'
    if (grepl('crc_143_snps',outcome)) scores_table$PRS_snps[i+1] <- 'Colorectal cancer'
    if (grepl('brc_212_snps',outcome)) scores_table$PRS_snps[i+1] <- 'Breast cancer'
  }

  return(scores_table)
}
