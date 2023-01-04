# comorbidPRS 
comorbidPRS is a tool for analysing an already computed PRS distribution to investigate shared aetiology in multiple conditions.

comorbidPRS is under GPL-3 license, and is freely available for download. 

## Prerequisite
stats 
utils
ggplot2 
R version 3.5 or higher 

## Installation
Direct download is available on [github](https://github.com/VP-biostat/comorbidPRS).
If you want to install comorbidPRS, put the code below directly in your R command shell
```bash
install.package("devtools") #if it is not done already
library(devtools)
install_github("VP-biostat/comorbidPRS")
```

## How to use it?
You can type this code in your R command shell to see a demo: 
```bash
library(comorbidPRS)

#use the demo dataset
dataset <- data(comorbidExample)

#do an association of one PRS with one Phenotype
result_1 <- assoc(dataset, prs_col = "PRS_1", phenotype_col = "Phenotype_1")
print(result_1)
#show some plots on these PRS/Phenotype
centile_plot <- centileplot(dataset, prs_col = "PRS_1", phenotype_col = "Phenotype_1", continuous_metric = "mean")
centile_plot
density_plot <- densityplot(dataset, prs_col = "PRS_1", phenotype_col = "Phenotype_2")
density_plot

#do multiple associations
assoc <- expand.grid(c("PRS_1", "PRS_2"), c("Phenotype_1", "Phenotype_2", "Phenotype_3", "Phenotype_4"))
result_2 <- multiassoc(df = dataset, assoc_table = assoc, covar = c("Age", "Sex", "Covariate"))
print(result_2)

#show multiple association in a plot
or_plot <- orplot(score_table = result_2)
or_plot
```

## Citation 
If you use comorbidPRS in any published work, please cite the following manuscript:

??? insert citation of the white paper ????