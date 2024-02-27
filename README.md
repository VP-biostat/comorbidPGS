
<!-- README.md is generated from README.Rmd. Please edit that file -->

# comorbidPRS

<!-- badges: start -->

[![GitHub
tag](https://img.shields.io/github/v/tag/VP-biostat/comorbidPRS.svg?label=latest%20version)](https://github.com/VP-biostat/comorbidPRS)
<!-- badges: end -->

comorbidPRS is a tool for analysing an already computed PRS distribution
to investigate shared aetiology in multiple conditions.

comorbidPRS is under GPL-3 license, and is freely available for
download.

## Prerequisite

-   R version 3.5 or higher with the following packages:
    -   stats
    -   utils
    -   ggplot2

## Installation

You can install the development version of comorbidPRS from
[GitHub](https://github.com/VP-biostat/comorbidPRS) with:

``` r
# install.packages("devtools")
devtools::install_github("VP-biostat/comorbidPRS")
```

## Example

### Building an Association Table

This is a basic example which shows you how to do basic association with
the example dataset:

``` r
library(comorbidPRS)
#> 
#> Attachement du package : 'comorbidPRS'
#> L'objet suivant est masqué depuis 'package:graphics':
#> 
#>     assocplot

# use the demo dataset
dataset <- comorbidData
# NOTE: The dataset must have at least 3 different columns:
# - an ID column (the first one)
# - a PRS column (must be numeric, by default it is the column named "SCORESUM" or the second column if "SCORESUM" is not present)
# - a Phenotype column, can be factors, numbers or characters

# do an association of one PRS with one Phenotype
result_1 <- assoc(dataset, prs_col = "t2d_PRS", phenotype_col = "t2d")
```

| PRS     | Phenotype | Phenotype_type | Statistical_method         | Covar | N_cases | N_controls |     N |   Effect | SE  | lower_CI | upper_CI | P_value |
|:--------|:----------|:---------------|:---------------------------|:------|--------:|-----------:|------:|---------:|:----|---------:|---------:|--------:|
| t2d_PRS | t2d       | Cases/Controls | Binary logistic regression | NA    |    2845 |      36535 | 39380 | 1.784127 | NA  | 1.714658 |  1.85641 |       0 |

``` r
# do multiple associations
assoc <- expand.grid(c("t2d_PRS", "ldl_PRS"), c("ethnicity","brc","t2d","log_ldl","sbp_cat"))
result_2 <- multiassoc(df = dataset, assoc_table = assoc, covar = c("age", "sex", "gen_array"))
#> Warning in phenotype_type(df = df, phenotype_col = phenotype_col): Phenotype
#> column log_ldl is continuous and not normal, please normalise prior association

#> Warning in phenotype_type(df = df, phenotype_col = phenotype_col): Phenotype
#> column log_ldl is continuous and not normal, please normalise prior association
```

|     | PRS     | Phenotype        | Phenotype_type      | Statistical_method              | Covar             | N_cases | N_controls |     N |    Effect |        SE |  lower_CI |  upper_CI |   P_value |
|:----|:--------|:-----------------|:--------------------|:--------------------------------|:------------------|--------:|-----------:|------:|----------:|----------:|----------:|----------:|----------:|
| 2   | t2d_PRS | ethnicity 1 \~ 2 | Categorical         | Multinomial logistic regression | age+sex+gen_array |    8526 |      24922 | 33448 | 0.9843528 |        NA | 0.9604383 | 1.0088627 | 0.2088171 |
| 3   | t2d_PRS | ethnicity 1 \~ 3 | Categorical         | Multinomial logistic regression | age+sex+gen_array |    4779 |      24922 | 29701 | 1.0161173 |        NA | 0.9604383 | 1.0088627 | 0.3114462 |
| 4   | t2d_PRS | ethnicity 1 \~ 4 | Categorical         | Multinomial logistic regression | age+sex+gen_array |    1153 |      24922 | 26075 | 1.0026245 |        NA | 0.9604383 | 1.0088627 | 0.9306895 |
| 21  | ldl_PRS | ethnicity 1 \~ 2 | Categorical         | Multinomial logistic regression | age+sex+gen_array |    8526 |      24922 | 33448 | 0.9925562 |        NA | 0.9684554 | 1.0172569 | 0.5513399 |
| 31  | ldl_PRS | ethnicity 1 \~ 3 | Categorical         | Multinomial logistic regression | age+sex+gen_array |    4779 |      24922 | 29701 | 1.0152907 |        NA | 0.9684554 | 1.0172569 | 0.3370977 |
| 41  | ldl_PRS | ethnicity 1 \~ 4 | Categorical         | Multinomial logistic regression | age+sex+gen_array |    1153 |      24922 | 26075 | 1.0128078 |        NA | 0.9684554 | 1.0172569 | 0.6730281 |
| 1   | t2d_PRS | brc              | Cases/Controls      | Binary logistic regression      | age+sex+gen_array |    1601 |      19867 | 21468 | 1.0348337 |        NA | 0.9832930 | 1.0890761 | 0.1889709 |
| 11  | ldl_PRS | brc              | Cases/Controls      | Binary logistic regression      | age+sex+gen_array |    1601 |      19867 | 21468 | 1.0119061 |        NA | 0.9615206 | 1.0649319 | 0.6496868 |
| 12  | t2d_PRS | t2d              | Cases/Controls      | Binary logistic regression      | age+sex+gen_array |    2845 |      36535 | 39380 | 1.8286147 |        NA | 1.7557926 | 1.9044570 | 0.0000000 |
| 13  | ldl_PRS | t2d              | Cases/Controls      | Binary logistic regression      | age+sex+gen_array |    2845 |      36535 | 39380 | 0.9968686 |        NA | 0.9591734 | 1.0360453 | 0.8732979 |
| 14  | t2d_PRS | log_ldl          | Continuous          | Linear regression               | age+sex+gen_array |      NA |         NA | 39380 | 0.0036203 | 0.0011515 | 0.0013633 | 0.0058773 | 0.0016686 |
| 15  | ldl_PRS | log_ldl          | Continuous          | Linear regression               | age+sex+gen_array |      NA |         NA | 39380 | 0.0843551 | 0.0010703 | 0.0822574 | 0.0864529 | 0.0000000 |
| 16  | t2d_PRS | sbp_cat          | Ordered Categorical | Ordinal logistic regression     | age+sex+gen_array |      NA |         NA | 39380 | 1.0757027 |        NA | 1.0730560 | 1.0783558 | 0.0000000 |
| 17  | ldl_PRS | sbp_cat          | Ordered Categorical | Ordinal logistic regression     | age+sex+gen_array |      NA |         NA | 39380 | 1.0754824 |        NA | 1.0728378 | 1.0781336 | 0.0000000 |

### Examples of plot

``` r
densityplot(dataset, prs_col = "ldl_PRS", phenotype_col = "sbp_cat")
```

<img src="man/figures/README-densityplot-1.png" width="100%" />

``` r
# show multiple associations in a plot
assoplot <- assocplot(score_table = result_2)
assoplot$continuous_phenotype
```

<img src="man/figures/README-assocplot-1.png" width="100%" />

``` r
assoplot$discrete_phenotype
```

<img src="man/figures/README-assocplot-2.png" width="100%" /> NOTE: The
score_table should have the assoc() output format

``` r
centileplot(dataset, prs_col = "brc_PRS", phenotype_col = "brc")
```

<img src="man/figures/README-centileplot-1.png" width="100%" />

As those graphical functions use ggplot2, you can fully customize your
plot:

``` r
library(ggplot2)
centileplot(dataset, prs_col = "t2d_PRS", phenotype_col = "t2d") + 
  scale_color_gradient(low = "green", high = "red")
```

<img src="man/figures/README-centileplot-and-ggplot-1.png" width="100%" />

``` r
decileboxplot(dataset, prs_col = "ldl_PRS", phenotype_col = "ldl")
```

<img src="man/figures/README-decileplot-1.png" width="100%" />

## Citation

If you use comorbidPRS in any published work, please cite the following
manuscript:

<p>
Pascat V (????). <em>comorbidPRS: Assessing the shared predisposition
between Phenotypes using Polygenic Scores (PRS)</em>. R package version
0.2.9000.
</p>
