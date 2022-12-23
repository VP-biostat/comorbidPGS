#' @title
#' Density Plot from a PRS Association
#'
#' @description
#' `densityplot()` take a distribution of PRS, a Phenotype and eventual Confounders
#' return a plot (ggplot2 object) with density of PRS in x by Categories of the
#' Phenotype
#'
#' @param df a dataframe with individuals on each row, at least one column PRS
#' (continuous variable) and one with phenotype (continuous or categorical)
#' @param prs_col a character specifying the PRS column name
#' @param phenotype_col a character specifying the Phenotype column name
#' @param scale a boolean specifying if scaling of PRS should be done before plotting
#' @param threshold a facultative numeric specifying for continuous Phenotype
#' the Threshold to consider individuals as Cases/Constrols as following:
#' Phenotype > Threshold = Case
#' Phenotype < Threshold = Control
#' @param filename a facultative character, specifying the path and file name
#' where to store the plot
#'
#' @return return a figure of results in the format ggplot2 object
#' @importFrom stats na.omit
#' @import ggplot2
#' @export
densityplot <- function(df = NULL, prs_col = "SCORESUM", phenotype_col =
                      "Phenotype", scale = T, threshold = NA, filename = NA) {
  ## Checking inputs
  if (is.null(df)) {
    stop("Please provide a data frame (that includes PRS values with at least
         columns PRS and Phenotype)")
  } else if (ncol(df)<2) {
    stop("Please provide a data frame (that includes at least 2 columns PRS
         and Phenotype)")
  } else if (length(unique(df[,phenotype_col])) < 2) {
    stop("Phenotype column have less than 2 values")
  }

  ## Taking only subset of df
  df <- df[,c(prs_col,phenotype_col)]
  names(df) <- c("PRS","Phenotype")
  df <- na.omit(df)
  if (scale) {
    df[,"PRS"] <- scale(df[,"PRS"]) #scaling if scale = T
  }

  ## Making plot based on the category of Phenotype
  phenotype_type <- phenotype_type(df = df, phenotype_col = "Phenotype")
  if (phenotype_type %in% c("Cases/Controls","Categorical")) {
    p <- ggplot(df, aes(PRS, fill = as.factor(Phenotype))) +
      geom_density(alpha = 0.4) +
      labs(x = prs_col, y = "Density", fill = phenotype_col) +
      theme_minimal()+
      theme(axis.title.x = element_text(vjust=-0.5, size = 11),
            axis.text.x.bottom = element_text(size = 11, angle = 60),
            axis.title.y = element_text(size = 11),
            axis.text.y.left = element_text(size = 11))
  } else if (!is.na(threshold) & typeof(threshold) %in% c("integer","numeric","double")) {
    p <- ggplot(df, aes(PRS, fill = as.logical.factor(Phenotype > threshold))) + #TO BE COMPLETED
      geom_density(alpha = 0.4) +
      theme_minimal()+
      labs(x = prs_col, y = "Density", fill = phenotype_col) +
      theme(axis.title.x = element_text(vjust=-0.5, size = 11),
            axis.text.x.bottom = element_text(size = 11, angle = 60),
            axis.title.y = element_text(size = 11),
            axis.text.y.left = element_text(size = 11))
  } else {
    if (!typeof(threshold) %in% c("integer","numeric","double") & !is.na(threshold)) {
      warning("threshold is not a number, ignoring the parameter")
    }
    p <- ggplot(df, aes(PRS)) +
      geom_density(alpha = 0.4) +
      theme_minimal()+
      labs(x = prs_col, y = "Density") +
      theme(axis.title.x = element_text(vjust=-0.5, size = 11),
            axis.text.x.bottom = element_text(size = 11, angle = 60),
            axis.title.y = element_text(size = 11),
            axis.text.y.left = element_text(size = 11))
  }

  if (!is.na(filename)) {
    ggsave(p, file = filename)
  }

  return(p)
}
