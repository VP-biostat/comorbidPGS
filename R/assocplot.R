#' @title
#' Multiple PRS Associations Plot
#'
#' @description
#' `assocplot()` take a data frame of associations, return plot of the associations
#' from `assoc()` (ggplot2 object or list of ggplot object)
#'
#' @param score_table a dataframe with association results with at least the
#' following columns:
#'
#' * PRS: the name of the PRS
#' * Phenotype: the name of Phenotype
#' * Phenotype_type: either `'Continuous'`, `'Ordered Categorical'`, `'Categorical'` or `'Cases/Controls'`
#' * Effect: if Phenotype_type is Continuous, it represents the Beta coefficient of linear regression, OR of logistic regression otherwise
#' * lower_CI: lower confidence interval of the related Effect (Beta or OR)
#' * upper_CI: upper confidence interval of the related Effect (Beta or OR)
#' * P_value: associated P-value
#' @param axis a character, `'horizontal'` or `"vertical"` (the default) specifying the rotation
#' of the plot
#' @param pval a  parameter specifying information on how to display P-value
#'
#'  * if pval is FALSE, P-value does not appear on the plot
#'  * if pval is TRUE, P-value always appears next to the signal
#'  * if pval is a number, P-value will appear if the P-value is inferior to
#'  this given number.
#'
#' @return return either:
#'
#'  * a ggplot object representing the association results.
#'  * a list of two ggplot objects, accessible by $continuous_phenotype and
#'  $discrete_phenotype, if there are both Continuous Phenotypes and Discrete
#'  Phenotypes (i.e. "Categorical" or "Cases/Controls")
#'
#' @import ggplot2
#' @export
assocplot <- function(score_table = NULL, axis = "vertical", pval = F) {
  ## Checking inputs
  if (is.null(score_table)) {
    stop("Please provide a data frame (that includes at least 'PRS'	'Phenotype' 'Phenotype_type'  'Effect'	'lower_CI'	'upper_CI'	'P_value')")
  } else if (!Reduce(`|`, class(score_table) %in% c("data.frame"))) {
    stop("Please provide a data frame (that includes at least 'PRS'	'Phenotype' 'Phenotype_type'  'Effect'	'lower_CI'	'upper_CI'	'P_value')")
  } else if (!"PRS" %in% names(score_table)) {
    stop("Please provide a column named 'PRS' in the data frame of results")
  } else if (!"Phenotype" %in% names(score_table)) {
    stop("Please provide a column named 'Phenotype' in the data frame of results")
  } else if (!"Phenotype_type" %in% names(score_table)) {
    stop("Please provide a column named 'Phenotype_type' in the data frame of results")
  } else if (!"Effect" %in% names(score_table)) {
    stop("Please provide a column named 'Effect' in the data frame of results")
  } else if (!"lower_CI" %in% names(score_table)) {
    stop("Please provide a column named 'lower_CI' in the data frame of results")
  } else if (!"upper_CI" %in% names(score_table)) {
    stop("Please provide a column named 'upper_CI' in the data frame of results")
  } else if (!"P_value" %in% names(score_table)) {
    stop("Please provide a column named 'P_value' in the data frame of results")
  } else if (is.null(axis)) {
    warning("'axis' parameter is not 'horizontal' or 'vertical', changing it to
            the value by default")
    axis <- "vertical"
  } else if (!axis %in% c("horizontal", "vertical")) {
    warning("'axis' parameter is not 'horizontal' or 'vertical', changing it to the value by default")
    axis <- "vertical"
  } else if (is.null(pval)) {
    stop("Missing 'pval' parameter")
  } else if (!Reduce(`|`, class(pval) %in% c("numeric", "integer", "double", "logical"))) {
    stop("'pval' parameter should be either a logical or a numeric")
  }

  ## Making plot
  p <- NULL
  p1_flag <- F
  p2_flag <- F
  if (axis == "horizontal") {

    if ("Continuous" %in% score_table$Phenotype_type) {

      temp_score <- score_table[which(score_table$Phenotype_type == "Continuous"),]
      p1_flag <- T

      p1 <- ggplot(temp_score, aes(x = Phenotype, y = Effect, ymin = lower_CI, ymax = upper_CI, color = PRS)) +
        geom_point(position = position_dodge(0.5), cex = 2) +
        geom_errorbar(lwd = 1.25, width = 0.2, position = position_dodge(0.5)) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
        geom_text(aes(label = ifelse(((P_value <= pval) & (pval != F)), "*", ""), group = PRS),
                  angle = 90, colour = "black", size = 5, position = position_dodge(0.5), vjust = 2
        ) +
        labs(color = "PRS", y = "Beta", x = "Phenotype") +
        theme_minimal() +
        theme(
          axis.title.y = element_text(size = 11),
          axis.title.x = element_text(size = 11),
          legend.position = ifelse((length(unique(score_table$PRS)) == 1), "none", "right")
        )

    }

    if (Reduce(`|`, (c("Cases/Controls","Categorical","Ordered Categorical") %in% score_table$Phenotype_type))) {

      temp_score <- score_table[which(score_table$Phenotype_type %in% c("Cases/Controls", "Categorical")),]
      p2_flag <- T

      p2 <- ggplot(temp_score, aes(x = Phenotype, y = Effect, ymin = lower_CI, ymax = upper_CI, color = PRS)) +
        geom_point(position = position_dodge(0.5), cex = 2) +
        geom_errorbar(lwd = 1.25, width = 0.2, position = position_dodge(0.5)) +
        geom_hline(yintercept = 1, linetype = "dashed", colour = "grey") +
        geom_text(aes(label = ifelse(((P_value <= pval) & (pval != F)), "*", ""), group = PRS),
                  angle = 90, colour = "black", size = 5, position = position_dodge(0.5), vjust = 2
        ) +
        labs(color = "PRS", y = "Odds Ratio", x = "Phenotype") +
        theme_minimal() +
        theme(
          axis.title.y = element_text(size = 11),
          axis.title.x = element_text(size = 11),
          legend.position = ifelse((length(unique(score_table$PRS)) == 1), "none", "right")
        )

    }

  } else if (axis == "vertical") {

    if ("Continuous" %in% score_table$Phenotype_type) {

      p1_flag <- T
      temp_score <- score_table[which(score_table$Phenotype_type == "Continuous"),]

      p1 <- ggplot(temp_score, aes(y = Phenotype, x = Effect, xmin = lower_CI, xmax = upper_CI, color = PRS)) +
        geom_point(position = position_dodge(0.5), cex = 2) +
        geom_errorbar(lwd = 1.25, width = 0.2, position = position_dodge(0.5)) +
        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey") +
        geom_text(aes(label = ifelse(((P_value <= pval) & (pval != F)), "*", ""), group = PRS),
                  angle = 0, colour = "black", size = 5, position = position_dodge(0.5), vjust = 1
        ) +
        labs(color = "PRS", x = "Beta", y = "Phenotype") +
        theme_minimal() +
        theme(
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size = 11),
          legend.position = ifelse((length(unique(score_table$PRS)) == 1), "none", "bottom")
        )

    }

    if (Reduce(`|`, (c("Cases/Controls","Categorical","Ordered Categorical") %in% score_table$Phenotype_type))) {

      p2_flag <- T
      temp_score <- score_table[which(score_table$Phenotype_type %in% c("Cases/Controls", "Categorical")),]

      p2 <- ggplot(temp_score, aes(y = Phenotype, x = Effect, xmin = lower_CI, xmax = upper_CI, color = PRS)) +
        geom_point(position = position_dodge(0.5), cex = 2) +
        geom_errorbar(lwd = 1.25, width = 0.2, position = position_dodge(0.5)) +
        geom_vline(xintercept = 1, linetype = "dashed", colour = "grey") +
        geom_text(aes(label = ifelse(((P_value <= pval) & (pval != F)), "*", ""), group = PRS),
                  angle = 0, colour = "black", size = 5, position = position_dodge(0.5), vjust = 1
        ) +
        labs(color = "PRS", x = "Odds Ratio", y = "Phenotype") +
        theme_minimal() +
        theme(
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size = 11),
          legend.position = ifelse((length(unique(score_table$PRS)) == 1), "none", "bottom")
        )

    }

  }

  ## Returning a ggplot or a list of ggplots?
  if (p1_flag & p2_flag) {
    p <- list("continuous_phenotype" = p1, "discrete_phenotype" = p2)
  } else if (p1_flag) {
    p <- p1
  } else if (p2_flag) {
    p <- p2
  }

  return(p)

}
