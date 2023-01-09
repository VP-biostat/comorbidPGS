#' @title
#' Odds Ratio plot from Multiple PRS Associations
#'
#' @description
#' `orplot()` take a data frame of associations (format from assoc()),
#' return a plot (ggplot2 object) of Odds Ratio
#'
#' @param score_table a dataframe with association results with at least
#' PRS	Phenotype  OR	lower_CI	upper_CI	P_value
#' @param axis a character ('horizontal' or 'vertical') specifying the rotation
#' of the plot, 'vertical' by default
#' @param pval a  parameter specifying information on how to display P-value
#' if pval is FALSE, P-value does not appear on the plot
#' if pval is TRUE, P-value always appears next to the signal
#' if pval is a number, P-value will appear if the P-value is inferior to
#' this given number.
#'
#' @return return a figure of results in the format ggplot2 object
#' @import ggplot2
#' @export
orplot <- function(score_table = NULL, axis = "vertical", pval = 0.05) {
  ## Checking inputs
  if (is.null(score_table)) {
    stop("Please provide a data frame (that includes at least 'PRS'	'Phenotype'  'OR'	'lower_CI'	'upper_CI'	'P_value')")
  } else if (!class(score_table)[1] %in% c("data.frame")) {
    stop("Please provide a data frame (that includes at least 'PRS'	'Phenotype'  'OR'	'lower_CI'	'upper_CI'	'P_value')")
  } else if (!"PRS" %in% names(score_table)) {
    stop("Please provide a column named 'PRS' in the data frame of results")
  } else if (!"Phenotype" %in% names(score_table)) {
    stop("Please provide a column named 'Phenotype' in the data frame of results")
  } else if (!"OR" %in% names(score_table)) {
    stop("Please provide a column named 'OR' in the data frame of results")
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
    warning("'axis' parameter is not 'horizontal' or 'vertical', changing it to
            the value by default")
    axis <- "vertical"
  } else if (is.null(pval)) {
    stop("Missing 'pval' parameter")
  } else if (!class(pval)[1] %in% c("numeric", "integer", "double", "logical")) {
    stop("'pval' parameter should be either a logical or a numeric")
  }

  ## Making plot
  if (axis == "horizontal") {
    p <- ggplot(score_table, aes(x = Phenotype, y = OR, ymin = lower_CI, ymax = upper_CI, color = PRS)) +
      geom_point(position = position_dodge(0.5), cex = 2) +
      geom_errorbar(lwd = 1.25, width = 0.2, position = position_dodge(0.5)) +
      geom_hline(yintercept = 1, linetype = "dashed", colour = "grey") +
      geom_text(aes(label = ifelse(((P_value <= pval) & (pval != F)), formatC(P_value, format = "e", digits = 1), ""), group = PRS),
        hjust = 0, vjust = 1, angle = 90, colour = "black", size = 3, position = position_dodge(1)
      ) +
      labs(color = "PRS", y = "Odds Ratio", x = "Phenotype") +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 11),
        axis.text.x.bottom = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y.left = element_text(size = 11),
        legend.position = ifelse((length(unique(score_table$PRS)) == 1), "none", "right")
      )
  } else if (axis == "vertical") {
    p <- ggplot(score_table, aes(y = Phenotype, x = OR, xmin = lower_CI, xmax = upper_CI, color = PRS)) +
      geom_point(position = position_dodge(0.5), cex = 2) +
      geom_errorbar(lwd = 1.25, width = 0.2, position = position_dodge(0.5)) +
      geom_vline(xintercept = 1, linetype = "dashed", colour = "grey") +
      geom_text(aes(label = ifelse(((P_value <= pval) & (pval != F)), formatC(P_value, format = "e", digits = 1), ""), group = PRS),
        hjust = 0, vjust = 0, angle = 0, colour = "black", size = 3, position = position_dodge(1)
      ) +
      labs(color = "PRS", y = "Odds Ratio", x = "Phenotype") +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 11),
        axis.text.x.bottom = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y.left = element_text(size = 11),
        legend.position = ifelse((length(unique(score_table$PRS)) == 1), "none", "bottom")
      )
  }

  return(p)
}
