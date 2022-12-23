#' @title
#' Centiles Plot from a PRS Association
#'
#' @description
#' `centileplot()` take a distribution of PRS, a Phenotype and eventual Confounders
#' return a plot (ggplot2 object) with centiles (or deciles if not enough individuals)
#' of PRS in x and Prevalence/Median/Mean of the Phenotype in y
#'
#' @param df a dataframe with individuals on each row, at least one column PRS
#' (continuous variable) and one with phenotype (continuous or categorical)
#' @param prs_col a character specifying the PRS column name
#' @param phenotype_col a character specifying the Phenotype column name
#' @param scale a boolean specifying if scaling of PRS should be done before plotting
#' @param decile a boolean specifying if centiles or deciles should be used
#' @param continuous_metric a facultative character specifying what metric to
#' use for continuous Phenotype, only two options: "median" or "mean"
#' @param filename a facultative character, specifying the path and file name
#' where to store the plot
#'
#' @return return a figure of results in the format ggplot2 object
#' @importFrom stats na.omit
#' @import ggplot2
#' @export
centileplot <- function(df = NULL, prs_col = "SCORESUM", phenotype_col =
                             "Phenotype", scale = T, decile = F, continuous_metric = NA,
                           filename = NA) {
  ## Checking inputs
  if (is.null(df)) {
    stop("Please provide a data frame (that includes PRS values with at least
         columns PRS and Phenotype)")
  } else if (ncol(df)<2) {
    stop("Please provide a data frame (that includes at least 2 columns PRS
         and Phenotype)")
  } else if (length(unique(df[,phenotype_col])) < 2) {
    stop("Phenotype column have less than 2 values")
  } else if (!continuous_metric %in% c(NA, "median", "mean")) {
    stop("continuous_metric parameter only accepts tree values: NA, 'median'
    or 'mean'")
  }

  ## Taking only subset of df
  df <- df[,c(prs_col,phenotype_col)]
  names(df) <- c("PRS","Phenotype")
  df <- na.omit(df)
  if (scale) {
    df[,"PRS"] <- scale(df[,"PRS"]) #scaling if scale = T
  }
  if (nrow(df) < 10000) {
    warning("The dataset has less than 10,000 individuals, centiles plot might not look good!
    Use the argument decile = T to adapt to small datasets")
  }

  ## Making centiles plot based on the category of Phenotype
  phenotype_type <- phenotype_type(df = df, phenotype_col = "Phenotype")

  df$centile <- with(df, cut(PRS, breaks=quantile(PRS, probs=seq(0,1, by=0.01), na.rm=TRUE), include.lowest=TRUE, dig.lab = 10))
  df$centile <- as.factor(df$centile)
  levels(df$centile) <- seq(1,100, by=1)
  if (decile) {
    df$centile <- cut(as.numeric(df$centile), breaks = seq(0,100, by = 10))
    df$centile <- as.factor(df$centile)
    levels(df$centile) <- seq(1,10, by=1)
  }

  preval <- data.frame(matrix(nrow = 0, ncol = 4))

  if (phenotype_type == "Categorical") {
    stop("Cannot use a categorical phenotype for centiles plot. Please use
         a Cases/Controls or Continuous Phenotype")
  } else if (phenotype_type == "Cases/Controls") {

    names(preval) <- c("centile","prevalence","n_cases","n")

    #create prevalence of cases group by centiles
    for (val in levels(df$centile)) {
      n_cases <- sum(df$centile == val & as.logical(df$Phenotype) == T, na.rm = T)
      n_controls <- sum(df$centile == val & as.logical(df$Phenotype) == F, na.rm = T)
      n <- n_cases+n_controls
      preval <- rbind(preval, data.frame("centile" = val,
                                         "n_cases" = n_cases,
                                         "prevalence" = n_cases/n,
                                         "n" = n))
    }

    p <- ggplot(preval, aes(x = as.numeric(centile),
                            y = as.numeric(prevalence)*100,
                            color = as.numeric(prevalence))) +
      geom_point(show.legend = F) +
      xlim(1,ifelse(decile, 10,100)) +
      labs(x = paste(ifelse(decile, "Deciles of","Centiles of"),prs_col),
           y = paste("Prevalence of",phenotype_col)) +
      theme_minimal()+
      theme(axis.title.x = element_text(vjust=-0.5, size = 11),
            axis.text.x.bottom = element_text(size = 11),
            axis.title.y = element_text(size = 11),
            axis.text.y.left = element_text(size = 11))
  } else if (phenotype_type == "Continuous") {

    if (is.na(continuous_metric)) {
      warning("The Phenotype selected is continuous, using mean value group by centiles by default")
      continuous_metric <- "mean"
    }

    names(preval) <- c("centile","median","mean","n")

    #create median of continuous phenotype group by centiles
    for (val in levels(df$centile)) {
      med <- median(df[which(df$centile == val), "Phenotype"], na.rm = T)
      mean <- mean(df[which(df$centile == val), "Phenotype"], na.rm = T)
      n <- nrow(df[which(df$centile == val),])
      preval <- rbind(preval, data.frame("centile" = val,
                                         "median" = med,
                                         "mean" = mean,
                                         "n" = n))
    }

    p <- ggplot(preval, aes(x = as.numeric(centile),
                            y = as.numeric(get(continuous_metric)),
                            color = as.numeric(get(continuous_metric)))) +
      geom_point(show.legend = F) +
      xlim(1,ifelse(decile, 10,100)) +
      labs(x = paste(ifelse(decile, "Deciles of","Centiles of"),prs_col),
           y = paste(continuous_metric,phenotype_col)) +
      theme_minimal()+
      theme(axis.title.x = element_text(vjust=-0.5, size = 11),
            axis.text.x.bottom = element_text(size = 11),
            axis.title.y = element_text(size = 11),
            axis.text.y.left = element_text(size = 11))


  }

  if (!is.na(filename)) {
    ggsave(p, file = filename)
  }

  return(p)
}

