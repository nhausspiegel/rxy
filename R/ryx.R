#' Calculate correlations in a data frame
#'
#' Calculates correlations between one variable (y) and all others (x)
#' in a data frame.
#'
#' @param data A data frame.
#' @param y The name of one of the variables in data.
#' @param x A character vector: the names of other variables in data.
#' If missing, the numeric variables in data other than y will be used.
#' @return A list including y, x, and a data frame of results, which includes
#' correlation values, p-values, and significance values for each variable
#' in x when tested against y.
#' @export
#' @examples
#' library(MASS)
#' ryx(Boston, y="medv")
ryx <- function(data, y, x){
  if(missing(x)){
    x <- names(data)[sapply(data, class)=="numeric"]
    x <- setdiff(x, y)
  }
  df <- data.frame()
  for (var in x){
    res <- cor.test(data[[y]], data[[var]])
    df_temp <- data.frame(variable = var,
                          r = res$estimate,
                          p = res$p.value)
    df <- rbind(df, df_temp)
    df <- df[order(-abs(df$r)),]
  }

  df$sigif <- ifelse(df$p < .001, "***",
                     ifelse(df$p < .01, "**",
                            ifelse(df$p < .05, "*", " ")))
  results <- list(y=y, x=x, df=df)
  class(results) <- "ryx"
  return(results)
}

#' Print a ryx object
#'
#' Prints a ryx object in a nicely formatted and informative manner.
#'
#' @param obj A ryx object.
#' @export
#' @examples
#' library(MASS)
#' print(ryx(Boston, y="medv"))
print.ryx <- function(obj) {
  cat(paste("Correlations of", obj$y, "with\n"))
  res_df <- obj$df
  res_df$r <- round(res_df$r, digits = 3)
  res_df$p <- signif(res_df$p, digits = 3)
  res_df$p[res_df$p < 2e-16] <- "< 2e-16"
  print(res_df, row.names = FALSE)
}

#' Summarize a ryx object
#'
#' Summarizes a ryx object, providing statistical info about its correlations.
#'
#' @param obj A ryx object.
#' @export
#' @examples
#' library(MASS)
#' summary(ryx(Boston, y="medv"))
summary.ryx <- function(obj) {

  cat(paste("Correlating", obj$y, "with "))
  cat(paste(obj$x))

  corr <- round(median(abs(obj$df$r)), digits = 3)
  range <- round(range(obj$df$r), digits = 3)
  cat(paste("\nThe median absolute correlation was", corr,
            "with a range from", range[1], "to", range[2]))

  n <- length(obj$df$r)
  n_signif <- length(obj$df$p[obj$df$p < 0.05])
  cat(paste("\n", n_signif, "out of", n,
            "variables were significant at the p < 0.05 level.\n"))

}

#' Plot a ryx object
#'
#' Plots a ryx object using ggplot2.
#'
#' The plot is arranged by the absolute value of each variable's correlation.
#'
#' @param obj A ryx object.
#' @export
#' @examples
#' library(MASS)
#' plot(ryx(Boston, y="medv"))
plot.ryx <- function(obj) {
  require(ggplot2)
  df <- obj$df[order(abs(obj$df$r)),]
  vars <- factor(df$variable, levels = df$variable)
  options(repr.plot.width = 8, repr.plot.height = 6)
  ggplot(df, aes(x = vars,
             y = abs(df$r))) +
    geom_point(aes(color = ifelse(df$r >= 0, "positive", "negative")),
               stat = "identity",
               size = 2.5) +
    geom_segment(aes(xend = vars),
                 yend = 0,
                 linewidth = 0.2,
                 color = "grey") +
    labs(title = paste("Correlations with", obj$y),
         x = "Variables",
         y = "Correlation (absolute value)",
         color = "Direction") +
    scale_color_manual(values = c("red",
                                  "blue")) +
    scale_y_continuous(breaks = seq(0, 1, 0.1),
                       limits = c(0, 1)) +
    theme(panel.grid.major.x = element_line(color = "grey90",
                                            linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = "white",
                                          color = "black"),
          legend.key = element_rect(fill = "white")) +
    coord_flip()
}
