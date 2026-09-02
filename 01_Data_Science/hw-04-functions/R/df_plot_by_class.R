#' Creates a set of plots
#'
#' It takes an input data frame and an input variable's class
#' and generates a list of plots for the variables with that
#' specific class type.
#'
#' Usage
#' df_plot_by_class(mpg, "character")
#'
#' @param df A data frame
#' @param class_var A variable class
#'
#' @return The set of plots of the variable(s) class level specified
#' @import tidyverse
#'
#' @examples
#' df <- data.frame(
#'   a = sample(letters[1:7], 200, replace = TRUE),
#'   b = rnorm(200),
#'   c = sample(c(TRUE, FALSE), 200, replace = TRUE),
#'   d = sample(letters[1:5], 200, replace = TRUE),
#'   e = rnorm(200),
#'   f = sample(c(TRUE, FALSE), 200, replace = TRUE)
#' )
#' list_of_plots <- df_plot_by_class(df, "numeric")
#' list_of_plots
#'
df_plot_by_class <- function(df, class_var) {
  # Validate inputs
  stopifnot(
    "The input must be a data frame" = is.data.frame(df),
    "This data frame must have more than one row" = nrow(df) > 1,
    "This must be a character name" = is.character(class_var)
  )

  # Get the variable names that match the class specified
  class_vars <- sapply(df, class)
  matching_vars <- names(class_vars[class_vars == class_var])

  # If no matching variables found, return error message
  if (length(matching_vars) == 0) {
    stop(paste(deparse(substitute(df)), "has no variables of class", class_var))
  }

  # Initiate list of plots
  plots <- list()

  for (var in matching_vars) {
    p <- NULL
    if (class_var %in% c("character", "factor", "logical")) {
      # Bar plots for character, factor, or logical types
      count_data <- df %>%
        group_by(.data[[var]]) %>%
        summarise(count = n()) %>%
        ungroup()

      p <- ggplot(count_data, aes_string(x = var, y = "count")) +
        geom_bar(stat = "identity") +
        labs(
          title = paste(deparse(substitute(df)), " - Counts of", var),
          x = var, y = "Count"
        ) +
        theme_minimal()
    } else if (class_var %in% c("double", "integer", "numeric")) {
      # Histograms for numeric types
      p <- ggplot(df, aes_string(x = var)) +
        geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
        labs(
          title = paste(deparse(substitute(df)), " - Histogram of", var),
          x = var, y = "Frequency"
        ) +
        theme_minimal()
    }

    # Store the plots
    plots[[var]] <- p
  }

  return(plots)
}
