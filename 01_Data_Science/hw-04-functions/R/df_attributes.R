#' Data Frame Characteristics
#'
#' Outputs a data frame with the characteristics of the input data frame
#'
#' @param df A data frame
#'
#' @return A data frame with the characteristics from the input data frame
#'
#' @import tidyverse
#'
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, 3, NA, 5),
#'   b = c("a", "b", "c", "d", "e"),
#'   c = c(TRUE, FALSE, TRUE, TRUE, TRUE)
#' )
#' df_summary <- df_attributes(df)
#' print(df_summary)
df_attributes <- function(df) {
  # Validate inputs
  stopifnot(
    "The input must be a data frame" = is.data.frame(df),
    "This data frame must have more than one row" = nrow(df) > 1
  )

  # Create a data frame with the attributes from the input data frame
  df_str <- tibble(
    variable_name = names(df),
    variable_class = sapply(df, class),
    variable_type = sapply(df, typeof),
    unique_values = sapply(df, function(x) length(unique(x))),
    na_count = sapply(df, function(x) sum(is.na(x)))
  )

  return(df_str)
}
