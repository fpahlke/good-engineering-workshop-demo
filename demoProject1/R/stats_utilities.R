


## Principle 5 — **Pure inputs, explicit outputs**

#' 
#' @title 
#' Compute Confidence Interval
#'
#' @description
#' Computes the confidence interval for a set of point estimates using standard errors.
#'
#' @param estimates Numeric vector. The point estimates.
#' @param se Numeric vector. The standard errors for each estimate.
#' @param alpha Numeric. The significance level to use for the interval (default is 0.05).
#'
#' @return A \code{data.frame} with the following columns:
#' \itemize{
#'   \item \code{estimate}: The original point estimates.
#'   \item \code{ci_lower}: The lower bound of the computed confidence interval.
#'   \item \code{ci_upper}: The upper bound of the computed confidence interval.
#' }
#'
#' @examples
#' \dontrun{
#' estimates <- c(3, 5, 7)
#' se <- c(0.5, 0.7, 0.6)
#' add_ci(estimates, se)
#' }
#'
#' @export
#' 
add_ci <- function(estimates, se, alpha = 0.05) {
    z <- qnorm(1 - alpha / 2)
    data.frame(
        estimate = estimates,
        ci_lower = estimates - z * se,
        ci_upper = estimates + z * se
    )
}


#' 
#' @title 
#' Assert Condition With Message
#'
#' @description
#' Checks a condition and stops execution with a custom error message if the condition is not met.
#' The error message is formed by concatenating the additional arguments.
#'
#' @param condition Logical. The condition to be evaluated.
#' @param message Character. The base message to be used if the condition is FALSE.
#' @param ... Additional message parts to be concatenated for the error message.
#'
#' @return This function does not return a value. It stops execution with an error message if
#' the condition is FALSE.
#'
#' @examples
#' \dontrun{
#'   x <- 5
#'   stopifnot_with_message(x > 10, "x must be greater than 10")
#' }
#'
#' @export
#' 
stopifnot_with_message <- function(condition, message, ...) {
    if (!condition) {
        stop(paste0(c(message, list(...)), collapse = " "), call. = FALSE)
    }
}


#' 
#' @title
#' Load Data from CSV File
#'
#' @description
#' Loads a CSV file from the specified file path. It checks if the file exists and stops
#' execution with an error message if it does not.
#'
#' @param path Character. The file path to the CSV file.
#'
#' @return A \code{data.frame} containing the CSV data.
#'
#' @examples
#' \dontrun{
#'   data <- load_data("data/file.csv")
#' }
#'
#' @export
#' 
load_data <- function(path) {
    stopifnot_with_message(is.character(path) && length(path) == 1, 
        "'path' must be a single character string")
    stopifnot_with_message(file.exists(path), 
        "File ", sQuote(path), " does not exist")
    read.csv(path)
}

#' 
#' @title
#' Assert Valid Data Frame
#'
#' @description
#' Validates that the provided object is a valid data frame. It checks that the data frame is
#' not \code{NULL}, that it is of class \code{data.frame}, and that it has at least one column and one row.
#'
#' @param df A data frame object to be validated.
#'
#' @return This function does not return a value. It stops execution with an error message if any
#' of the validations fail.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:10)
#' assert_is_valid_data_frame(df)
#' }
#' 
assert_is_valid_data_frame <- function(df) {
    stopifnot_with_message(!is.null(df), 
        "'df' must not be NULL")
    stopifnot_with_message(is.data.frame(df), 
        "'df' must be a data frame")
    stopifnot_with_message(ncol(df) > 0, 
        "'df' must have at least one column")
    stopifnot_with_message(nrow(df) > 0, 
        "'df' must have at least one row")
}

#' 
#' @title
#' Assert Is Valid Parameter
#'
#' @description
#' Validates that the provided parameter name is a single character string and is not NA.
#'
#' @param param_name Character. The name of the parameter to validate.
#'
#' @return This function does not return a value. It stops execution with an error message if the validations fail.
#'
#' @examples
#' \dontrun{
#'   assert_is_valid_parameter("age")
#' }
#'
#' @export
assert_is_valid_parameter <- function(param_name) {
    stopifnot_with_message(!is.null(param_name) && 
            length(param_name) == 1 &&
            !is.na(param_name) &&
            is.character(param_name), 
        "'param_name' must be a single character string")
}

#' 
#' @title
#' Assert Data Frame Has Column
#'
#' @description
#' Validates that the provided data frame contains the specified column. It uses
#' pre-existing checks to validate the data frame and the parameter name, then verifies
#' that the given column exists in the data frame.
#'
#' @param df A data frame object to be validated.
#' @param param_name Character. The name of the column to check in the data frame.
#'
#' @return This function does not return a value. It stops execution with an error message if the
#' validations fail or if the column is not present.
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(a = 1:5, b = 6:10)
#'   assert_dataframe_has_column(df, "a")
#' }
#'
#' @export
#' 
assert_dataframe_has_column <- function(df, param_name) {
    assert_is_valid_data_frame(df) 
    assert_is_valid_parameter(param_name)
    stopifnot_with_message(param_name %in% names(df), 
        "Parameter ", sQuote(param_name), " does not exist in the provided data frame")
}

#' 
#' @title
#' Filter Complete
#'
#' @description
#' Filters the data frame by removing rows where the specified column contains \code{NA} values.
#'
#' @param df A data frame object to filter.
#' @param param_name Character. The name of the column to check for non-\code{NA} values.
#'
#' @return A \code{data.frame} containing only rows with non-\code{NA} values in the specified column.
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(a = c(1, NA, 3), b = c(4, 5, NA))
#'   complete_df <- filter_complete(df, "a")
#' }
#'
#' @export
#' 
filter_complete <- function(df, param_name) {
    assert_dataframe_has_column(df, param_name)
    subset(df, !is.na(df[[param_name]]))
}

#' 
#' @title
#' Summarise Parameter
#'
#' @description
#' Summarises a parameter in the provided data frame by computing its mean and standard deviation.
#' It first validates that the specified column exists in the data frame.
#'
#' @param df A data frame object.
#' @param param_name Character. The name of the column to summarise.
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{param_name}: The name of the parameter.
#'   \item \code{mean}: The computed mean of the parameter.
#'   \item \code{sd}: The computed standard deviation of the parameter.
#' }
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(a = rnorm(100))
#'   summarise_parameter(df, "a")
#' }
#'
#' @export
#' 
summarise_parameter <- function(df, param_name) {
    assert_dataframe_has_column(df, param_name)
    param <- df[[param_name]]
    list(
        param_name = param_name,
        mean = mean(param), 
        sd = sd(param)
    )
}