# Test add_ci -----------------------------------------------------------------
test_that("add_ci computes correct confidence intervals", {
    estimates <- c(3, 5, 7)
    se <- c(0.5, 0.7, 0.6)
    alpha <- 0.05
    z <- qnorm(1 - alpha / 2)

    result <- add_ci(estimates, se, alpha)

    expect_equal(result$estimate, estimates)
    expect_equal(result$ci_lower, estimates - z * se)
    expect_equal(result$ci_upper, estimates + z * se)
})

# Test stopifnot_with_message -------------------------------------------------
test_that("stopifnot_with_message does not error when condition is TRUE", {
    expect_silent(stopifnot_with_message(TRUE, "This should not error"))
})

test_that("stopifnot_with_message errors when condition is FALSE", {
    expect_error(stopifnot_with_message(FALSE, "Error occurred"), "Error occurred")
})

# Test load_data --------------------------------------------------------------
test_that("load_data loads a CSV file", {
    # Create a temporary CSV file
    temp_file <- tempfile(fileext = ".csv")
    write.csv(mtcars, temp_file, row.names = FALSE)

    df <- load_data(temp_file)
    expect_true(is.data.frame(df))
    expect_equal(ncol(df), ncol(mtcars))

    # Remove temporary file
    unlink(temp_file)
})

test_that("load_data errors when file does not exist", {
    fake_path <- tempfile(fileext = ".csv")
    expect_error(load_data(fake_path), "does not exist")
})

# Test assert_is_valid_data_frame ---------------------------------------------
test_that("assert_is_valid_data_frame passes for a valid data frame", {
    df <- data.frame(x = 1:10)
    expect_silent(assert_is_valid_data_frame(df))
})

test_that("assert_is_valid_data_frame errors for NULL", {
    expect_error(assert_is_valid_data_frame(NULL), "must not be NULL")
})

test_that("assert_is_valid_data_frame errors for non-data.frame", {
    expect_error(assert_is_valid_data_frame(5), "a data frame")
})

# Test assert_is_valid_parameter ----------------------------------------------
test_that("assert_is_valid_parameter passes for a valid parameter", {
    expect_silent(assert_is_valid_parameter("age"))
})

test_that("assert_is_valid_parameter errors for non-character", {
    expect_error(assert_is_valid_parameter(123), "a single character string")
})

test_that("assert_is_valid_parameter errors for NA", {
    expect_error(assert_is_valid_parameter(NA), "must be a single character string")
})

# Test assert_dataframe_has_column --------------------------------------------
test_that("assert_dataframe_has_column passes when column exists", {
    df <- data.frame(a = 1:5, b = 6:10)
    expect_silent(assert_dataframe_has_column(df, "a"))
})

test_that("assert_dataframe_has_column errors when column does not exist", {
    df <- data.frame(a = 1:5)
    expect_error(assert_dataframe_has_column(df, "b"), "does not exist")
})

# Test filter_complete --------------------------------------------------------
test_that("filter_complete returns rows with non-NA values", {
    df <- data.frame(a = c(1, NA, 3), b = c(4, 5, NA))
    filtered <- filter_complete(df, "a")
    expect_true(all(!is.na(filtered$a)))
    expect_equal(nrow(filtered), 2)
})

# Test summarise_parameter -----------------------------------------------------
test_that("summarise_parameter computes correct mean and sd", {
    set.seed(123)
    vals <- rnorm(100)
    df <- data.frame(a = vals)
    summary <- summarise_parameter(df, "a")

    expect_equal(summary$param_name, "a")
    expect_equal(summary$mean, mean(vals))
    expect_equal(summary$sd, sd(vals))
})
