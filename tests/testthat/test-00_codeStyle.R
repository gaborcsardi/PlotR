context("Code Style")

# test_that("Code style is in line with INWT style conventions", {
#   Sys.setenv(NOT_CRAN = "true")
#   lintr::expect_lint_free(linters = list(
#     a = lintr::assignment_linter,
#     b = lintr::commas_linter,
#     ## c = lintr::commented_code_linter,
#     d = lintr::infix_spaces_linter,
#     e = lintr::line_length_linter(100),
#     f = lintr::no_tab_linter,
#     ## g = lintr::snake_case_linter, # detects only testthat functions
#     ## g = lintr::object_name_linter(styles = "snake_case"),
#     h = lintr::object_length_linter(30),
#     i = lintr::spaces_left_parentheses_linter
#   ))
# })

# Leads to "invalid 'path' argument" error. Only run locally.
