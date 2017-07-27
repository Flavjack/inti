library(stringr)
context("Upload template/fieldbook file")

test_that("without column header for dca/dbca design", {

  datos <- readxl::read_excel(path = "datasets/fbtemplate_file_cases.xlsx", sheet = 2,col_names = FALSE)
  header <- names(datos)
  testthat::expect_equal(header, "X0")

})


test_that("without column header for dca/dbca design in xlsx format", {

  datos <- readxl::read_excel(path = "datasets/fbtemplate_file_cases.xlsx", sheet = 2,col_names = FALSE)
  header <- names(datos)
  testthat::expect_equal(header, "X0")

})

test_that("Just headers in dca/dbca design in csv format", {

  #datos <- read.csv(file = "tests/testthat/datasets/no_header_dbca.csv",header = TRUE)
  datos <- read.csv(file = "datasets/no_header_dbca.csv",header = TRUE)
  n_rows <- nrow(datos)

  testthat::expect_equal(n_rows, 0)

})


test_that("No data in dca/dbca design in csv format", {

  #datos <- read.csv(file = "tests/testthat/datasets/no_data_dbca.csv",header = TRUE)
  datos <- try(read.csv(file = "datasets/no_data_dbca.csv",header = TRUE))
  n_rows <- nrow(datos)
  testthat::expect_equal(n_rows, NULL)

})


test_that("No data in fdca/fdbca design in csv format", {

  #datos <- read.csv(file = "tests/testthat/datasets/no_data_fdbca.csv",header = TRUE)
  datos <- try(read.csv(file = "datasets/no_data_fdbca.csv",header = TRUE))
  n_rows <- nrow(datos)
  testthat::expect_equal(n_rows, NULL)

})

#
# test_that("Just one header in fdca/fdbca design in csv format", {
#
#   #datos <- read.csv(file = "tests/testthat/datasets/no_header_fdbca.csv",header = TRUE)
#   datos <- read.csv(file = "datasets/no_header_fdbca.csv",header = TRUE)
#   n_rows <- ncol(datos)
#
#   testthat::expect_equal(n_rows, 1)
#
# })

