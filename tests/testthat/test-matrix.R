test_that("init_matrix creates a matrix with correct dimensions and names", {
  units <- c("Unit1", "Unit2", "Unit3")
  interventions <- c("Intervention1", "Intervention2", "Intervention3")

  mat <- init_matrix(units, interventions)

  expect_type(mat, "logical")
  expect_equal(nrow(mat), length(units))
  expect_equal(ncol(mat), length(interventions))
  expect_equal(dimnames(mat)[[1]], units)
  expect_equal(dimnames(mat)[[2]], interventions)
})

test_that("init_matrix creates a matrix with all values initialized to FALSE", {
  units <- c("Unit1", "Unit2", "Unit3")
  interventions <- c("Intervention1", "Intervention2", "Intervention3")

  mat <- init_matrix(units, interventions)

  expect_equal(all(mat == FALSE), TRUE)
})

test_that("init_matrix throws an error when units or interventions is empty", {
  units <- character(0)
  interventions <- c("Intervention1", "Intervention2", "Intervention3")

  expect_error(init_matrix(units, interventions),
               "length(units) > 0",
               fixed = TRUE)

  units <- c("Unit1", "Unit2", "Unit3")
  interventions <- character(0)

  expect_error(init_matrix(units, interventions),
               "length(interventions) > 0",
               fixed = TRUE)
})

test_that("init_matrix throws an error when units or interventions are not character vectors", {
  units <- c("Unit1", "Unit2", "Unit3")
  interventions <- 1:3

  expect_error(init_matrix(units, interventions),
               "is.character(interventions) is not TRUE",
               fixed = TRUE)

  units <- 1:3
  interventions <- c("Intervention1", "Intervention2", "Intervention3")

  expect_error(init_matrix(units, interventions),
               "is.character(units) is not TRUE",
               fixed = TRUE)
})
