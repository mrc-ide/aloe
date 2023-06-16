test_that("matrix creation works", {
  units <- letters
  interventions <- c("tx", "itn", "smc")
  choice_matrix <- create_choice_matrix(
    units = units,
    interventions = interventions
  )
  expect_type(choice_matrix, "logical")
  expect_equal(dim(choice_matrix), c(26, 3))
  expect_equal(mean(choice_matrix), 0)
  expect_equal(colnames(choice_matrix), interventions)
  expect_equal(rownames(choice_matrix), units)
})

test_that("matrix reversal works", {
  m <- matrix(c(TRUE, FALSE, TRUE, FALSE), ncol = 2)
  rownames(m) <- c("a", "b")
  colnames(m) <- c("tx", "itn")

  # Reverse all
  expect_equal(
    reverse_selection(
      choice_matrix = m
    ),
    !m
  )

  # Reverse selected row
  m2 <- m
  m2[1,] <- !m2[1,]
  expect_equal(
    reverse_selection(
      choice_matrix = m,
      rows = "a"
    ),
    m2
  )

  # Reverse selected col
  m3 <- m
  m3[,1] <- !m3[,1]
  expect_equal(
    reverse_selection(
      choice_matrix = m,
      cols = "tx"
    ),
    m3
  )

  # Reverse selected row and col
  m4 <- m
  m4[1, 1] <- !m4[1, 1]
  expect_equal(
    reverse_selection(
      choice_matrix = m,
      rows = "a",
      cols = "tx"
    ),
    m4
  )
})

test_that("matrix clear works", {
  m <- matrix(c(TRUE, FALSE, TRUE, FALSE), ncol = 2)
  rownames(m) <- c("a", "b")
  colnames(m) <- c("tx", "itn")

  # Reverse all
  m1 <- m
  m1[,] <- FALSE
  expect_equal(
    clear_selection(
      choice_matrix = m
    ),
    m1
  )

  # Reverse selected row
  m2 <- m
  m2[1,] <- FALSE
  expect_equal(
    clear_selection(
      choice_matrix = m,
      rows = "a"
    ),
    m2
  )

  # Reverse selected col
  m3 <- m
  m3[,1] <- FALSE
  expect_equal(
    clear_selection(
      choice_matrix = m,
      cols = "tx"
    ),
    m3
  )

  # Reverse selected row and col
  m4 <- m
  m4[1, 1] <- FALSE
  expect_equal(
    clear_selection(
      choice_matrix = m,
      rows = "a",
      cols = "tx"
    ),
    m4
  )
})


test_that("matrix all works", {
  m <- matrix(c(TRUE, FALSE, TRUE, FALSE), ncol = 2)
  rownames(m) <- c("a", "b")
  colnames(m) <- c("tx", "itn")

  # Reverse all
  m1 <- m
  m1[,] <- TRUE
   expect_equal(
    all_selection(
      choice_matrix = m
    ),
    m1
  )

  # Reverse selected row
  m2 <- m
  m2[1,] <- TRUE
  expect_equal(
    all_selection(
      choice_matrix = m,
      rows = "a"
    ),
    m2
  )

  # Reverse selected col
  m3 <- m
  m3[,1] <- TRUE
  expect_equal(
    all_selection(
      choice_matrix = m,
      cols = "tx"
    ),
    m3
  )

  # Reverse selected row and col
  m4 <- m
  m4[1, 1] <- TRUE
  expect_equal(
    all_selection(
      choice_matrix = m,
      rows = "a",
      cols = "tx"
    ),
    m4
  )
})
