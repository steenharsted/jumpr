test_that("add_phases_jump work", {
  # Test input
  my_test_df <- data.frame(frame = 1:15)
  expect_error(add_phases_jump(my_test_df))

  my_test_df$marks <- NA
  expect_error(add_phases_jump(my_test_df))

  my_test_df$marks[4] <- "TOB"
  expect_error(add_phases_jump(my_test_df))

  my_test_df$marks[8] <- "FFB"
  expect_error(add_phases_jump(my_test_df))


  df <- tibble::tibble(
    frame = c(1:11),
    marks = c( NA, NA, NA,"TOB",  NA,  NA,  NA, "FFB", NA,  NA,  NA),
    LHY =   c(100, 50, 75,  100, 125, 150, 125,   100, 75,  50, 100),
    RHY =   c(100, 50, 75,  100, 125, 150, 125,   100, 75,  50, 100)
  )

  df2 <- tibble::tibble(
    frame = c(1:15),
    marks = c( NA, NA, NA, "TOL", "TOR",  NA,  NA,  NA,  NA, "FFL", "FFR",  NA,  NA, NA,  NA),
    LHY =   c(100, 50, 50,    75,  100,  125, 150, 150, 125,   100,    75,  70,  50, 50, 100),
    RHY =   c(100, 50, 50,    75,  100,  125, 150, 150, 125,   100,    75,  70,  50, 50, 100)
  )

  df <- add_phases_jump(df)
  df2 <- add_phases_jump(df2)

  expect_equal(df$events_b, c(NA, "DP", NA,  "TO", NA,   "MH", NA,   "FF", NA,   "DL", NA ))
  expect_equal(df2$phase_b, c(1,  2,  3, NA,  4,  5,  6,  7,  7, NA,  8,  9,  9, 10, 11))

  # Test problematic data (gave error earlier)
  # This data has uneven landing

  df3 <- data.frame(
    frame = seq(1:15),
    marks = c(NA, NA, NA,"TOR", "TOL",  NA,  NA,  NA,"FFR",  NA, NA, "FFL", NA, NA, NA),
    LHY   = c(10,  7,  5,    7,    10,  12,  15,  12,   11, 10,  10,  9, 10, 11, 11),
    RHY   = c(10,  7,  5,    7,    10,  12,  15,  12,   11, 10,  10,  9, 10, 11, 11))

  df3 <- add_phases_jump(df3)

  expect_equal(df3$phase_b, c(1,1,2,NA,4,5,6,7,NA,NA,NA,8,10,11,11))

  df4 <- data.frame(
    frame = seq(1:15),
    marks = c(NA, NA, NA,"TOR", "TOL",  NA,  NA,  NA,"FFR",  NA, NA, "FFL", NA, NA, NA),
    LHY   = c(10,  7,  5,    7,    15,  12,  14,  12,   11, 10,  10,  9, 10, 11, 11),
    RHY   = c(10,  7,  5,    7,    15,  12,  14,  12,   11, 10,  10,  9, 10, 11, 11))

  df4 <- add_phases_jump(df4)
  expect_equal(df4$phase_b, c(1, 1, 2, NA, 4, 5, 6, 7, NA, NA, NA, 8, 10, 11, 11))
})


test_that("add_jump_events works", {
  df <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, NA,"TOB",  NA, "FFB", NA,  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100)
  )

  df2 <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, "TOL", "TOR",  NA, "FFL", "FFR",  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100)
  )

  df <- add_jump_events(df)
  df2 <- add_jump_events(df2)

  expect_equal(unique(df$phase), c(1,2,3,4,5,6,7,8,9))
  expect_equal(unique(df2$phase), c(1,2,3,4,5,6,7,8,9))
  expect_equal(df$jump_events, c(NA, "deepest_prep", NA, "toe_off", "highest_pos", "flat_foot", NA, "deepest_land", NA))
  expect_equal(df2$jump_events, c(NA, "deepest_prep", "first_toe_off", "toe_off", "highest_pos", "flat_foot", "last_flat_foot", "deepest_land", NA))

  # Expect errors
  df1 <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, NA,"TOB",  NA, "FFB", "FFL",  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100)
  )

  df2 <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, NA, NA,  NA, "FFB", NA,  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100, 75,  50, 100)
  )

  expect_error(add_jump_events(df1))
  expect_error(add_jump_events(df2))

})

