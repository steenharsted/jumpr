test_that("add_jump_length_and_jump_height works with recommended method", {

  # Test method argument
  my_test_df <- data.frame(frame = seq(1:10))
  expect_error(add_jump_length_and_height(my_test_df, method = "bla"))

  # Test input
  expect_error(add_jump_length_and_height(my_test_df))

  my_test_df$events_b <- NA
  expect_error(add_jump_length_and_height(my_test_df))

  my_test_df$events_b <- c(NA, NA, NA, "TO", NA, NA, "FF", NA, NA, NA)
  expect_error(add_jump_length_and_height(my_test_df))

  my_test_df$LA_MPF <- c(0, 0, 0, 0, 100, 150, 200, 200, 200, 200)
  my_test_df$RA_MPF <- c(0, 0, 0, 0, 100, 150, 200, 200, 200, 200)
  expect_error(add_jump_length_and_height(my_test_df))

  my_test_df$LHY <- c(100, 75, 50, 100, 125, 150, 100, 75, 50, 100)
  my_test_df$RHY <- c(100, 75, 50, 100, 125, 150, 100, 75, 50, 100)
  expect_error(add_jump_length_and_height(my_test_df))

  my_test_df$marks <- c(NA, NA, NA, "TOB", NA, NA, "FFB", NA, NA, NA)
  my_test_df$phase_b <- c(1, 1,  2,     4,  5,  6,     7,  8,  9, 10)
  my_test_df <- add_jump_length_and_height(my_test_df)

  expect_equal(my_test_df$jump_length[1], 20)
  expect_equal(my_test_df$jump_height[1], 5)


  # Create a jump with bilateral take off and landing
  df <- mocapr::mocapr_data
  df <- df %>% dplyr::filter(movement_nr == 1)
  df <- mocapr::project_full_body_to_MP(df)
  df <- add_phases_jump(df)


  # Create a jump with unilateral take off and landing
  df2 <- mocapr::mocapr_data
  df2 <- df2 %>% dplyr::filter(movement_nr == 2)
  df2 <- df2 %>%
    dplyr::mutate(
      marks = NA,
      marks = ifelse(frame == 59, "TOL", marks),
      marks = ifelse(frame == 61, "TOR", marks),
      marks = ifelse(frame == 85, "FFL", marks),
      marks = ifelse(frame == 87, "FFR", marks))
  df2 <- mocapr::project_full_body_to_MP(df2)
  df2 <- add_phases_jump(df2)

  add_jump_length_and_height(df)
  add_jump_length_and_height(df2)


  })


test_that("add_jump_length_and_jump_height works with global method", {

  # Take of and landing with both feet
  df <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, NA,"TOB",  NA, "FFB",  NA,  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100,  75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100,  75,  50, 100),
    LAX =   c(  0,  0,  0,    0,  50,   100, 100, 100, 100),
    RAX =   c(  0,  0,  0,    0,  50,   100, 100, 100, 100),
    LAZ =   rep(-10,9),
    RAZ =   rep( 10,9))

  # Test that the function fails if add_jump_events() is not run before
  expect_warning(expect_error(add_jump_length_and_height(df, method = "global")))

  # Test that jump_events contains the needed values if it exists
  df_test1 <- df_test2 <- df
  df_test1$jump_events <- "BAD"
  df_test1$jump_events <- NA
   expect_warning(expect_error((add_jump_length_and_height(df_test1, method = "global"))))
   expect_warning(expect_error((add_jump_length_and_height(df_test2, method = "global"))))

  # Run add_jump_events
  df <- suppressWarnings(add_jump_events(df))
  df <- suppressWarnings(add_jump_length_and_height(df, method = "global"))

  expect_equal(unique(df$jump_length), 10)
  expect_equal(unique(df$jump_height), 5)

  # Single leg take off and landing
  df <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, "TOL","TOR",  NA, "FFL",  "FFR",  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100,  75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100,  75,  50, 100),
    LAX =   c(  0,  0,  0,    0,  50,   100, 100, 100, 100),
    RAX =   c(  0,  0,  0,    0,  50,   100, 100, 100, 100),
    LAZ =   rep(-10,9),
    RAZ =   rep( 10,9))

  df <- add_jump_events(df)
  df <- suppressWarnings(add_jump_length_and_height(df, method = "global"))

  expect_equal(unique(df$jump_length), 10.2)
  expect_equal(unique(df$jump_height), 5)


  df <- tibble::tibble(
    frame = c(1:9),
    marks = c( NA, NA, "TOR","TOL",  NA, "FFR",  "FFL",  NA,  NA),
    LHY =   c(100, 50, 75,  100, 150,   100,  75,  50, 100),
    RHY =   c(100, 50, 75,  100, 150,   100,  75,  50, 100),
    LAX =   c(  0,  0,  0,    0,  50,   100, 100, 100, 100),
    RAX =   c(  0,  0,  0,    0,  50,   100, 100, 100, 100),
    LAZ =   rep(-10,9),
    RAZ =   rep( 10,9))

  df <- add_jump_events(df)
  df <- suppressWarnings(add_jump_length_and_height(df, method = "global"))

  expect_equal(unique(df$jump_length), 10.2)
  expect_equal(unique(df$jump_height), 5)
  })



