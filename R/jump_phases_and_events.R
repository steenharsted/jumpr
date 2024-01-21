#' Divide a jump into phases
#'
#' \code{add_phases_jump} uses average hip joint-center height and supplied events to divide a jump into 11 phases.
#' This division is useful for further analyis of the individual phases in the jump.
#'
#' Three phase columns are generated \code{phase_l, phase_r, phase_b}, describing the phases of the left leg, right leg, and both extremities respectively.
#'
#' The numerical values in \code{phase} column correspond to:
#' 1. Descending phase of the preparation
#' 1. The deepest postion in the preparation phase (A single frame)
#' 1. The ascending phase of the preparation (push-off)
#' 1. Toe off - the last fase with contact between the feet and the ground (A single frame)
#' 1. The ascending phase of the hang time
#' 1. The highest position of the hang time (A single frame)
#' 1. The descending phase of the hang time
#' 1. Impact (A single frame)
#' 1. Descending phase of the landing.
#' 1. Deepest position of the landing (A single frame)
#' 1. Ascending phase of the landing.
#'
#' The values in the column \code{phase_b} will be identical to the values in \code{phase_l} and \code{phase_r} if the subject is taking off
#' and landing simultaneously with both extremities. If this is not the case \code{phase_b} will take the value \code{4} at the frame where
#' the last foot has contact with the ground, and the value \code{8} at the frame where the last foot has impact. \code{NA} values will be given to phases
#' where there is transition (e.g. left foot still has contact with ground, while right foot is in the air.)
#'
#' @param .data A tibble containing motioncapture data from a jump. The data must contain:\cr
#' * \code{LHY} and \code{RHY} columns containg global spatial joint-center positions of the left and right hip\cr
#' * A \code{frame} column\cr
#' * A \code{marks} column with values descring toe off events (TOL = toe off of left foot, TOR = toe off of right foot, TOB = toe off from both feet),
#' and impact events (FFL = flat foot contact with left foot, FFR = flat foot contact with right foot, FFB = flat foot contact with both feet).
#'
#' @return The tibble supplied in the \code{.data argument}, with the added columns \code{phase_l}, \code{phase_r}, \code{phase_b},
#'  \code{events_l}, \code{events_r}, \code{events_b}
#' @export
#'
#' @examples
#' # With synthetic data
#' df <- tibble::tibble(
#'        frame = c(1:11),
#'        marks = c( NA, NA, NA,"TOB",  NA,  NA,  NA, "FFB", NA,  NA,  NA),
#'        LHY =   c(100, 50, 75,  100, 125, 150, 125,   100, 75,  50, 100),
#'        RHY =   c(100, 50, 75,  100, 125, 150, 125,   100, 75,  50, 100))
#'
#' df2 <- tibble::tibble(
#'         frame = c(1:15),
#'         marks = c( NA, NA, NA, "TOL", "TOR",  NA,  NA,  NA,  NA, "FFL", "FFR",  NA,  NA, NA,  NA),
#'         LHY =   c(100, 50, 50,    75,  100,  125, 150, 150, 125,   100,    75,  70,  50, 50, 100),
#'         RHY =   c(100, 50, 50,    75,  100,  125, 150, 150, 125,   100,    75,  70,  50, 50, 100))
#' add_phases_jump(df)
#' add_phases_jump(df2)
#'
#' # With real mocap data
#'  df3 <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#'  df3 <- dplyr::select(df, frame, marks, LHY, RHY)
#'
#'  add_phases_jump(df3)
#'
#' # A plot displaying the phases
#' df4 <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df4 <- add_phases_jump(df4)
#'  mocapr::animate_global(df4,
#'                         planes = "X",
#'                         remove_facet_labels = FALSE,
#'                         return_plot = TRUE,
#'                         col_facets = phase_b)
add_phases_jump <- function(.data){
  # Avoid no global binding
  marks <- toe_offs <- impacts <- events_l <- events_r <- LHY <- RHY <- phase_l <- phase_r <- NULL
  .dummy_HA_prep <- .dummy_HA_air <- .dummy_HA_land <- NULL

  # Check inputs
  # Test if marks contain the needed elements
  .test <-
    data.frame(
      marks = unique(.data$marks)
    ) %>%
    dplyr::mutate(
      toe_offs = dplyr::if_else(marks == "TOB", 2, 0, missing = 0),
      impacts = dplyr::if_else(marks == "FFB", 2, 0, missing =0 ),
      toe_offs = dplyr::if_else(marks %in% c("TOL", "TOR"), 1, toe_offs),
      impacts = dplyr::if_else(marks %in% c("FFL", "FFR"), 1, impacts)
    ) %>%
    dplyr::summarise_if(is.numeric, sum)

  if(.test[["toe_offs"]] != 2) {
    stop("add_jump_events() requires the column marks to contain one row with 'TOB', or one row with 'TOL' and one row with 'TOR'")
  }

  if(.test[["impacts"]] != 2) {
    stop("add_jump_events() requires the column marks to contain one row with 'FFB', or one row with 'FFL' and one row with 'FFR'")
  }


  # Function
  df <- .data %>%


    # The events from both extremities is needed in order to define the jump phases
    dplyr::mutate(
      events_l = dplyr::if_else(marks == "TOB" | marks == "TOL", "TO", "TBD"),
      events_r = dplyr::if_else(marks == "TOB" | marks == "TOR", "TO", "TBD"),
      events_l = dplyr::if_else(marks == "FFB" | marks == "FFL", "FF", events_l, "TBD"),
      events_r = dplyr::if_else(marks == "FFB" | marks == "FFR", "FF", events_r, "TBD")
      ) %>%

    # First create a rough phase grouping
    dplyr::mutate(
      phase_l = dplyr::case_when(
        frame < frame[events_l == "TO"] ~ 3,
        frame > frame[events_l == "FF"] ~ 9,
        frame > frame[events_l == "TO"] & frame < frame[events_l == "FF"] ~ 5,
        TRUE ~ NA_real_),
      phase_r = dplyr::case_when(
        frame < frame[events_r == "TO"] ~ 3,
        frame > frame[events_r == "FF"] ~ 9,
        frame > frame[events_r == "TO"] & frame < frame[events_r == "FF"] ~ 5,
        TRUE ~ NA_real_)
    )


    # Fine tune the preparation phase

  df_prep <- df %>%
    dplyr::mutate(
      .dummy_HA_prep = LHY+RHY,
      .dummy_HA_prep = dplyr::if_else(phase_l == 3 & phase_r == 3, .dummy_HA_prep, 10000, missing = 10000),
      events_l = ifelse(phase_l == 3 & (LHY+RHY) == min(.dummy_HA_prep), "DP", events_l),
      events_r = ifelse(phase_r == 3 & (LHY+RHY) == min(.dummy_HA_prep), "DP", events_r),

      # Remove possible duplicates
      events_l = replace(events_l, duplicated(events_l), "TBD"),
      events_r = replace(events_r, duplicated(events_r), "TBD"),

      phase_l = dplyr::case_when(
      events_l == "TO" ~ 4,
      events_l == "DP" ~ 2,
      frame < frame[events_l == "DP"] ~ 1,
      frame > frame[events_l == "DP"] & frame < frame[events_l == "TO"] ~ 3,
      TRUE ~ phase_l),
    phase_r = dplyr::case_when(
      events_r == "TO" ~ 4,
      events_r == "DP" ~ 2,
      frame < frame[events_r == "DP"] ~ 1,
      frame > frame[events_r == "DP"] & frame < frame[events_r == "TO"] ~ 3,
      TRUE ~ phase_r)
    ) %>%
    dplyr::select(-.dummy_HA_prep)

    # Fine tune the air phase
  df_air <- df_prep %>%
    dplyr::mutate(
      .dummy_HA_air = LHY+RHY,
      .dummy_HA_air = dplyr::if_else(phase_l == 5 & phase_r == 5, .dummy_HA_air, -10000, missing = -10000),
      events_l = ifelse(phase_l == 5 & (LHY+RHY) == max(.dummy_HA_air), "MH", events_l),
      events_r = ifelse(phase_r == 5 & (LHY+RHY) == max(.dummy_HA_air), "MH", events_r),

      # Remove possible duplicates
      events_l = replace(events_l, duplicated(events_l), "TBD"),
      events_r = replace(events_r, duplicated(events_r), "TBD"),

      phase_l = dplyr::case_when(
        events_l == "MH" ~ 6,
        events_l == "FF" ~ 8,
        frame < frame[events_l == "MH"] & phase_l == 5 ~ 5,
        frame > frame[events_l == "MH"] & phase_l == 5 ~ 7,
        TRUE ~ phase_l),
      phase_r = dplyr::case_when(
        events_r == "MH" ~ 6,
        events_r == "FF" ~ 8,
        frame < frame[events_r == "MH"] & phase_r == 5 ~ 5,
        frame > frame[events_r == "MH"] & phase_r == 5 ~ 7,
        TRUE ~ phase_r)
      ) %>%
    dplyr::select(-.dummy_HA_air)

  # Fine tune the landing phase
  df_land <- df_air %>%
    dplyr::mutate(
      .dummy_HA_land = LHY+RHY,
      .dummy_HA_land = dplyr::if_else(phase_l == 9 & phase_r == 9, .dummy_HA_land, 10000, missing = 10000),
      events_l = ifelse(phase_l == 9 & phase_r == 9 & (LHY+RHY) == min(.dummy_HA_land), "DL", events_l),
      events_r = ifelse(phase_l == 9 & phase_r == 9 & (LHY+RHY) == min(.dummy_HA_land), "DL", events_r),

      # Remove possible duplicates
      events_l = replace(events_l, duplicated(events_l, fromLast = TRUE), "TBD"),
      events_r = replace(events_r, duplicated(events_r, fromLast = TRUE), "TBD"),

      phase_l = dplyr::case_when(
        events_l == "DL" ~ 10,
        frame < frame[events_l == "DL"] & phase_l == 9 ~ 9,
        frame > frame[events_l == "DL"] & phase_l == 9 ~ 11,
        TRUE ~ phase_l),
      phase_r = dplyr::case_when(
        events_r == "DL" ~ 10,
        frame < frame[events_r == "DL"] & phase_r == 9 ~ 9,
        frame > frame[events_r == "DL"] & phase_r == 9 ~ 11,
        TRUE ~ phase_r)
      ) %>%
    dplyr::select(-.dummy_HA_land)

  # Create the bilateral phase and events using phase and events from both extremities
  df_land %>%
    dplyr::mutate(
      events_l = dplyr::recode(events_l, TBD = NA_character_),
      events_r = dplyr::recode(events_r, TBD = NA_character_),
      events_b = dplyr::case_when(
        events_l == events_r ~ events_l,
        !is.na(events_l) & is.na(events_r) ~ events_l,
        is.na(events_l) & !is.na(events_r) ~ events_r,
        is.na(events_l) & is.na(events_r) ~ NA_character_,
        TRUE ~ "WARNING"),
      phase_b = dplyr::case_when(
        phase_l == phase_r ~ phase_l,
        dplyr::row_number() == dplyr::last(which(events_b %in% "TO")) ~ 4,
        dplyr::row_number() == dplyr::last(which(events_b %in% "FF")) ~ 8,
        TRUE ~ NA_real_)
    )

}



# add_jump_events----
#' Divide a jump into phases
#'
#' \code{add_jump_events} uses average hip joint-center height and supplied events to divide a jump into 9 phases.
#' This division is useful for further analyis of the individual phases in the jump.
#' The numerical values in \code{phase} column correspond to:
#' 1. Descending phase of the preparation
#' 1. The deepest postion in the preparation phase
#' 1. The ascending phase of the preparation (push-off)
#' 1. Toe off - the last fase with contact between the feet and the ground
#' 1. Hang time.
#' 1. Impact
#' 1. Descending phase of the landing.
#' 1. Deepest position of the landing.
#' 1. Ascending phase of the landing.
#'
#' @param .data A tibble containing motioncapture data from a jump. The data must contain:\cr
#' * \code{LHY} and \code{RHY} columns containg global spatial joint-center positions of the left and right hip\cr
#' * A \code{frame} column\cr
#' * A \code{marks} column with values descring toe off events (TOL = toe off of left foot, TOR = toe off of right foot, TOB = toe off from both feet),
#' and impact events (FFL = flat foot contact with left foot, FFR = flat foot contact with right foot, FFB = flat foot contact with both feet).
#'
#' @return The tibble supplied in the \code{.data argument}, with the added columns \code{phase} and \code{jump_events}
#' @export
#'
#' @examples
#' # Prepare data
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df <- dplyr::select(df, frame, marks, LHY, RHY)
#'
#' suppressMessages(add_jump_events(df))
#'
#' # A plot displaying the phases
#' df2 <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df2 <- suppressMessages(add_jump_events(df2))
#'
#' mocapr::animate_global(df2,
#'                        planes = "X",
#'                        remove_facet_labels = FALSE,
#'                        return_plot = TRUE,
#'                        col_facets = phase)
add_jump_events <- function(.data){

  message("This function has been deprecated since mocapr version 1.9006 available on Github since 2019-12-01. Consider using add_phases_jump() instead")

  # Avoid "No visible binding for global variable ..." when performing check()
  LHY <- RHY <- marks <- frame <- phase <- .HAY <- NULL
  HAY_during_TAKE_OFF <- HAY_during_LANDING <- NULL
  toe_offs <- impacts <- NULL

  # Test if marks contain the needed elements
  .test <-
    data.frame(
      marks = unique(.data$marks)
      ) %>%
    dplyr::mutate(
      toe_offs = dplyr::if_else(marks == "TOB", 2, 0, missing = 0),
      impacts = dplyr::if_else(marks == "FFB", 2, 0, missing =0 ),
      toe_offs = dplyr::if_else(marks %in% c("TOL", "TOR"), 1, toe_offs),
      impacts = dplyr::if_else(marks %in% c("FFL", "FFR"), 1, impacts)
      ) %>%
    dplyr::summarise_if(is.numeric, sum)

  if(.test[["toe_offs"]] != 2) {
    stop("add_jump_events() requires the column marks to contain one row with 'TOB', or one row with 'TOL' and one row with 'TOR'")
  }

  if(.test[["impacts"]] != 2) {
    stop("add_jump_events() requires the column marks to contain one row with 'FFB', or one row with 'FFL' and one row with 'FFR'")
  }


  # Function
  df <- .data %>%
    # Generate the averaged hip height
    dplyr::mutate(.HAY = (LHY+RHY)/2)

  #Create a small dataframe that only contains rows with toe off events (TO[L|R|B])
  TOE_OFF <- df %>%
    dplyr::filter(stringr::str_detect(marks, 'TO')) %>%
    dplyr::filter(marks == dplyr::last(marks) | marks == dplyr::first(marks)) %>%
    dplyr::select(frame, marks) %>%
    dplyr::arrange(frame)


  ## If toe off of the feet happens simultaneously, add a second row identical to the first row
  if(nrow(TOE_OFF) == 1){
    TOE_OFF <- TOE_OFF %>%
      dplyr::slice(rep(1:dplyr::n(), each=2))
  }

  #Create a small dataframe that only contains rows with flat foot events (FF[L|R|B])
  FLAT_FOOT <- df %>%
    dplyr::filter(stringr::str_detect(marks, 'FF')) %>%
    dplyr::filter(marks == dplyr::first(marks) | marks == dplyr::last(marks)) %>%
    dplyr::select(frame, marks) %>%
    dplyr::arrange(frame)

  ## If flat foot contact of the feet happens simultaneously, add a second row identical to the first row
  if(nrow(FLAT_FOOT) == 1){
    FLAT_FOOT <- FLAT_FOOT %>%
      dplyr::slice(rep(1:dplyr::n(), each=2))
  }

  ##create jump_events so that last TO[L|R|B] will be toe off and
  ##first FF[L|R|B] will be flat_foot
  df <- df %>%
    dplyr::mutate(
      jump_events = dplyr::case_when(
        frame == TOE_OFF[[2,1]] ~ "toe_off",
        frame == TOE_OFF[[1,1]] ~ "first_toe_off",
        frame == FLAT_FOOT[[1,1]] ~ "flat_foot",
        frame == FLAT_FOOT[[2,1]] ~ "last_flat_foot",
        TRUE ~ marks))




  #Generate Jump Phases----
  df <- df %>%
    #Define 4 (toe_off) and 6 (flat_foot)
    dplyr::mutate(
      phase = dplyr::case_when(
        jump_events == "toe_off" ~ 4,
        jump_events == "flat_foot" ~ 6,
        TRUE ~ 5)) %>%
    #Assign:
    #time before 4 as 3 and
    #time after 6 as 7
    dplyr::mutate(
      phase = dplyr::case_when(
        frame < frame[phase == 4] ~ 3,
        frame > frame[phase == 6] ~ 7,
        TRUE ~ phase)) %>%
    #Find the lowest position in phase 3 and give it value 2
    #Find the lowest position in phase 7 and give it value 8
    dplyr::mutate(
      #Generate HAY at
      HAY_during_TAKE_OFF = ifelse(phase == 3, .HAY, 10000),
      HAY_during_LANDING = ifelse(phase == 7, .HAY, 10000),
      phase = ifelse(HAY_during_TAKE_OFF == min(HAY_during_TAKE_OFF), 2, phase),
      phase = ifelse(HAY_during_LANDING == min(HAY_during_LANDING), 8, phase)) %>%
    #Assign:
    #time before 2 as 1
    #time after 8 as 9
    dplyr::mutate(
      phase = dplyr::case_when(
        frame < frame[phase == 2] ~ 1,
        frame > frame[phase == 8] ~ 9,
        TRUE ~ phase)) %>%
    dplyr::select(-HAY_during_TAKE_OFF, -HAY_during_LANDING) %>%
    dplyr::mutate(
      jump_events = dplyr::case_when(
        phase == 5 & .HAY == max(.HAY[phase ==5]) ~ "highest_pos",
        phase == 2 ~ "deepest_prep",
        phase == 8 ~ "deepest_land",
        TRUE ~ jump_events
        ))
  df <- df %>% dplyr::select(-.HAY)

  df
}
