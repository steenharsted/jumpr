# add_jump_length_and_jump_height ----
#' Calculate measures of jump length and jump height
#'
#' Adds the columns \code{jump_length} and \code{jump_height} to a tibble containg mocap data of a jump.
#' @param .data A tibble containing motion-capture data from a jump.\cr
#' The data must contain the following columns:
#' * \code{jump_events} You can create this column using \code{\link{add_jump_events}}
#' * \code{marks} A character column containg one or more of c("TOL", "TOR", "TOB"), AND one or more of c("FFL", "FFR", "FFB")
#' * Global spatial ankle joint center positions in the floor plane: \code{LAX} \code{LAZ} \code{RAX} \code{RAZ}
#' * Global spatial hip joint-center height positions: \code{LHY} \code{RHY}
#' @param method The method that should be used to calculate jump length.
#'
#' @return The tibble suplied in \code{.data} argument with the added columns \code{jump_length} and \code{jump_height} both measures are in cm.
#' @export
#'
#' @examples
#' # Prepare data
#' df <- dplyr::filter(mocapr::mocapr_data, movement_nr == 1)
#' df <- add_phases_jump(df)
#' df <- mocapr::project_full_body_to_MP(df)
#'
#' add_jump_length_and_height(df)
#'
add_jump_length_and_height <- function(.data, method = "movement_plane"){
  # Avoid "No visible binding for global variable ..." when running check()
  LAZ <- LAX <- RAZ <- RAX <- LHY <- RHY <- jump_events <- phase <- NULL
  events_b <- dummy <- . <- jump_length <- frame <- marks <- phase_b <- NULL
  jump_height <- NULL

  # Check method argument
  if(!method %in% c("movement_plane", "global")){
    stop("the method argument must be either 'movement_plane' or 'global'.")
  }


  if(method == "movement_plane") {

    # Check inputs

    if(suppressWarnings(is.null(.data$events_b))) {
      stop(".data is missing the column 'events_b'. You probably need to run 'mocapr::add_phases_jump()' first.")
    }

    if(!any(.data$events_b == "TO" | .data$events_b == "FF", na.rm = TRUE)){
      stop("The column 'events_b' must contain the values 'TO' and 'FF' in order to calculate jump length and jump height")
    }

    if(suppressWarnings(any(is.null(.data$LA_MPF), is.null(.data$RA_MPF)))) {
      stop(".data must contain the columns 'LA_MPF' and 'RA_MPF'. You probably need to run 'mocapr::add_project_full_body_to_MP()' first.")
    }

    if(suppressWarnings(any(is.null(.data$LHY), is.null(.data$RHY)))) {
      stop(".data must contain the columns 'LHY' and 'RHY', these columns contain the height coordinates of the two hip-joints.")
    }


    # In order to calculate the jump length and height we need use the last frame with TO and the first frame with FF
    df <- .data %>%
      dplyr::filter(events_b == "TO" | events_b == "FF") %>%
      dplyr::group_by(events_b) %>%
      dplyr::mutate(
        dummy = dplyr::case_when(
          events_b == "TO" & frame == max(frame) ~ 1,
          events_b == "FF" & frame == min(frame) ~ 1,
          TRUE ~ 0
        )) %>%
      dplyr::ungroup() %>%
      dplyr::filter(dummy == 1)

    # Determine what positions needs to be used for start point and end point for jump.
    # The positions needs to be different based on wheter or not the subject has a bilateral or a unilateral take off and landing
    # finally divide by 10 because output should be in cm
    jump_length_value <- df %>%
      dplyr::mutate(
        position = dplyr::case_when(
          marks == "TOR" ~ RA_MPF,
          marks == "TOL" ~ LA_MPF,
          marks == "TOB" ~ (RA_MPF+LA_MPF)/2,
          marks == "FFR" ~ RA_MPF,
          marks == "FFL" ~ LA_MPF,
          marks == "FFB" ~ (RA_MPF+LA_MPF)/2)) %>%
      dplyr::mutate(
        jump_length = abs(dplyr::first(.$position)-dplyr::last(.$position))/10
        ) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::pull(jump_length)


    jump_height_value <- .data %>%
      dplyr::select(frame, marks, events_b, phase_b, LHY, RHY) %>%
      dplyr::filter(phase_b == 4 | phase_b == 6) %>%
      dplyr::arrange(frame) %>%
      dplyr::mutate(HAY = (LHY+RHY)/2) %>%
      dplyr::mutate(jump_height = (dplyr::last(.$HAY) - dplyr::first(.$HAY))/10) %>%
      dplyr::slice(1) %>%
      dplyr::pull(jump_height)


    df_return <- .data %>%
      dplyr::mutate(
        jump_length = round(jump_length_value, 1),
        jump_height = round(jump_height_value, 1)
      )

    return(df_return)
  }


  if(method == "global"){
    warning("You are using a method of calcularing jump distance that has been deprecated since mocapr version 1.9007")
    # Check inputs
    if(suppressWarnings(is.null(.data$jump_events))) {
      stop(".data is missing the column 'jump_events'. You probably need to run 'mocapr::add_jump_events()' first.")
    }

    if(!any(.data$jump_events == "toe_off" | .data$jump_events == "flat_foot", na.rm = TRUE)){
      stop("The column 'jump_events' must contain the values 'toe_off' and 'flat_foot' in order to calculate jump length and jump height")
    }

    # Function
    df_1 <- .data %>%
      dplyr::filter(jump_events == "toe_off" |  jump_events == "flat_foot")

    #Define start point
    # The value of df_1$marks[1] tells if left or right or both feet were used at toe off
    # The start value is set at the ankle joint of the foot with last floor contact
    # If both feet touch the ground at toe off a point between the two ankle joints is
    # used as starting point
    if(df_1$marks[1] == "TOB"){
      start <- df_1 %>%
        dplyr::summarise(
          start_X = (dplyr::first(LAX) + dplyr::first(RAX)) / 2,
          start_Z = (dplyr::first(LAZ) + dplyr::first(RAZ)) / 2)
    }
    if(df_1$marks[1] == "TOL"){
      start <- df_1 %>%
        dplyr::summarise(
          start_X = dplyr::first(LAX),
          start_Z = dplyr::first(LAZ))
    }
    if(df_1$marks[1] == "TOR"){
      start <- df_1 %>%
        dplyr::summarise(
          start_X = dplyr::first(RAX),
          start_Z = dplyr::first(RAZ))
    }
    #Define end point
    # The value df_1$marks[2] tells what foot or if both has flat-foot contact first when landing
    if(df_1$marks[2] == "FFB"){
      end <- df_1 %>%
        dplyr::summarise(
          end_X = (dplyr::last(LAX) + dplyr::last(RAX)) / 2,
          end_Z = (dplyr::last(LAZ) + dplyr::last(RAZ)) / 2)
    }
    if(df_1$marks[2] == "FFL"){
      end <- df_1 %>%
        dplyr::summarise(
          end_X = dplyr::last(LAX),
          end_Z = dplyr::last(LAZ))
    }
    if(df_1$marks[2] == "FFR"){
      end <- df_1 %>%
        dplyr::summarise(
          end_X = dplyr::last(RAX),
          end_Z = dplyr::last(RAZ))
    }

    #Calculate Jump Height Value
    hip_height_start <-
      .data %>% dplyr::filter(phase==4) %>%
      dplyr::summarise(
        value  = (LHY+ RHY)/2) %>%
      dplyr::pull()

    hip_height_max  <-  .data %>%
      dplyr::filter(phase==5) %>%
      dplyr::summarise(
        value  = max(( LHY + RHY)/2)) %>%
      dplyr::pull()

    .data %>%
      dplyr::mutate(
        jump_length = round(sqrt((start$start_X - end$end_X)^2 + (start$start_Z - end$end_Z)^2 )/10, 1),
        jump_height = round( (hip_height_max - hip_height_start)/10, 1))
  }
}
