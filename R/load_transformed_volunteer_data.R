load_transformed_volunteer_data <- function(data) {
  data |> 
    tibble::as_tibble() |>
    dplyr::select(
      timestamp,
      player = derby_name,
      player2 = who_are_you_though,
      month = month_of_volunteer_hours,
      volunteer_hrs = amount_of_volunteer_hours,
      purchased_hrs = how_many_hours_are_you_purchasing_10_hr_will_need_to_confirm_payment_before_credit_is_given,
      rollover_hrs = how_many_hours_will_you_be_rolling_over_this_month_from_last_month_keep_in_mind_that_you_cannot_roll_over_hours_if_you_have_already_completed_the_minimum_number_of_required_hours_for_the_current_month_you_can_roll_over_up_to_4_hours_from_the_previous_month_and_are_responsible_for_keeping_track_of_your_own_surplus_hours
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ dplyr::if_else(
          .x == "",
          NA,
          .x
        )
      )
    ) |>
    dplyr::mutate(
      timestamp = lubridate::mdy_hms(timestamp) |> lubridate::date(),
      year = lubridate::year(timestamp),
      month2 = lubridate::mdy(month) |> lubridate::month(label = TRUE),
      .before = timestamp
    ) |>
    dplyr::mutate(
      month3 = dplyr::if_else(
        is.na(month2),
        month,
        month2
      ) |>
        as.character() |>
        tolower()
    ) |>
    dplyr::mutate(
      month = dplyr::case_when(
        stringr::str_detect(month3, "jan") ~ "jan",
        stringr::str_detect(month3, "feb") ~ "feb",
        stringr::str_detect(month3, "mar") ~ "mar",
        stringr::str_detect(month3, "apr") ~ "apr",
        stringr::str_detect(month3, "may") ~ "may",
        stringr::str_detect(month3, "jun") ~ "jun",
        stringr::str_detect(month3, "jul") ~ "jul",
        stringr::str_detect(month3, "aug") ~ "aug",
        stringr::str_detect(month3, "sep") ~ "sep",
        stringr::str_detect(month3, "oct") ~ "oct",
        stringr::str_detect(month3, "nov") ~ "nov",
        stringr::str_detect(month3, "dec") ~ "dec",
        .default = NA
      ),
      month_year = lubridate::my(
        paste(month, year, sep = "-")
      ),
      player = dplyr::if_else(
        is.na(player),
        player2,
        player
      ) |> 
        tolower()
    ) |>
    dplyr::select(
      month_year,
      year,
      month,
      player,
      matches("hrs$")
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("hrs$"),
        ~ readr::parse_number(.x)
      ),
      total_hrs = rowSums(
        dplyr::across(
          dplyr::matches("hrs$")
        ),
        na.rm = TRUE
      ),
      .before = volunteer_hrs
    ) |>
    dplyr::group_by(
      month_year,
      year,
      month,
      player
    ) |>
    dplyr::reframe(
      dplyr::across(
        dplyr::matches("hrs$"),
        ~ sum(.x, na.rm = TRUE)
      )
    )
}