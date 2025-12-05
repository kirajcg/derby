compute_volunteer_status <- function(meta, volunteer, game) {
  m <- meta |> dplyr::filter(game == !!game)
  
  volunteer |>
    dplyr::filter(
      year == lubridate::year(m$start) &
        stringr::str_detect(
          tolower(m$volunteer_month),
          month
        )
    ) |>
    dplyr::arrange(
      dplyr::desc(month_year)
    ) |>
    dplyr::select(
      - month,
      - month_year,
      - year
    ) |>
    dplyr::mutate(
      hrs_reqd = m$volunteer_hr_req,
      status = dplyr::if_else(
        total_hrs >= hrs_reqd,
        "Complete",
        paste(
          round(abs(total_hrs - hrs_reqd), 1),
          "h reqd"
        )
      ),
      .before = volunteer_hrs
    ) |>
    dplyr::select(
      player,
      status,
      volunteer_hrs,
      purchased_hrs,
      rollover_hrs
    ) |> 
    dplyr::mutate(
      game = game,
      .before = 1
    ) |> 
    dplyr::arrange(
      dplyr::desc(volunteer_hrs)
    )
}