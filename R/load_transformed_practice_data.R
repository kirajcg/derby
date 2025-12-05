load_transformed_practice_data <- function(data, meta) {
  
  data |>
    tibble::as_tibble() |> 
    dplyr::select(
      timestamp,
      p_date = practice_date,
      p_type = practice_type,
      off_skates,
      in_attendance,
      duration_pop = how_long_was_your_pop_up,
      duration_osw = how_long_did_you_work_out
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
      timestamp = lubridate::mdy_hms(timestamp) |>
        lubridate::as_date(),
      p_date = lubridate::mdy(p_date),
      p_type = tolower(p_type)
    ) |> 
    dplyr::mutate(
      p_type2 = case_when(
        stringr::str_detect(p_type, "\\(bw\\)") ~ "bw",
        stringr::str_detect(p_type, "\\(di\\)") ~ "di",
        stringr::str_detect(p_type, "\\(fmc\\)") ~ "fmc",
        stringr::str_detect(p_type, "\\(lge\\)") ~ "lge",
        stringr::str_detect(p_type, "\\(lm\\)") ~ "lm",
        stringr::str_detect(p_type, "\\(osw\\)") ~ "osw",
        stringr::str_detect(p_type, "\\(po\\)") ~ "pop",
        stringr::str_detect(p_type, "\\(star\\)") ~ "star",
        .default = "Uncategorized"
      ),
      .after = p_type
    ) |> 
    dplyr::mutate(
      error = dplyr::if_else(
        lubridate::year(timestamp) != lubridate::year(p_date),
        TRUE,
        FALSE
      ),
      .before = 1
    )
}