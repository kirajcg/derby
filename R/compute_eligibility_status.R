compute_eligibility_status <- function(meta, practice, game) {
  
  m <- meta |> dplyr::filter(game == !!game)
  
  dat_practice <- practice |>
    dplyr::filter(
      p_date >= m$start &
        p_date <= m$end
    ) |>
    dplyr::filter(
      error == FALSE
    ) |>
    dplyr::select(
      - error
    ) |>
    tidyr::unite(
      "player",
      c(off_skates, in_attendance),
      sep = ", ",
      remove = FALSE
    ) |>
    tidyr::separate_longer_delim(
      player,
      delim = ", "
    ) |>
    dplyr::filter(
      player != "NA"
    ) |>
    dplyr::mutate(
      player = tolower(trimws(player)),
      player = case_when(
        stringr::str_detect(player, "mini") ~ "mini fridge",
        stringr::str_detect(player, "toxic") ~ "gin n. toxic",
        stringr::str_detect(player, "beaver") ~ "eager beaver",
        .default = player
      )
    ) |>
    dplyr::distinct()
  
  foo <- dat_practice |>
    dplyr::select(
      - in_attendance,
      - off_skates
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("^duration"),
        ~ dplyr::case_when(
          is.na(.) ~ 0,
          .default = readr::parse_number(.)
        ),
        .names = "new_{.col}"
      ),
      new_duration_osw = dplyr::if_else(
        is.na(new_duration_osw) &
          stringr::str_detect(
            duration_osw,
            "hr|hour"
          ),
        1,
        new_duration_osw
      ),
      new_duration_pop = dplyr::if_else(
        is.na(new_duration_pop) &
          stringr::str_detect(
            duration_pop,
            "hr|hour"
          ),
        1,
        new_duration_pop
      ),
    ) |>
    dplyr::select(
      player,
      timestamp,
      p_type2,
      p_type,
      dur_pop = new_duration_pop,
      dur_osw = new_duration_osw
    ) |>
    dplyr::mutate(
      credit_pop = 1*dur_pop,
      credit_osw = 0.5*dur_osw,
      credit_bw  = dplyr::if_else(
        p_type2 == "bw",
        1,
        0
      ),
      credit_other = dplyr::case_when(
        p_type2 == "lge" ~ 1,
        p_type2 == "star" ~ 1,
        p_type2 == "lm" ~ 1,
        p_type2 == "di" ~ 1,
        p_type2 == "fmc" ~ 1,
        .default = 0
      )
    )
  
  
  foo |>
    dplyr::group_by(
      player
    ) |>
    dplyr::reframe(
      credit_pop = sum(credit_pop, na.rm = TRUE),
      credit_osw = sum(credit_osw, na.rm = TRUE),
      credit_bw = sum(credit_bw, na.rm = TRUE),
      credit_other = sum(credit_other, na.rm = TRUE)
    ) |>
    # set limits on `bw` and `osw`
    dplyr::mutate(
      credit_osw = if_else(credit_osw > 2, 2, credit_osw),
      credit_bw = if_else(credit_bw > 1, 1, credit_bw)
    ) |>
    dplyr::mutate(
      credit_current = rowSums(
        dplyr::across(
          dplyr::matches("credit")
        )
      ),
      credit_reqd = m$practice_credit_req
    ) |>
    dplyr::arrange(
      dplyr::desc(credit_current)
    ) |> 
    dplyr::mutate(
      game = game,
      .before = 1
    )
}