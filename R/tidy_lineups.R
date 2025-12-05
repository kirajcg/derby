tidy_lineups <- function(data, teams) {
  data |> 
    dplyr::select(
      !matches("^x"),
      - team_roster,
      - jam_2
    ) |> 
    dplyr::rename(
      nopivot = no_pivot,
      nopivot_2 = no_pivot_2,
      blocker1 = blocker,
      blocker2 = blocker_2,
      blocker3 = blocker_3,
      blocker1_2 = blocker_4,
      blocker2_2 = blocker_5,
      blocker3_2 = blocker_6
    ) |> 
    tidyr::pivot_longer(
      cols = !jam,
      names_to = "what",
      values_to = "value"
    ) |> 
    dplyr::mutate(
      box = dplyr::if_else(
        stringr::str_detect(
          what,
          "^jammer|^pivot|^blocker"
        ),
        dplyr::lead(value),
        NA
      )
    ) |> 
    dplyr::filter(
      !stringr::str_detect(what, "^box")
    ) |> 
    tidyr::separate_wider_delim(
      what,
      delim = "_",
      names = c("what", "team"),
      too_few = "align_start"
    ) |> 
    dplyr::mutate(
      team = dplyr::if_else(
        is.na(team),
        teams[1],
        teams[2]
      )
    ) |> 
    dplyr::mutate(
      jam = as.integer(jam)
    ) |> 
    dplyr::filter(!is.na(jam))
}
