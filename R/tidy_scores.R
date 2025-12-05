tidy_scores <- function(data, teams) {
  data |> 
    dplyr::select(
      !matches("^trip_"),
      - jam_2
    ) |> 
    dplyr::rename(
      jammer = jammers_number,
      jammer_2 = jammers_number_2,
      jamtotal = jam_total,
      jamtotal_2 = jam_total_2,
      gametotal = game_total,
      gametotal_2 = game_total_2
    ) |>
    tidyr::pivot_longer(
      cols = !jam,
      names_to = "what",
      values_to = "value"
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
    dplyr::arrange(
      jam,
      what,
      team
    ) |> 
    dplyr::group_by(jam, what, team) |> 
    dplyr::reframe(value = max(value, na.rm = TRUE)) |> 
    dplyr::mutate(
      jam = as.integer(jam)
    ) |> 
    dplyr::filter(!is.na(jam))
}