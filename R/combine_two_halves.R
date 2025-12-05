combine_two_halves <- function(x, y) {
  rbind(
    x, y
  ) |> 
    dplyr::arrange(
      half,
      jam,
      what,
      team
    ) |>
    dplyr::mutate(
      last_jam = max(
        last_jam, 
        na.rm = TRUE
      )
    ) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      jam = if_else(
        half == "first",
        jam,
        jam + last_jam
      )
    ) |> 
    dplyr::ungroup()
}