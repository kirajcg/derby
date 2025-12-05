plot_scoreboard_worm <- function(data) {
  
  last_jam <- data |> 
    dplyr::filter(half == "first") |> 
    dplyr::pull(jam) |> 
    max()
  
  data |> 
    dplyr::select(
      jam,
      dplyr::matches("gametotal")
    ) |> 
    tidyr::pivot_longer(
      cols = dplyr::matches("gametotal"),
      names_to = "team",
      values_to = "value"
    ) |> 
    dplyr::mutate(
      team = dplyr::if_else(
        stringr::str_detect(team, "1$"),
        data$team1[1],
        data$team2[1]
      )
    ) |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = jam, y = value, 
        color = team, fill = team
      )
    ) +
    ggplot2::geom_vline(
      aes(xintercept = last_jam),
      size = 1.2,
      col = "black",
      lty = 1
    ) +
    ggplot2::geom_line(
      size = 1.5
    ) +
    ggplot2::geom_point(
      size = 2,
      shape = 21,
      col = "black"
    ) +
    ggplot2::labs(
      x = "Jam number",
      y = "Total points",
      title = data$date[1]
    ) +
    ggplot2::theme_minimal(24) +
    ggplot2::theme(
      legend.position = "top",
      legend.justification = "right",
      plot.title = ggplot2::element_text(hjust = 0.5),
      # legend.text = element_text(margin = margin(t = 5)),
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(nrow = 2),
      fill = ggplot2::guide_legend(nrow = 2)
    )
}