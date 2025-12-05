pretty_eligibility_tbl <- function(data) {
  data |> 
    dplyr::mutate(
      status = dplyr::if_else(
        credit_current >= credit_reqd,
        "Complete",
        paste(
          round(abs(credit_current - credit_reqd), 1),
          " reqd"
        )
      )
    ) |>
    dplyr::select(
      game,
      player,
      status,
      `Credit Total` = credit_current,
      `Bout Watch` = credit_bw,
      `Off Skates` = credit_osw
    )
}