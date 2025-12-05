download_csv_button <- function(table_id, label = "Download as CSV") {
  htmltools::tags$button(
    label,
    onclick = sprintf("Reactable.downloadDataCSV('%s')", table_id)
  )
}