expand_toggle_button <- function(table_id, label = "Expand/collapse all") {
  htmltools::tags$button(
    label,
    onclick = sprintf("Reactable.toggleAllRowsExpanded('%s')", table_id)
  )
}