grouped_reactable <- function(data, group_var, id = "1", ...) {
  id <- paste0("expandable-table-", id)
  
  htmltools::browsable(
    htmltools::tagList(
      expand_toggle_button(id),
      
      reactable::reactable(
        data,
        groupBy = group_var,
        searchable = TRUE,
        filterable = TRUE,
        sortable = TRUE,
        resizable = TRUE,
        elementId = id,
        ...
      )
    )
  )
}