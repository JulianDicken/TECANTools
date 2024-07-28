Utility.case_when_from_datagroup_rules <- function(data.group, data.subject) {
  data.group.conditions = "dplyr::case_when(\n"
  for (data.category in names(data.group)) {
    category.condition    = data.group[[data.category]]
    data.group.conditions = sprintf(
      '%s grepl("%s", %s) ~ "%s",\n',
      data.group.conditions,
      category.condition,
      data.subject,
      data.category
    )
  }
  data.group.conditions = paste0(data.group.conditions, "TRUE ~ NA)\n")
  return(data.group.conditions)
}
