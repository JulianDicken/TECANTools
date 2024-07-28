Extract.data_simple <- function(
    filepath,
    datagroups,
    dataset,
    datavalue.name   = "Value",
    datagroup.leader = "Coordinate",

    na.warn = TRUE,
    na.rm = FALSE,
    .debug = FALSE
) {

  # --- Raw data construction ---
  # *****************************
  # -----------------------------
  data.in <- readxl::read_excel(
    filepath, col_types = "text",
    range = dataset$Range,
    # this is to suppress rename warnings
    .name_repair = "unique_quiet"
  )

  if (.debug) {
    assign("Extract.Snapshot.raw", data.in)
  }

  # --- Process raw data ---
  # ************************
  # ------------------------

  # turn this data into  long format data. This needs to be done BEFORE detecting NAs because of the data format
  data.out <- data.in %>%
    dplyr::rename(Row = 1) %>%
    tidyr::pivot_longer(
      cols = !c(.data$Row),
      names_to = "Column",
      values_to = datavalue.name
    ) %>%
    dplyr::mutate(
      Coordinate = paste0(.data$Row, .data$Column)
    ) %>%
    dplyr::mutate(dplyr::across(
        datavalue.name,
        ~purrr::map_dbl(.x, ~ suppressWarnings(as.numeric(.x)))
      )
    ) %>%
    dplyr::select(
      .data$Row, .data$Column, .data$Coordinate, !!rlang::sym(datavalue.name)
    )

  if (na.warn && any(is.na(data.out))) {
    print(
      paste0("Position of NAs:\n", which(is.na(data.out)))
    )
    stop("NAs found in data, please verify any occurences.
         Run this function with na.rm=TRUE to drop NAs.
         Run this function with na.warn=FALSE to prevent this warning"
    )
  }

  if (na.rm) {
    data.out <- data.out %>%
      dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.)))
  }

  # --- Process into groups ---
  # ***************************
  # ---------------------------

  for (datagroup.name in names(datagroups)) {
    datagroup <- datagroups[[datagroup.name]]
    datagroup.conditions <- Utility.case_when_from_datagroup_rules(datagroup, datagroup.leader)
    # here I am doing code gen which is less than ideal but IMO the best way of doing this. If you know a better way, PR it.
    data.out <- data.out %>%
      dplyr::mutate(!!rlang::sym(datagroup.name) := eval(parse(text = datagroup.conditions)))
  }


  data.out <- cbind(
    Source  = filepath,
    Dataset = dataset$Name,
    data.out
  )

  return(data.out)
}
