Extract.data_timeseries_single <- function(
    filepath,
    dataset,
    datagroups,
    datavalue.name   = "Value",
    datagroup.leader = "Coordinate",
    na.warn = TRUE,
    na.rm = FALSE,
    .debug = FALSE) {

  # --- Raw data construction ---
  # *****************************
  # -----------------------------
  data.in <- readxl::read_excel(
    filepath, col_types = "text",
    range = dataset$Range,
    # this is to suppress rename warnings
    .name_repair = "unique_quiet"
  )
  # bind a data.set row to the data to differentiate data.sets
  data.in <- cbind(
    Dataset = dataset$Name,
    data.in
  )
  # --- Process raw data ---
  # ************************
  # ------------------------
  data.out <- data.in %>%
    dplyr::mutate(dplyr::across(
      -c(.data$Dataset),
      ~ purrr::map_dbl(.x, ~ suppressWarnings(as.numeric(.x)))
    )
  )

  if (na.warn && any(is.na(data.out))) {
    print(
      paste0("Position of NAs:\n", which(is.na(data.out)))
    )
    stop("NAs found in data, please verify any occurences.
       Run this function with na.rm=TRUE to drop NAs.
       Run this function with na.ignore=TRUE to prevent this warning")
  }

  if (na.rm) {
    data.out <- data.out %>%
      dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.)))
  }

  data.out <- data.out %>%
    tidyr::pivot_longer(
      cols = tidyr::matches("^[A-Z][0-9]*$"),
      names_to = "Coordinate",
      values_to = "OD",
      cols_vary = "slowest"
    ) %>%
    stats::setNames(
      c("Dataset", "Cycle", "Time", "Temperature", "Coordinate", datavalue.name)
    ) %>%
    dplyr::mutate(
      Row    = as.character(gsub("[0-9]", "", .data$Coordinate)),
      Column = as.numeric(  gsub("[A-Z]", "", .data$Coordinate)),
    ) %>%
    dplyr::select(
      .data$Dataset,
      .data$Row, .data$Column, .data$Coordinate,
      .data$Cycle, .data$Time,
      .data$Temperature,
      !!rlang::sym(datavalue.name)
    )

  if (.debug) {
    assign("Extract.Timeseries.raw", data.in)
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

  return(data.out)
}

