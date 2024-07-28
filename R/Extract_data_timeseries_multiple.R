Extract.data_timeseries_multiple <- function(
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

  # --- Process raw data ---
  # ************************
  # ------------------------
  data.out <- data.frame()
  dataset.items_height    <- match(TRUE, is.na(data.in["Cycles / Well"])) + 1
  dataset.items_width     <- length(colnames(data.in)) - 1
  dataset.reads_per_well  <- dataset.items_height - 7

  for (i in seq(1, nrow(data.in), by = dataset.items_height)) {
    data.out.temp <- data.frame(
      .temp = rep(NA, times = dataset.items_width * dataset.reads_per_well)
    )
    data.out.temp$Coordinate    <- rep(as.character(data.in[i, 1], each = dataset.items_width * dataset.reads_per_well))
    data.out.temp$Cycle         <- rep(as.numeric(data.in[i, -1]), each = dataset.reads_per_well)
    data.out.temp$Time          <- rep(as.numeric(data.in[i + 1, -1]), each = dataset.reads_per_well)
    data.out.temp$Temperature   <- rep(as.numeric(data.in[i + 2, -1]), each = dataset.reads_per_well)

    data.out.temp[datavalue.name] <-
      data.in[
        seq(
          i + 5,
          i + 4 + dataset.reads_per_well
        ),
        -1
      ] %>%
      tidyr::pivot_longer(
        cols = tidyr::everything(),
        values_to = "Value"
      ) %>%
      dplyr::select(
        .data$Value
      ) %>%
      unlist()

    data.out <- rbind(data.out, data.out.temp)
  }
  print(data.out)
  data.out <- data.out %>%
    dplyr::select(-.data$.temp) %>%
    dplyr::mutate(
      dplyr::across(
        -c(.data$Coordinate),
        ~purrr::map_dbl(.x, ~ suppressWarnings(as.numeric(.x)))
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
    dplyr::mutate(
      Row    = as.character(gsub("[0-9]", "", .data$Coordinate)),
      Column = as.numeric(  gsub("[A-Z]", "", .data$Coordinate)),
    ) %>%
    dplyr::select(
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

  data.out <- cbind(
    Source  = filepath,
    Dataset = dataset$Name,
    data.out
  )

  return(data.out)
}
