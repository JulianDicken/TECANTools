#' Extract data from a TECAN measurement
#'
#' This function loads an excel file that it assumes is in a TECAN generated
#' format. It then predicts what kind of data you generated and classifies each dataset based on
#' multiple heuristics.
#'
#' @param path Path to the input file(s) or folder
#' @param datagroups The user-defined data groups. They follow the pattern list(group1 = list(pattern1, pattern2), group2 = list(pattern1, pattern2))
#' @param datavalue.name The name of the value column.
#' @param datagroup.leader The column to match the groups against. This is recommended to leave unchanged.
#' @param na.warn Whether or not to show an error message when NAs are detected
#' @param na.rm Whether or not to remove NAs from the data. Only turn this on after verifying each instance of NAs
#' @param .datagroups.prepostfix This adds some sensible defaults to the regexp. Only ever touch this if you know what ^(?=.*[A-Za-z])(?=.*\d)[A-Za-z\d]{8,}$ means.
#' @param .debug Toggle debug mode. Does not do much for now.
#' @return data.frame containing all measurements in long format.
#' @export
Extract.Data <- function(
    path,
    datagroups,
    datavalue.name   = "Value",
    datagroup.leader = "Coordinate",
    na.warn = TRUE,
    na.rm = FALSE,
    .datagroups.prepostfix = TRUE,
    .debug = FALSE
) {
  # --- Constants ---
  # *****************
  # -----------------
  datasets.meta <- Const.dataset_meta()

  # --- Input deconstruction and file validation ---
  # ************************************************
  # ------------------------------------------------
  files <- unlist(lapply(as.vector(path), function(p) {
    p.valid_suffixes <- c("xls", "xlsx")
    p.valid_pattern <- paste0(".*\\.(", paste0(p.valid_suffixes, collapse = "|"),")$")

    p.modified <- paste0(path, "." ,p.valid_suffixes)
    p.existing <- file.exists(p.modified)
    if (dir.exists(p)) {
      return(paste0(p, "/", list.files(
        p, pattern = p.valid_pattern
      )))
    }
    else if (any(p.existing)) {
      return(p.modified[[match(TRUE, p.existing)]])
    }
    else {
      return(NULL)
    }
  }))

  if (!is.list(datagroups)) {
    stop(paste0(
      "datagroups is not a list, instead it is of type ",
      typeof(datagroups)
    ))
  }

  # --- Modify Datagroups ---
  # *************************
  # -------------------------
  # adds sensible defaults for this data format to the regex
  if (.datagroups.prepostfix) {
    datagroups <- purrr::map(
      datagroups, ~ purrr::map(.x, ~ paste0("^(", .x, "){1}$"))
    )
  }

  # --- Process Files ---
  # *********************
  # ---------------------
  data.out <- data.frame()

  for (file in files) {

    # --- Extract Datasets ---
    # ************************
    # ------------------------
    datasets <- Utility.extract_datasets(
      filepath = file
    )

    # --- Extract Data ---
    # ********************
    # --------------------
    for (i in 1:nrow(datasets)) {
      dataset <- datasets[i, ]
      print(paste0(
        "Extracting dataset with name '", dataset$Name,
        "' at '", dataset$Range, "' From file: ", dataset$filepath
      ))
      dataset.data <- switch(match(dataset$Kind, datasets.meta$Kind),
        Extract.data_timeseries_multiple(
          filepath   = file,
          datagroups = datagroups,

          dataset          = dataset,
          datavalue.name   = datavalue.name,
          datagroup.leader = datagroup.leader,
          na.warn = na.warn,
          na.rm   = na.rm,

          .debug = .debug
        ),
        Extract.data_timeseries_single(
          filepath   = file,
          datagroups = datagroups,

          dataset          = dataset,
          datavalue.name   = datavalue.name,
          datagroup.leader = datagroup.leader,
          na.warn = na.warn,
          na.rm   = na.rm,

          .debug = .debug
        ),
        Extract.data_snapshot(
          filepath     = file,
          datagroups   = datagroups,

          dataset          = dataset,
          datavalue.name   = datavalue.name,
          datagroup.leader = datagroup.leader,
          na.warn = na.warn,
          na.rm   = na.rm,

          .debug = .debug
        ),
        Extract.data_simple(
          filepath     = file,
          datagroups   = datagroups,

          dataset          = dataset,
          datavalue.name   = datavalue.name,
          datagroup.leader = datagroup.leader,
          na.warn = na.warn,
          na.rm   = na.rm,

          .debug = .debug
        )
      )
      data.out <- dplyr::bind_rows(data.out, dataset.data)
    }
  }

  return(data.out)
}
