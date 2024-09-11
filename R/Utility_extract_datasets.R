Utility.extract_datasets <- function(
    filepath,
    datagroups.layout.Rows    = LETTERS[1:8],
    datagroups.layout.Columns = 1:12
) {
  # --- Constants ---
  # *****************
  # -----------------
  datasets.meta <- Const.dataset_meta()

  # --- Input validation ---
  # ************************
  # ------------------------
  if (!file.exists(filepath)) {
    filepath.modified <- paste0(filepath, c(".xls", ".xlsx"))
    if (any(file.exists(filepath.modified))) {
      filepath <- filepath.modified[[which(file.exists(filepath.modified))]]
    } else {
      stop(paste0(
        "File '", filepath, "' or '",
        filepath.modified, "' does not exist."
      ))
    }
  }

  data.raw <- readxl::read_excel(
    filepath, col_names = FALSE, col_types = "text",
    .name_repair = "unique_quiet"
  )

  # --- Find all values in data that are preceded by 'Name' ---
  # This would suggest this dataset existing within the file
  # ***********************************************************
  # -----------------------------------------------------------
  datasets <- data.raw %>%
    dplyr::filter(.[[1]] == "Name", !is.na(.[[2]])) %>%
    dplyr::select(2) %>%
    stats::setNames("Name") %>%
    dplyr::mutate(NIDX = NA) %>%
    dplyr::mutate(QIDX = NA) %>%
    dplyr::mutate(Kind = NA) %>%
    dplyr::mutate(Range = NA)

  dataset.size = length(datagroups.layout.Rows) * length(datagroups.layout.Columns)

  # --- Classify Datasets ---
  # ************************
  # ------------------------
  for (i in 1:nrow(datasets)) {
    dataset.name  <- datasets$Name[[i]]
    dataset.found.nidx <- list()
    dataset.found.qidx <- list()
    for (j in 1:nrow(datasets.meta)) {
      data.raw.kcol <- data.raw[[datasets.meta$Kcol[[j]]]] %>%
        as.character()
      data.raw.qcol <- data.raw[[datasets.meta$Qcol[[j]]]] %>%
        as.character()
      dataset.qual  <- datasets.meta$Qual[[j]]

      dataset.nidx  <- match(dataset.name, data.raw.kcol)
      if (is.na(dataset.nidx)) {
        next
      }

      dataset.qidx  <- dataset.nidx - 1 +
        match(dataset.qual, data.raw.qcol[dataset.nidx:length(data.raw.qcol)])
      if (is.na(dataset.qidx)) {
        next
      }
      dataset.found.nidx[datasets.meta$Kind[[j]]] <- dataset.nidx
      dataset.found.qidx[datasets.meta$Kind[[j]]] <- dataset.qidx
    }
    dataset.found.best.index <- which.min(dataset.found.qidx)
    dataset.found.best.kind  <- names(dataset.found.qidx)[dataset.found.best.index]
    dataset.found.best.nidx <- dataset.found.nidx[[dataset.found.best.index]]
    dataset.found.best.qidx <- dataset.found.qidx[[dataset.found.best.index]]
    datasets[datasets$Name == dataset.name,] <-
      data.frame(
        Name = dataset.name,
        NIDX = dataset.found.best.nidx,
        QIDX = dataset.found.best.qidx,
        Kind = dataset.found.best.kind,
        Range = NA
      )
  }
  # --- Extract Ranges ---
  # **********************
  # ----------------------
  for (i in 1:nrow(datasets)) {
    dataset.currentKind <- datasets$Kind[[i]]

    dataset.range <- switch(match(dataset.currentKind, datasets.meta$Kind),
      Utility.extract_datasets_timeseries_multiple(
        data.raw,
        datasets[i, ],  dataset.size
      ),
      Utility.extract_datasets_timeseries_single(
        data.raw,
        datasets[i, ],  dataset.size
      ),
      Utility.extract_datasets_snapshot(
        data.raw,
        datasets[i, ],  dataset.size
      ),
      Utility.extract_datasets_simple(
        data.raw,
        datasets[i, ],  dataset.size
      )
    )
    datasets$Range[[i]] <- dataset.range
  }


  return(datasets)
}
