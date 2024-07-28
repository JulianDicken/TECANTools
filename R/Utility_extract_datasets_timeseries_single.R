Utility.extract_datasets_timeseries_single <- function(
  data.raw,
  dataset,
  dataset.size
  ) {
    dataset.nidx <- dataset$NIDX
    dataset.qidx <- dataset$QIDX
    dataset.items_width  <- sum(!is.na(data.raw[dataset.qidx + 1, ]))
    dataset.items_height <- match(TRUE, is.na(data.raw[(dataset.qidx + 1):nrow(data.raw), 1])) - 1
    return(paste0(
      Utility.excel_to_colindex(1), dataset.qidx,
      ":",
      Utility.excel_to_colindex(dataset.items_width), dataset.qidx + dataset.items_height
    ))
}
