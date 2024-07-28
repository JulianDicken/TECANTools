Const.dataset_meta <- function() {
  return(data.frame(
    Kind = c(
      "Timeseries.Multiple",
      "Timeseries.Single",
      "Snapshot",
      "Simple"
    ),
    Kcol = c(
      1,
      1,
      2,
      2
    ),
    Qual = c(
      "Cycles / Well",
      "Cycle Nr.",
      "Well",
      "<>"
    ),
    Qcol = c(
      1,
      1,
      1,
      1
    )
  ))
}
