#' Config
#'
#' @return (list) configuration parameters for import of data and models
#'
#' @export
config <- function() {
  config_path <- system.file("config.yaml", package = "PlotR")
  read_yaml(config_path)
}
