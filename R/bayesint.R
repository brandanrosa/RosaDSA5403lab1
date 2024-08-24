#' bayesint
#'
#' An interactive app with sliders for the mean and sd values
#'
#' @return an interactive plot with labels
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples \dontrun{bayesint()}
bayesint <- function() {
  runApp(system.file("ShinyIntegrals",
                     package = "RosaDSA5403lab1"),
         launch.browser = TRUE)
}
