
PKGENVIR <- new.env(parent=emptyenv()) # package level envir
#' Launch app
#'
#' This function launch a shiny-based web app to display a graph of condition count
#'
#' @param data data taked from the environment
#' @export
#'
launchShinyApp<- function(data) {
  appDir <- system.file("shiny-examples", "myapp", package = "JMcondition")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  PKGENVIR$DATA <- data # put the data into envir
  shiny::runApp(appDir, display.mode = "normal")
}
