#' @title 
#' Launch the Covid-19 Shiny app
#' 
#' @description 
#'Runs a Shiny application. This function normally does not return; interrupt R to stop the application.
#'
#' @param display.mode The mode in which to display the application. If set to the value "showcase", shows application code and metadata from a DESCRIPTION file in the application directory alongside the application. If set to "normal", displays the application normally. Defaults to "auto", which displays the application in the mode given in its DESCRIPTION file, if any.
#' @param launch.browser If true, the system's default web browser will be launched automatically after the app is started. Defaults to true in interactive sessions only. This value of this parameter can also be a function to call with the application's URL.
#'
#' @examples 
#' launch_app()
#' launch_app(display.mode = "showcase", launch.browser = TRUE)
#'
#' @export
#' 
launch_app <- function(display.mode = c("auto", "normal", "showcase"),
                       launch.browser = c(TRUE, FALSE)) {
  appDir <- system.file("app/app.R", package = "CovChina")
  if (appDir == "") {
    stop("Could not find inst/app directory. Try re-installing `CovChina`.", call. = FALSE)
    
  }
  
  shiny::runApp(appDir, display.mode = display.mode, launch.browser = launch.browser)
}

