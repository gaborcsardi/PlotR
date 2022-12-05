# helpers

tryCatchWithMessage <- function(expr, errorOutput = NULL) {
  tryCatch(
    withCallingHandlers(
      expr,
      message = function(m) showNotification(m$message, type = "message"),
      warning = function(w) {
        logWarning(w)
        showNotification(w$message, type = "warning")
      }
    ),
    error = function(e) {
      logError(e)
      shinyalert("Error!", e$message, type = "error")
      errorOutput
    }
  )
}
