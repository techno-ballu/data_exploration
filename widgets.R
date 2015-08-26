# This function generates the client-side HTML for a button
buttonInput <- function(inputId, label, value = "", class="",eventName = "") {
  #   browser()
  tagList(
    shiny::tags$button(id=inputId, 
                type="button", 
                class=paste("btn custom-button shiny-bound-input",class),as.character(value),"data-feature"=inputId,
                "data-event"=eventName)
  )
}

# Send an update message to a URL input on the client.
# This update message can change the value and/or label.
updateButtonInput <- function(session, inputId,
                           feature = "") {
  session$sendInputMessage(inputId, list(feature = feature))
}

# # This function generates the client-side HTML for a button
# urlInput <- function(inputId, label, value = "") {
# #   browser()
#   tagList(
#     # This makes web page load the JS file in the HTML head.
#     # The call to singleton ensures it's only included once
#     # in a page.
#     shiny::singleton(
#       shiny::tags$head(
#         shiny::tags$script(src = "widget-bindings.js")
#       )
#     ),
# #     shiny::tags$button(id=inputId, 
# #                 type="button", 
# #                 class=paste("btn action-button shiny-bound-input",class),as.character(value))
#     shiny::tags$label(label, 'for' = inputId),
#     shiny::tags$input(id = inputId, type = "text", value = value)
#   )
# }
# 
# 
# # Given a vector or list, drop all the NULL items in it
# dropNulls <- function(x) {
#   x[!vapply(x, is.null, FUN.VALUE=logical(1))]
# }