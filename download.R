downloadInput <- function(inputId){
  print(is.finite(inputId))
  divId <- paste(inputId, 'download', sep="-")
  tagList(
    singleton(
      tags$script(src="export-binder.js")
    ),
    div(id=divId, role='group', class='btn-group-xs export-chart',
        tags$button('Save As Image', id='save', type='button', class='btn btn-default'),
        tags$button('Export', id='export', type='button', class='btn btn-default')
     )
  )
}