#library(shinyGridster)
# library(shinysky)
source("chooser.R")
source("download.R")
HEIGHT_NZVCHART = 200
HEIGHT_RICHART = 350
sidebarPanel3 <- function (..., width = 3) 
{
  div(class = paste0("span", width), tags$form(class = "well", 
                                               ...))
}

mainPanel9 <- function (..., width = 9) 
{
  div(class = paste0("span", width), ...)
}

navlistPanel9 <- function (..., id = NULL, selected = NULL, well = TRUE, fluid = TRUE, widths = c(3, 9)) {
  textFilter <- function(text) {
    if (grepl("^\\-+$", text)) 
      tags$li(class = "divider")
    else tags$li(class = "nav-header", text)
  }
  tabs <- list(...)
  tabset <- buildTabset(tabs, "nav nav-list", textFilter, id, 
                        selected)
  columns <- list(column(widths[[1]], class = ifelse(well, 
                                                     "well", ""), tabset$navList), column(widths[[2]], tabset$content))
  if (fluid) 
    fluidRow(columns)
  else fixedRow(columns)
}

shinyUI(bootstrapPage(
  tagList(
    singleton(tags$head(
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'ayata-nvd3.css'),
      tags$script(src = 'widget-bindings.js'),
      tags$script(src = 'event-handling.js'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'dataTables.extentions/css/dataTables.colReorder.css'),
      tags$script(src = 'dataTables.extentions/js/dataTables.colReorder.js'),
      tags$script(src = 'dataTables.extentions/js/dataTables.tableTools.js'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'dataTables.extentions/css/dataTables.colvis.jqueryui.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'dataTables.extentions/css/dataTables.colVis.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'dataTables.extentions/css/dataTables.tableTools.css'),
      tags$script(src = 'dataTables.extentions/js/dataTables.colVis.js'),
      tags$script(src = 'underscore/underscore.js')
    ))
  ),navbarPage(title = "DataScience",
               inverse = TRUE,
               tabPanel("About", includeMarkdown('help.md')),
               navbarMenu('Data', tabPanel("Load data",fluidPage(
                 fluidRow(
                   column(3,fileInput('file1', 'Choose csv file',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                   column(3, htmlOutput("rowsNCols")),
                   column(3, selectInput("id", "Select id column(single)",c(),NULL)), #, width = "220px"
                   column(3, selectInput(inputId = "metrics", label = "Select metrics(multiple)", choices = c("Data not loaded yet"), multiple = T, selectize = T))  # selectInput(inputId = "useless", label = "Select redundant variables(multiple):", choices = c("Data not loaded yet"), multiple = T, selectize = T)
                 ),
                 fluidRow(
                   column(12,dataTableOutput('contents'))
                 )
               )),
               tabPanel("Clean data", fluidPage(
                 tabsetPanel(
                   tabPanel("View metadata",
                            fluidRow(
                              column(12,dataTableOutput("statistics"))), value = "metadata"),
                   tabPanel("Clean rows & columns",
                            h4("Improve the quality of observations & features",class="pull-left"),
                            actionButton("cleanRowsNCols","Auto clean",icon = "bolt",icon.library = c("font awesome"),class="pull-right btn-default bottom-margin"), #
                            wellPanel(fluidRow(
                              h4("Clean rows"),
                              helpText("Below is a histogram of the frequency of wells vs. the number of features missing:"),
                              column(12, downloadInput('clean_row'),nvd3ChartOutput("clean_row","nvd3-histogram",height = 200))
                            ),style="clear:both;"),
                            wellPanel(fluidRow(
                              h4("Clean columns"),
                              helpText("Below is a histogram of the frequency of features vs. the number of wells for which it is missing:"),
                              column(12,downloadInput('clean_col'),nvd3ChartOutput("clean_col","nvd3-histogram",height = 200))
                            )),bsModalBoot3("auto-clean-rows-cols", "Auto clean rows & columns based on thresholds set below?", trigger = "cleanRowsNCols", 
                                            body = list(tags$p(class="modal-info"),tags$form(role="form",class="form-horizontal",tags$div(class = "form-group",tags$label("for"="user-name",class="col-sm-2 control-label","Name"),
                                                                                                                                          tags$div(class="col-sm-5",tags$input(type="text",class="span3 form-control shiny-bound-input",id="user-name",value=Sys.info()[["user"]]))),
                                                                                             tags$div(class = "form-group",tags$label("for"="comments",class="col-sm-2 control-label","Reason"),
                                                                                                      tags$div(class="col-sm-10",tags$textarea(class="form-control shiny-bound-input",id="comments",rows = 3,cols = 3))
                                                                                             ),tags$hr(),
                                                                                             tags$div(class = "form-group",tags$label("for"="rows-threshold",class="col-sm-7 control-label","Clean rows with % features missing > or ="),
                                                                                                      tags$div(class="col-sm-3",numericInput("rows-threshold", "", min = 1, max = 100, value = 50,step = 1))),
                                                                                             tags$div(class = "form-group",tags$label("for"="cols-threshold",class="col-sm-7 control-label","Clean features with % wells missing > or ="),
                                                                                                      tags$div(class="col-sm-3",numericInput("cols-threshold", "", min = 1, max = 100, value = 50,step = 1))))), 
                                            footer = list(tags$button(class = "btn btn-default modal-cancel", 'data-dismiss' = "modal", "No"),tags$button(class = "btn btn-default modal-save", 'data-dismiss' = "modal", "Yes"))
                            )
                            , value = "rows_columns"),
                   tabPanel("Clean outliers", fluidRow(
                     column(5,tags$h4("Remove outliers from continuous & nominal features")), 
                     column(4,tags$div(radioButtons("variablesType", label = "", choices = list("Numericals" = "numericals", "Categoricals" = "categoricals"),
                                                    selected = "numericals", inline = T))), #class="pull-right",
                     #                       column(3,bsButtonGroup("variablesType", bsButton("nums",label = "Numericals", value = "numericals",style = "default"),
                     #                                              bsButton("cats",label = "Categoricals", value = "categoricals",style = "default"), label = "", toggle = "radio", value = "numericals",
                     #                                              style = "default", disabled = FALSE)),
                     column(2,downloadButton('downloadData', 'Download csv',"pull-right"))),
                     fluidRow(column(12,
                                     conditionalPanel(
                                       condition = "input.variablesType == 'numericals'",
                                       fluidPage(fluidRow(column(3,selectInput("numberOfNumFeatures", "Control # of features to view at a time:", c(1,5,10,"All"), "5")),
                                                          column(3,selectInput("quickFindNum", "Quickly look up a feature:",c("features not loaded yet!"),multiple=T,selectize = T)),
                                                          column(3,actionButton(inputId = "rearrange-outliers", label = "Rank features with outliers on top",icon = "sort-amount-desc",icon.library = c("font awesome"),class="btn-default header-buttons")),
                                                          column(3,actionButton("cleanNumOutliers","Auto clean",icon = "bolt",icon.library = c("font awesome"),class="btn-default header-buttons pull-right"))),
                                                 fluidRow(column(12,uiOutput("clean_numericals"))),bsModalBoot3("auto-clean-outliers", "Auto clean numerical outliers based on threshold set below", trigger = "cleanNumOutliers", 
                                                                                                                body = list(tags$p(class="modal-info"),tags$form(role="form",class="form-horizontal",tags$div(class = "form-group",tags$label("for"="user-name",class="col-sm-2 control-label","Name"),
                                                                                                                                                                                                              tags$div(class="col-sm-5",tags$input(type="text",class="span3 form-control shiny-bound-input",id="user-name",value=Sys.info()[["user"]]))),
                                                                                                                                                                 tags$div(class = "form-group",tags$label("for"="comments",class="col-sm-2 control-label","Reason"),
                                                                                                                                                                          tags$div(class="col-sm-10",tags$textarea(class="form-control shiny-bound-input",id="comments",rows = 3,cols = 3))
                                                                                                                                                                 ),tags$hr(),
                                                                                                                                                                 tags$div(class = "form-group",tags$label("for"="common-threshold",class="col-sm-5 control-label","# of SDs tolerance for outliers?"),
                                                                                                                                                                          tags$div(class="col-sm-7",sliderInput("common-threshold", "", min = 1, max = 100, value = c(5),step = 0.1))))), 
                                                                                                                footer = list(tags$button(class = "btn btn-default modal-cancel", 'data-dismiss' = "modal", "No"),tags$button(class = "btn btn-default modal-save", 'data-dismiss' = "modal", "Yes"))
                                                 ))),
                                     conditionalPanel(
                                       condition = "input.variablesType == 'categoricals'",
                                       fluidPage(fluidRow(column(3,selectInput("quickFindCat", "Quickly look up a feature:",c(),multiple=F,selectize = T),
                                                                 checkboxGroupInput("namesCategories", "Filter categories starting with:",c("hello"),inline = T)),#,multiple=T,selectize = T,actionButton("quickFindCategories", "Locate",icon("search"))
                                                          column(9, #downloadInput('barplot'),
                                                                 d3ChartOutput("clean_categorical","d3-bubbles",height = 450)))
                                       ))
                     )), value = "outliers"),
                   tabPanel("Near zero variance",h3("Eliminate near zero variance columns"),fluidPage(wellPanel(fluidRow(helpText("Below is a histogram that shows the unique vale percentage per feature"),(column(12,nvd3ChartOutput("nearZeroVariancePercent","nvd3-histogram",height=HEIGHT_NZVCHART))) 
                   )),wellPanel(fluidRow(helpText("Below is a histogram that shows the frequency of most prevalent value over the second most frequent value which would be nearing 1 for well behaved predictors and very large for highly-unbalanced data :"),(column(12,nvd3ChartOutput("nearZeroVarianceFreq","nvd3-histogram",height=HEIGHT_NZVCHART)))))                                                                                         
                   )),type = "tabs",id = "clean-data")
               ))
               ),
               navbarMenu("Discovery", tabPanel("Relations",fluidPage(tabsetPanel(
                 type = "pills",
                 tabPanel("View numerical relations", fluidRow(
                   column(4, 
                          fluidRow(
                            column(3, h5("X variable", style="padding-top:16px")),
                            column(9, selectInput("x", '', choices = NULL, multiple = F))
                          )
                   ),
                   column(4, 
                          fluidRow(
                            column(3, h5("Y variable", style="padding-top:16px")),
                            column(9, selectInput("y", '', choices = NULL, multiple = F))
                          )
                   ),
                   column(1, br(), actionButton("goButton", "Apply")),
                   column(2, br(),  splitLayout(cellWidths = c("50%", "50%"),h5("Correlation"),verbatimTextOutput(outputId = "COR")))
                 ),
                 fluidRow(
                   column(12,downloadInput('scatter'),nvd3ChartOutput("scatter","nvd3-scatterchart", height = 450))
                 )),
                 tabPanel("View categorical relations", fluidRow(
                   column(4, fluidRow(
                     column(3, h4("Feature", style="padding-top:16px")),
                     column(9, selectInput("xC", "", choices = NULL, multiple = F))
                   )),
                   column(4, fluidRow(
                     column(3, h4("Group by", style="padding-top:16px")),
                     column(9, selectInput("yC", "", choices = NULL, multiple = F))
                   ))#,
                   #       column(1, br(), actionButton("goButtonC", "Apply"))
                 ),fluidRow(
                   column(12,d3ChartOutput("cat-cat","d3-bubbles-clusters",height = 500))
                 )),
                 tabPanel("View numerical vs categorical relations", fluidRow(
                   column(3, selectInput("xNC", h5("X variable"), choices = NULL, multiple = F)),
                   column(3, selectInput("yNC", h5("Y variable"), choices = NULL, multiple = F)),
                   column(2, br(), br(), actionButton("goButtonNC", "Apply"))
                 )),
                 tabPanel("View categorical vs numerical relations", fluidRow(
                   column(4, fluidRow(
                     column(3, h4("Feature", style="padding-top:16px")),
                     column(9, selectInput("xCN", "", choices = NULL, multiple = F))
                   )),
                   column(6, fluidRow(
                     column(4, h4("View distribution for", style="padding-top:16px")),
                     column(8, selectInput("yCN", "", choices = NULL, multiple = F))
                   )),
                   column(2, br(), actionButton("goButtonCN", "Apply"))
                 ),fluidRow(
                   column(12,d3ChartOutput("categoricalVSnumerical","d3-box-plot",height = 450))
                 )
                 )
               ))
               ),
               tabPanel("Pair wise collinearity", fluidPage(fluidRow(column(6,d3ChartOutput("corrPlot","d3-correlations",height = 365)),
                                                                     column(6,textOutput("pairwiseCOR"),nvd3ChartOutput("test-cor","nvd3-scatterchart",height = 350, width=550))),
                                                            fluidRow(
                                                              column(12,correlationOutput("compare-features","nvd3-linechart",height = 180))
                                                            )
               )), #plotOutput("corrPlot", height = 600)
               tabPanel("Multicollinearity",
                        h3("Multicollinearity")
               )
               ),
               tabPanel('Engineering', fluidPage(h3('feature engineering...'))),
               navbarMenu("Transformations", tabPanel("Numerical transformations",
                                                     fluidPage(
                                                       #              h3("Transform features"),
                                                       tabsetPanel(tabPanel("Numerical",
                                                                            fluidRow(
                                                                              column(4,selectInput(inputId="chartType","Select Chart",choices=c("Linechart","Barchart"),selected="Barchart")),             column(8,selectInput(inputId="numericFeatures","Features",choices = c("No feature"),multiple=F) 
                                                                              )),
                                                                            uiOutput("numericalUpdate")),
                                                                   tabPanel("NumericalBin",fluidRow(
                                                                     column(4,sliderInput("noOfBin", "Number of Bin:", min = 2, max = 100, value = 4)),
                                                                     column(8,selectInput(inputId="numericbin_feature","Features",choices = c("Select")))),
                                                                     fluidRow(
                                                                       #                              column(4,hotable("hotable1")),
                                                                       column(8,
                                                                              fluidRow(nvd3ChartOutput("numericalBinChart","nvd3-histogram",height = 200),
                                                                                       tags$input(id="btnApplyBins",type="button",class="btn applybins action-button btn-small btn-primary", value="Apply"),
                                                                                       #bsButton("btn_applyBinning", "Apply", style = "primary"),
                                                                                       bsModalBoot3("confirm_applyBinning", "Confirm numerical binning", trigger = "", 
                                                                                                    body = list(tags$p(class="modal-info"),tags$form(role="form",class="form-horizontal",tags$div(class = "form-group",tags$label("for"="user-name",class="col-sm-3 control-label","Name"),                                                                                                                                          tags$div(class="col-sm-5",tags$input(type="text",class="span3 form-control",id="user-name",value=Sys.getenv("LOGNAME")))),
                                                                                                                                                     
                                                                                                                                                     
                                                                                                                                                     tags$div(class = "form-group",tags$label("for"="new-column-name",class="col-sm-3 control-label","Feature name"),                                                                                                                                          tags$div(class="col-sm-5",tags$input(type="text",class="span3 form-control",id="user-name",value=""))),
                                                                                                                                                     tags$div(class = "form-group",tags$label("for"="comments",class="col-sm-3 control-label","Reason"),
                                                                                                                                                              tags$div(class="col-sm-8",tags$textarea(class="form-control",id="comments",rows = 3,cols = 3))
                                                                                                                                                     ))), 
                                                                                                    footer = list(
                                                                                                      tags$button(class = "btn btn-default modal-cancel", 'data-dismiss' = "modal", "No"),
                                                                                                      tags$button(class = "btn btn-default modal-save", 'data-dismiss' = "modal", "Yes"))
                                                                                       ))
                                                                       ))
                                                                   )))
                                                     
               ),
               tabPanel("Categorical transformations",fluidPage(
                 tabsetPanel(tabPanel("Categorical",fluidRow(
                   column(4,selectInput(inputId="categoricalFeatures","Features",choices = c("No feature"),multiple=F) 
                   ))),
                   tabPanel("CategoricalBin",h3("Add categorical binning here!")
                   ))
                 #     fluidRow(column(6,selectInput("type", "Replace",
                 #                                                                   c("category with mean target value",
                 #                                                                     "category with dummy predictors",
                 #                                                                     "do not replace"),"category with mean target value",
                 #                                                                   width='300px')),
                 #                                              column(6,buttonInput("transform-categories", "Apply", "Apply", eventName = "transform-categories"))),
                 #     fluidRow(column(12,
                 #                     conditionalPanel(
                 #                       condition = "input.type === 'category with mean target value'",
                 #                       h3("For a categorical feature F, replace categorical value x with mean target value for the samples for which feature F has value x."),
                 #                       fluidRow(
                 #                         column(12,
                 #                                uiOutput("transforming_categories")) #uiOutput("transforming_categories")
                 #                       )),conditionalPanel(
                 #                         condition = "input.type === 'category with dummy predictors'",
                 #                         h3("Add dummy predictor variables")
                 #                       ),conditionalPanel(
                 #                         condition = "input.type === 'do not replace'",
                 #                         h3("Just leave the categorical features alone!")
                 #                       )))
               ))),
               tabPanel("Impute missing",fluidPage(h3("Impute missing values"))),
               tabPanel("Selections",fluidPage(tabsetPanel(
                 type = "pills",
                 tabPanel("Select a sub-set",h3("Allow whole data or a subset!")
                 ),
                 tabPanel("Ranking and importance",
                          h3("Rank and select the important features."),fluidRow(column(4,fluidRow(column(4, h6("Response variable", style="padding-top:16px")),
                                                                                                   column(8, selectInput("rankingFeature","", choices =c(), multiple = F)))),
                                                                                 column(4,fluidRow(column(4,h6("Ranking Method",style="padding-top:16px")),
                                                                                                   column(8,selectInput("rankingMethod",'',choices = c("Information Gain","Gain Ratio","Symmetrical Uncertainity"),selected = "Information Gain", multiple = F)))),
                                                                                 wellPanel(fluidRow(column(12,nvd3ChartOutput("rankingAndImportance","nvd3-histogram",height= HEIGHT_RICHART)))        
                                                                                 ))
                 )))),
               navbarMenu("Learning & Predictions", tabPanel("Held out test",h3("Set aside hold-out test data")
               ),
               tabPanel("Define RSS",
                        h3("Set up the random straitified splits")
               ),
               tabPanel("Algorithms",
                        h3("This is where we select algorithms.")
               ),
               tabPanel("Learning",
                        h3("Learning")
               ),
               tabPanel("Held out predictions",
                        h3("Predict the held-out test set")
               ),
               tabPanel("Test set predictions",
                        h3("Predict the test set")
               )),
               # tabPanel("Cook recipes","Cook recipes!"),
               tabPanel("LOGs", fluidPage(downloadLink('downloadLog', 'Download'),verbatimTextOutput("log")))
  ),bsModalBoot3("confirm", "Confirm delete", trigger = "", 
                 body = list(tags$p(class="modal-info"),tags$form(role="form",class="form-horizontal",tags$div(class = "form-group",tags$label("for"="user-name",class="col-sm-2 control-label","Name"),
                                                                                                               tags$div(class="col-sm-5",tags$input(type="text",class="span3 form-control shiny-bound-input",id="user-name",value=Sys.info()[["user"]]))),
                                                                  tags$div(class = "form-group",tags$label("for"="comments",class="col-sm-2 control-label","Reason"),
                                                                           tags$div(class="col-sm-10",tags$textarea(class="form-control shiny-bound-input",id="comments",rows = 3,cols = 3))
                                                                  ))), 
                 footer = list(tags$button(class = "btn btn-default modal-cancel", 'data-dismiss' = "modal", "No"),tags$button(class = "btn btn-default modal-save", 'data-dismiss' = "modal", "Yes"))
  ))) #,navbarMenu("Utilities",tabPanel("Download",downloadButton('downloadData','Download',"downloadData")))             selectInput(inputId = "useless", label = "Select redundant variables