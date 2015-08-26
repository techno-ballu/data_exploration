library(shiny)
library(rjson)

# To be called from ui.R
d3ChartOutput <- function(inputId, theClass, width="100%", height="100%") {
  style <- sprintf("width: %s; height: %s;",
                   validateCssUnit(width), validateCssUnit(height))
  
  tagList(
    # Include CSS/JS dependencies. Use "singleton" to make sure that even
    # if multiple lineChartOutputs are used in the same page, we'll still
    # only include these chunks once.
    singleton(tags$head(
      tags$script(src="d3/d3.v3.js"),
      tags$script(src="d3/d3.box.js"),
      tags$script(src="nvd3/nv.d3.js"),
      tags$link(rel="stylesheet", type="text/css", href="nvd3/nv.d3.min.css"),
      tags$script(src="d3-bindings.js")
    )),
    div(id=inputId, class=theClass, style=style) #tag("svg", list())
  )
}

# To be called from server.R
renderD3Chart <- function(expr, env=parent.frame(), quoted=FALSE) {
  # This piece of boilerplate converts the expression `expr` into a
  # function called `func`. It's needed for the RStudio IDE's built-in
  # debugger to work properly on the expression.
  installExprFunction(expr, "func", env, quoted)
  
  function() {
    df <- func()
    dat <- df$data
    cor <- df$cor
    ind <- rownames(dat)
    var <- colnames(dat)
    
    dat <- data.matrix(dat)
    
    # get rid of names
    dimnames(cor) <- dimnames(dat) <- NULL
#     cor <- matrix2json(cor)
#     dat <- matrix2json(t(dat))
    
    list(ind=ind,var=var,cor=cor,dat=t(dat))
  }
}

# render a bubbles chart using d3
renderD3BubbleChart <- function(expr, env=parent.frame(), quoted=FALSE) {
  installExprFunction(expr, "func", env, quoted)
  
  function() {
    df <- func()
    x <- df$x
    y <- df$y
    
    values <- mapply(function(val, i) {
      valx <- x[i]
      list(x = valx, y = val)
    }, y, 1:length(y), SIMPLIFY=FALSE, USE.NAMES=FALSE)
    list(data=values, tooltip = df$tooltip, xlabel = df$xlabel, ylabel = df$ylabel, evt = df$evt, d=df$d, range=df$range, type=df$type, modified=df$modified) #
  }
}

# render a bubbles multi-foci force layout using d3
renderD3BubbleMultiChart <- function(expr, env=parent.frame(), quoted=FALSE) {
  installExprFunction(expr, "func", env, quoted)
  
  function() {
    df <- func()
    x <- df$x
    y <- df$y
    list(data=y, xlabel = df$xlabel, ylabel = df$ylabel,groupby=df$groupby,feature=df$feature) #
  }
}

# render a bubbles clustered force layout using d3
renderD3BubbleClusterChart <- function(expr, env=parent.frame(), quoted=FALSE) {
  installExprFunction(expr, "func", env, quoted)
  
  function() {
    df <- func()
    y <- df$y
    list(data=y, xlabel = df$xlabel, ylabel = df$ylabel,groupby=df$groupby,feature=df$feature,range=df$range) #
  }
}

# render a box plot using d3
renderD3BoxPlot <- function(expr, env=parent.frame(), quoted=FALSE) {
  installExprFunction(expr, "func", env, quoted)
  
  function() {
    df <- func()
    y <- df$y
    list(data=y, xlabel = df$xlabel, ylabel = df$ylabel,dist=df$dist,feature=df$feature,range=df$range) #
  }
}

# Data frame or list looks like:
# 
# {
#   "Series A": [1,2,3,4,5],
#   "Series B": [6,7,8,9,10]
# }
# 
# D3 expects:
# 
# [
#   {
#     key: "Series A",
#     values: [{x:1,y:1}, {x:2,y:2}, {x:3,y:3}, {x:4,y:4}, {x:5,y:5}]
#   },
#   {
#     key: "Series B",
#     values: [{x:1,y:6}, {x:2,y:7}, {x:3,y:8}, {x:4,y:9}, {x:5,y:10}]
#   }
# ]
dataFrameToD3 <- function(df=cars) {

}
