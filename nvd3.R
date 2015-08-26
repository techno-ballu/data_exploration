library(shiny)

# To be called from ui.R
nvd3ChartOutput <- function(inputId, theClass, width="100%", height="100%") {
  style <- sprintf("width: %s; height: %s;", 
                   validateCssUnit(width), validateCssUnit(height))
 
  
  tagList(
    # Include CSS/JS dependencies. Use "singleton" to make sure that even
    # if multiple lineChartOutputs are used in the same page, we'll still
    # only include these chunks once.
    singleton(tags$head(
      tags$script(src="d3/d3.v3.js"),
      tags$script(src="nvd3/nv.d3.js"),
      tags$link(rel="stylesheet", type="text/css", href="nvd3/nv.d3.min.css"),
      tags$script(src="nvd3-bindings.js")
    )),
   # tag("div", list(id=inputId, class="btn-group-sm pull-right download-panel"))
    div(id=inputId, class=theClass, style=style,  tag("svg", list()))
  )
}

# To be called from server.R
renderBarChart <- function(expr, env=parent.frame(), quoted=FALSE) {
  # This piece of boilerplate converts the expression 'expr' into a
  # function called `func`. It's needed for the RStudio IDE's built-in
  # debugger to work properly on the expression.
  installExprFunction(expr, "func", env, quoted)
  
  function() {
    df <- func()
    
    if (length(df$y) == 0)
      return(NULL)
    
    x <- df$x
    d <- data.frame(y=df$y)
    obj <- mapply(function(col, name) {
      values <- mapply(function(val, i) {
        valx <- x[i]
        if (is.numeric(x[i]))
#           valx <- round(x[i],1)
          valx <- x[i]
        list(x = valx, y = val)
      }, col, 1:nrow(d), SIMPLIFY=FALSE, USE.NAMES=FALSE)
      list(key = name, values = values)
    }, d, names(d), SIMPLIFY=FALSE, USE.NAMES=FALSE)
list(data=obj, tooltip = df$tooltip, xlabel = df$xlabel, ylabel = df$ylabel, evt = df$evt, d=df$d, max=df$max, type=df$type,showVerticalXaLabel=df$showVerticalXaLabel,showDecimal= df$showDecimal) #
  }
}

# To be called from server.R
renderHorizontalBarChart <- function(expr, env=parent.frame(), quoted=FALSE) {
  # This piece of boilerplate converts the expression `expr` into a
  # function called `func`. It's needed for the RStudio IDE's built-in
  # debugger to work properly on the expression.
  installExprFunction(expr, "func", env, quoted)
  
  function() {
    dataframe <- func()
    x <- dataframe$x
    d <- data.frame(y=dataframe$y)
    
    mapply(function(col, name) {
      values <- mapply(function(val, i) {
        valx <- x[i]
        if (is.numeric(x[i]))
          valx <- round(x[i],1)
        list(label = valx, value = val)
      }, col, 1:nrow(d), SIMPLIFY=FALSE, USE.NAMES=FALSE)
      list(key = name, values = values)
    }, d, names(d), SIMPLIFY=FALSE, USE.NAMES=FALSE)
  }
}

renderScatterChart <- function(expr, env=parent.frame(), quoted=FALSE) {
  installExprFunction(expr, "func", env, quoted)
  
  function() {
    data <- func()
    xcol <- data$xcol
    ycol <- data$ycol
    xvar <- data$xvar
    yvar <- data$yvar
    
    d <- data.frame(ycol)
    colnames(d) <- yvar
    
    res <-  mapply(function(col, name) {
      values <- mapply(function(val, i) {
        valx <- xcol[i]
        list(x = valx, y = val)
      }, col, 1:nrow(d), SIMPLIFY=FALSE, USE.NAMES=FALSE)
      list(key = xvar,values = values)
    }, d, yvar, SIMPLIFY=FALSE, USE.NAMES=FALSE)
    
    list(data=res, xlab=xvar, ylab = yvar, type="scatter")
  }
}

# This function generates the client-side HTML for a button
correlationOutput <- function(inputId, theClass = "", width="100%", height="100%") {
  style <- sprintf("width: %s; height: %s;",
                   validateCssUnit(width), validateCssUnit(height))
  
  tagList(
    # Include CSS/JS dependencies. Use "singleton" to make sure that even
    # if multiple lineChartOutputs are used in the same page, we'll still
    # only include these chunks once.
    singleton(tags$head(
      tags$script(src="d3/d3.v3.js"),
      tags$script(src="nvd3/nv.d3.js"),
      tags$link(rel="stylesheet", type="text/css", href="nvd3/nv.d3.min.css"),
      tags$script(src="nvd3-bindings.js")
    )),
    div(id=inputId, class=theClass, style=style,tag("svg", list()))
  )
}

# To be called from server.R
renderLineChart <- function(expr, env=parent.frame(), quoted=FALSE) {
  # This piece of boilerplate converts the expression `expr` into a
  # function called `func`. It's needed for the RStudio IDE's built-in
  # debugger to work properly on the expression.
  installExprFunction(expr, "func", env, quoted)
  
  function() {
    df <- func()
   
    x <- df$x
    d <- data.frame(df$y)
    obj <- mapply(function(col, name) {
      values <- mapply(function(val, i) {
        valx <- x[i]
        if (is.numeric(x[i]))
          valx <- i
#           valx <- x[i]
        list(x = valx, y = val)
      }, col, 1:nrow(d), SIMPLIFY=FALSE, USE.NAMES=FALSE)
      list(key = name, values = values)
    }, d, names(d), SIMPLIFY=FALSE, USE.NAMES=FALSE)
    list(data=obj, tooltip = df$tooltip, xlabel = df$xlabel, ylabel = df$ylabel, evt = df$evt, d=df$d, max=df$max) #
 }
}

renderLineChart_1 <- function(expr, env=parent.frame(), quoted=FALSE) {
  # This piece of boilerplate converts the expression `expr` into a
  # function called `func`. It's needed for the RStudio IDE's built-in
  # debugger to work properly on the expression.
  installExprFunction(expr, "func", env, quoted)
  
  function() {
    dataframe <- func()   
    datam <- dataframe$data   
    # x <- datam[[dataframe$xLabel]]    
    d <- data.frame(y=datam[[dataframe$yLabel]])
    
    obj <- mapply(function(col, name) {
      
      values <- mapply(function(val, i) {
        list(x = i, y = val)
      }, col, 1:nrow(datam), SIMPLIFY=FALSE, USE.NAMES=FALSE)
      
      list(key = name, values = values)
      
    }, d, names(d), SIMPLIFY=FALSE, USE.NAMES=FALSE)
    
    list(data=obj, xLabel=dataframe$xLabel,yLabel=dataframe$yLabel,tooltip = dataframe$tooltip)
  }
}

