# Source file: server.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

library(shiny)

# Defines how the summary table is going to be represented
app_table <- function(dat, inputObj, ...) {
  stopifnot(is.data.frame(dat))
  x <- inputObj$x
  y <- inputObj$y
  xcol <- dat[[x]]
  freq <- "Frequency"
  if (!isTruthy(y)) {
    if (is.factor(xcol))
      return(table(xcol, dnn = x, ...))
    if (is.numeric(xcol)) {
      sm <- summary(xcol, ...)
      nm <- names(sm)
      sm <- as.matrix(as.numeric(sm))
      dimnames(sm) <- list(nm, "Value")
      return(sm)
    }
  }
  table(xcol, dat[[y]], ...) |>
    as.data.frame() |>
    setNames(c(x, y, freq)) |>
    tidyr::pivot_wider(names_from = dplyr::all_of(y),
                       values_from = tidyselect::all_of(freq))
}




# Sets the variable options for the selector input widgets
var_opts <- function(df) {
  stopifnot(is.data.frame(df))
  c("", names(df))
}




# Sets the columns of the data frame to appropriate types
# Necessary because the SQL types are not necessarily adequate
# for our purposes.
set_types <- function(dat) {
  stopifnot(is.data.frame(dat))
  
  # Sets the type of a given column based on certain conditions
  setType <- function(col) {
    if (!is.character(col))
      return(col)
    
    tmpcol <- na.omit(col)  # in case there are missing values
    
    if (length(unique(col)) > 10L) {
      tryCatch({
        dgt <- "[[:digit:]]"
        dgtrgx <- paste0(dgt, "{4}-", dgt, "{2}-", dgt, "{2}.?")
        
        if (all(grepl(dgtrgx, tmpcol))) {
          col <- if (all(nchar(tmpcol)) == 10L)
            as.Date(col)
          else
            as.POSIXct(col)
          return(col)
        }
      }, error = function(e) col)
      
      if (all(!naijR::is_lga(tmpcol)))
        return(col)
    }
    as.factor(col)
  }
  
  purrr::map_dfc(dat, setType)
}




fetch_data <- function(qry, db) {
  require(RSQLite, quietly = TRUE)
  stopifnot({
    is.character(qry)
    file.exists(db)
  })
  dbcon <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(dbcon))
  dbGetQuery(dbcon, qry)
}


update_var_control <- function(session, var, ...) {
  stopifnot(is.character(var))
  updateSelectInput(session, var, stringr::str_to_title(var), ...)
}


reset_plot_orientation <- function(session, controls) {
  updateCheckboxInput(
    session, 
    controls$horiz$id, 
    controls$horiz$lab, 
    value = FALSE
  )
}


make_plot <- function(data, widgets) {
  require(ggplot2, quietly = TRUE)
  x.var <- widgets$x
  y.var <- widgets$y
  
  ycol <- NULL
  ptitle <- x.var
  if (isTruthy(y.var)) {
    ycol <- data[[y.var]]
    ptitle <- sprintf("%s and %s", x.var, y.var)
  }
  xcol <- data[[x.var]]
  p <- plotMethod(xcol, ycol, df = data, widgets)
  p <- p + 
    labs(x = x.var, y = y.var, title = paste("Plot of", ptitle))
  
  if (widgets$rotate)
    p <- p + coord_flip()
  p
}



.get_class <- function(dt, var)  {
  stopifnot(is.data.frame(dt))
  class(getElement(dt, var))
}





# === Server function ===
function(input, output, session) {
  dtInput <- reactive({
    
    grepcols <- function(rgx) {
      which(!grepl(rgx, names(df)))
    }
    
    tbl <- dbTables[[input$dbtbl]]
    qry <- sprintf("SELECT * FROM %s;", tbl)
    df <- fetch_data(qry, "data.db")
    
    # get rid of 'id' columns
    df <- subset(df, select = grepcols("_id$"))
    
    # remove GPS coordinates from Facility data
    if (input[[ctrl$tables$id]] == "Facilities")
      df <- subset(df, select = grepcols("^gps_"))
    
    if (input$state != opts$allopts)
      df <- subset(df, State == input$state, select = -State)
    
    set_types(df)
  },
  label = "Data initialization")
  
  observe({
    project <- input$proj
    
    updateSelectInput(
      session,
      ctrl$state$id, 
      ctrl$state$lab, 
      choices = c(opts$allopts, projectStates[[project]])
    )
  })
  
  rSelectOpts <- reactiveValues()
  
  observe({                 # x-variable control
    
    rSelectOpts$x <- var_opts(dtInput())
    xval <- input$x
    
    if (!isTruthy(xval)) {
      xval <- NULL
      yval <- isolate(input$y)
      if (isTruthy(yval))
        xval <- yval
    }
    
    update_var_control(
      session,
      ctrl$xvar$id,
      choices = isolate(rSelectOpts$x),
      selected = xval
    )
  })
  
  observe({              # y-variable control
    nms <- rSelectOpts$x
    x.val <- input$x
    
    rSelectOpts$y <- if (isTruthy(x.val))
      nms[!(nms %in% x.val)]
    else
      nms
    
    update_var_control(
      session,
      ctrl$yvar$id,
      choices = isolate(rSelectOpts$y),
      selected = isolate(input$y)
    )
  })
  
  observeEvent(input$reset,
               { 
                 df <- isolate(dtInput())
                 rSelectOpts$x <- var_opts(df)
                 
                 update_var_control(session,
                                    ctrl$xvar$id,
                                    choices = rSelectOpts$x,
                                    selected = character(1))
                 
                 update_var_control(session,
                                    ctrl$yvar$id,
                                    choices = isolate(rSelectOpts$y),
                                    selected = character(1))
                 
                 reset_plot_orientation(session, ctrl)
               })
  
  observeEvent({
    !input$stack
  },
  updateCheckboxInput(session, ctrl$fill$id, ctrl$fill$lab, value = FALSE))
  
  varclass <- reactiveValues()
  
  observe({
    df <- isolate(dtInput())
    x <- input$x
    
    if (!isTruthy(x))
      return()
    
    varclass$X <- .get_class(df, x)
    
    if (varclass$X != "factor")
      reset_plot_orientation(session, ctrl)
    
    y <- input$y
    
    if (isTruthy(y)) 
      varclass$Y <- .get_class(df, y)
  })
  
  observeEvent(input$invert, {
    upd <-
      function(var, sel, ss = session, cc = isolate(rSelectOpts$x))
        update_var_control(ss, var, choices = cc, selected = sel)
    
    upd(ctrl$xvar$id, isolate(input$y))
    upd(ctrl$yvar$id, isolate(input$x))
  })
  
  #
  ##### Outputs ###
  #
  ## The main chart
  #################
  rGgObj <- reactiveVal()
  
  output$plot <- renderPlot({
    
    if (!isTruthy(input$x))
      return()
    
    pp <- make_plot(dtInput(), input)
    
    rGgObj(pp)
    isolate(rGgObj())
  })
  
  
  output$saveplot <- downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, isolate(rGgObj()))
    }
  )
  
  
  output$xvar <- reactive(varclass$X,
                          label = "Conditional panel ctrl") 
  output$yvar <- reactive(varclass$Y, 
                          label = "Additional variable panel")
  outputOptions(output, "xvar", suspendWhenHidden = FALSE)
  outputOptions(output, "yvar", suspendWhenHidden = FALSE)
  
  
  
  ####################
  ## The summary table
  ####################
  output$sumtable <- renderTable({
    
    if (!isTruthy(input$x))
      return()
    
    app_table(dtInput(), input)
  },
  rownames = TRUE,
  bordered = TRUE)
  
  
  
  #################
  ## The data table
  #################
  output$DT <- renderDataTable({
    
    if (isFALSE(input$maindata))
      return()
    
    dtInput()
  })
}
