# Source file: server.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

library(shiny)
library(RSQLite)
library(ggplot2)
# library(jGBV)

read_from_db <- function(db, tbl, ...) {
  tryCatch({
    con <- dbConnect(SQLite(), db, ...)
    df <- dbReadTable(con, tbl)
  }, error = function(e) {
    stop(e)
  }, finally = dbDisconnect(con))
  df
}

query_db <- function(qry, db = dbfile, ...) {
  stopifnot({
    is.character(qry)
    file.exists(db)
  })
  tryCatch({
    con <- dbConnect(SQLite(), db, ...)
    if (!dbIsValid(con))
      return()
    df <- dbGetQuery(con, qry)
  }, error = function(e) {
    stop(e)
  }, finally = dbDisconnect(con))
  df
}


.app_table <- function(dat, x, y, ...) {
  stopifnot(is.data.frame(dat))
  xcol <- dat[[x]]
  if (!isTruthy(y))
    return(table(xcol, ...))
  table(xcol, dat[[y]], ...)
}



var_opts <- function(df) {
  stopifnot(is.data.frame(df))
  c("", names(df))
}



set_types <- function(dat) {
  stopifnot(is.data.frame(dat))
  purrr::map_dfc(dat, function(col) {
    if (!is.character(col))
      return(col)
    if (length(unique(col)) > 10L) {
        if (all(!naijR::is_lga(col)))  # i.e. none are LGAs
          return(col)
      }
      as.factor(col)
  })
}




fetch_data <- function(qry, db) {
  stopifnot({
    is.character(qry)
    file.exists(db)
  })
  dbcon <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(dbcon))
  dbGetQuery(dbcon, qry)
}




# Server function
function(input, output, session) {
  
  .get_class <- function(dt, var)  {
    stopifnot(is.data.frame(dt))
    class(getElement(dt, var))
  }
  
  
  .is_num <- function(dat, var) {
    cl <- .get_class(dat, var)
    cl == "numeric" || cl == "integer"
  }
  
  dbfile <- here::here("app/www/gbvdata.db")
  
  dtInput <- reactive({
    qry <- sprintf("SELECT * FROM %s;", input$dbtbl)
    df <- fetch_data(qry, dbfile)
    
    if (input$state != "All")
      df <- subset(df, State == input$state, select = -State)
    
    set_types(df)
  })
  
  observe({
    # browser()
    nms <- var_opts(dtInput())
    updateSelectInput(session, "x", "x", nms)
  })
  
  observe({
    nms <- var_opts(isolate(dtInput()))
    x.val <- input$x
    if (isTruthy(x.val)) {
      less.one <- nms[!(nms %in% x.val)]
      updateSelectInput(session, "y", "y", less.one)
    }
  })
  
  varclass <- reactiveValues()
  observe({
    df <- isolate(dtInput())
    x <- input$x
    if (!isTruthy(x))
      return()
    varclass$X <- .get_class(df, x)
    varclass$Y <- NULL
    y <- input$y
    if (isTruthy(y)) 
      varclass$Y <- .get_class(df, y)
  })
  
  #
  ##### Outputs ###
  #
  ## The main chart
  #################
  output$plot <- renderPlot({
    if (!isTruthy(input$x))
      return()
    
    gg.aes <- ggplot(dtInput(), aes_string(input$x)) 
    
    pp <- if (varclass$X == "factor") {
      if (is.null(varclass$Y))
        gg.aes + geom_bar(fill = "purple")
      else if (varclass$Y == "factor")
        gg.aes + 
        geom_bar(aes_string(fill = input$y), position = input$position, show.legend = input$legend, orientation = ) 
      else if (.is_num(dtInput(), input$y))
        gg.aes + geom_boxplot()
    }
    else if (.is_num(dtInput(), input$x)) {
      if (y.is.truthy && .is_num(dtInput(), input$y))
        gg.aes + geom_point(aes_string(y = input$y))
      else
        gg.aes + geom_histogram(binwidth = input$binwidth)
    }
    
    if (input$rotate)
      pp <- pp + coord_flip()
    
    pp
  })
  
  
  output$xvar <- reactive({
    return(varclass$X)
  })
  outputOptions(output, "xvar", suspendWhenHidden = FALSE)
  
  ## The summary table
  ####################
  output$sumtable <- renderTable({
    if (!isTruthy(input$x))
      return()
    .app_table(dtInput(), input$x, input$y)
  })
  
  
  
  
  ## The data table
  #################
  output$DT <- renderDataTable({
    if (isFALSE(input$maindata))
      return()
    dtInput()$df
  })
}