# Source file: server.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

library(shiny)
library(RSQLite)
library(ggplot2)
# library(jGBV)

read_from_db <- function(tbl, db = dbfile, ...) {
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
  
  dt.elem <- reactive({
    qry <- sprintf("SELECT * FROM %s;", input$dbtbl)
    df <- fetch_data(qry, dbfile)
    
    if (input$state != "All")
      df <- subset(df, State == input$state, select = -State)
    
    set_types(df)
  })
  
  observe({
    # browser()
    nms <- var_opts(dt.elem())
    updateSelectInput(session, "x", "x", nms)
  })
  
  observe({
    nms <- var_opts(isolate(dt.elem()))
    x.val <- input$x
    if (isTruthy(x.val)) {
      less.one <- nms[!(nms %in% x.val)]
      updateSelectInput(session, "y", "y", less.one)
    }
  })
  
  #
  ##### Outputs ####
  #
  ## The summary table
  output$sumtable <- renderTable({
    if (!isTruthy(input$x))
      return()
    .app_table(dt.elem(), input$x, input$y)
  })
  
  
  ## The main chart
  output$plot <- renderPlot({
    if (!isTruthy(input$x))
      return()

    class.x <- .get_class(dt.elem(), input$x)
    class.y <- NULL
    y.is.truthy <- isTruthy(input$y)
    if (y.is.truthy)
      class.y <- .get_class(dt.elem(), input$y)
    
    gg.aes <- ggplot(dt.elem(), aes_string(input$x)) 
    
    pp <- if (class.x == "factor") {
      if (is.null(class.y))
        gg.aes + geom_bar(fill = "purple")
      else if (class.y == "factor")
        gg.aes + geom_bar(aes_string(fill = input$y)) 
      else if (.is_num(dt.elem(), input$y))
        gg.aes + geom_boxplot()
    }
    else if (.is_num(dt.elem(), input$x)) {
      if (y.is.truthy && .is_num(dt.elem(), input$y))
        gg.aes + geom_point(aes_string(y = input$y))
      else
        gg.aes + geom_histogram()
    }
    
    if (input$rotate)
      pp <- pp + coord_flip()
    
    pp
  })
  
  
  ## The data table
  output$DT <- renderDataTable({
    if (isFALSE(input$maindata))
      return()
    dt.elem()$df
  })
}