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


apptable <- function(dat, x, y, ...) {
  # browser()
  dx <- dat[[x]]
  if (!shiny::isTruthy(y))
    return(table(dx, ...))
  table(dx, dat[[y]], ...)
}




function(input, output, session) {
  dbfile <- here::here("app/www/gbvdata.db")
  
  df <- reactive({
    qry <- sprintf("SELECT * FROM %s;", input$data)
    
    tryCatch({
      dbcon <- dbConnect(SQLite(), dbfile)
      df <- dbGetQuery(dbcon, qry)
      # browser()
      subset(df, State == input$state)
    },
    error = function(e)
      stop(e),
    finally = dbDisconnect(dbcon))
  })
  
  vars <- reactive(list(x = input$x, y = input$y))
  
  observe({
    nms <- c("", names(df()))
    updateSelectInput(session, "x", "x", nms)
    updateSelectInput(session, "y", "y", nms)
  })
  
  output$sumtable <- renderTable({
    # browser()
    if (!isTruthy(vars()$x))
      return()
    apptable(df(), vars()$x, vars()$y)
  })
  
  output$plot <- renderPlot({
    browser()
    if (!isTruthy(vars()$x))
      return()
    
    gg.aes <- if (!isTruthy(vars()$y))
      ggplot(df(), aes_string(vars()$x)) 
    else
      ggplot(df(), aes_string(vars()$x, vars()$y))
    
    gg.bar <- gg.aes + geom_bar(fill = "purple")
    
    if (input$rotate)
      gg.bar <- gg.bar + coord_flip()
    gg.bar
  })
  
  output$DT <- renderDataTable({
    if (isFALSE(input$maindata))
      return()
    df()
  })
}