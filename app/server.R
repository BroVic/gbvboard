# Source file: server.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

library(shiny)
library(ggplot2)
# library(jGBV)

read_from_db <- function(db, tbl, ...) {
  require(RSQLite, quietly = TRUE)
  stopifnot({
    file.exists(db)
    is.character(tbl)
  })
  con <- dbConnect(SQLite(), db, ...)
  on.exit(dbDisconnect(con))
  dbReadTable(con, tbl)
}


.app_table <- function(dat, x, y, ...) {
  stopifnot(is.data.frame(dat))
  xcol <- dat[[x]]
  if (!isTruthy(y))
    return(table(xcol, ...))
  table(xcol, dat[[y]], ...) |>
    as.data.frame() |>
    setNames(c(x, y, "Freq")) |>
    tidyr::pivot_wider(names_from = dplyr::all_of(y), values_from = "Freq")
}



var_opts <- function(df) {
  stopifnot(is.data.frame(df))
  c("", names(df))
}



set_types <- function(dat) {
  stopifnot(is.data.frame(dat))
  ..setType <- function(col) {
    if (!is.character(col))
      return(col)
    if (length(unique(col)) > 10L) {
      col <- tryCatch({
        numchars10 <- all(nchar(col)) == 10L
        if (numchars10)
          as.Date(col)
        else
          as.POSIXct(col)
      }, error = function(e)
        col)
      if (isFALSE(is.character(col)))
        return(col)
      if (all(!naijR::is_lga(col))) # i.e. none are LGAs
        return(col)
    }
    as.factor(col)
  }
  purrr::map_dfc(dat, ..setType)
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


# S4 generic and methods
# setGeneric("plotMethod", function(x, y, ..., df) standardGeneric("plotMethod"))
# 
# setMethod("plotMethod", c("factor", "factor"), function(x, y, df) {
#   ggplot(df, aes_string(x)) +
#     geom_bar(aes_string(fill = y))
# })
# 
# setMethod("plotMethod", "factor", function(x, y = NULL, ..., df) {
#   ggplot(df, aes_string(x, fill = ...))
# })
# 
# setMethod("plotMethod", "numeric", function(x, y = NULL, ..., df) {
#   ggplot(df, aes_string(x)) +
#     geom_histogram()
# })
# 
# setMethod("plotMethod", c("numeric", "numeric"), function(x, y, ..., df) {
#   ggplot(df, aes_string(x, y)) +
#     geom_point()
# })
# 
# setMethod("plotMethod", c("factor", "numeric"), function(x, y, ..., df) {
#   ggplot(df, aes_string(x, y)) +
#     geom_boxplot()
# })
# 
# 
# make_plot <- function(data, x.var, y.var) {
#   xcol <- data[[x.var]]
#   ycol <- NULL
#   if (!is.null(y.var))
#     ycol <- data[[y.var]]
#   browser()
#   plotMethod(xcol, ycol, data, x = x.var, y = y.var)
# }


.get_class <- function(dt, var)  {
  stopifnot(is.data.frame(dt))
  class(getElement(dt, var))
}


.is_num <- function(dat, var) {
  cl <- .get_class(dat, var)
  cl == "numeric" || cl == "integer"
}




# Server function
function(input, output, session) {
  dtInput <- reactive({
    tbl <- dbTables[[input$dbtbl]]
    qry <- sprintf("SELECT * FROM %s;", tbl)
    df <- fetch_data(qry, "data.db")
    
    if (input$state != opts$allstates)
      df <- subset(df, State == input$state, select = -State)
    
    set_types(df)
  },
  label = "Data initialization")
  
  observe({
    project <- input$proj
    updateSelectInput(
      session,
      controls$state$id, 
      controls$state$label, 
      choices = c(opts$allstates, projectStates[[project]])
    )
  })
  
  select.opts <- reactiveValues()
  observe({ # x-variable control
    select.opts$x <- var_opts(dtInput())
    xval <- isolate(input$x)
    if (!isTruthy(xval))
      xval <- NULL
    update_var_control(session, controls$xvar$id, choices = select.opts$x, selected = xval)
  })
  
  observe({ # y-variable control
    nms <- select.opts$x
    x.val <- input$x
    select.opts$y <- if (isTruthy(x.val))
      nms[!(nms %in% x.val)]
    else
      nms
    update_var_control(
      session,
      controls$yvar$id,
      choices = isolate(select.opts$y),
      selected = isolate(input$y)
    )
  })
  
  observeEvent(input$reset,
               { 
                 df <- isolate(dtInput())
                 select.opts$x <- var_opts(df)
                 update_var_control(session,
                                    controls$xvar$id,
                                    choices = select.opts$x,
                                    selected = character(1))
                 update_var_control(session,
                                    controls$yvar$id,
                                    choices = isolate(select.opts$y),
                                    selected = character(1))
                 updateCheckboxInput(session, controls$horiz$id, controls$horiz$label)
                 # updateCheckboxInput(session, controls$stack$id, controls$stack$label, value = TRUE)
               })
  
  varclass <- reactiveValues()
  observe({
    df <- isolate(dtInput())
    x <- input$x
    if (!isTruthy(x))
      return()
    varclass$X <- .get_class(df, x)
    y <- input$y
    if (isTruthy(y)) 
      varclass$Y <- .get_class(df, y)
  })
  
  observeEvent(input$invert, {
    upd <-
      function(var, sel, ss = session, cc = isolate(select.opts$x))
        update_var_control(ss, var, choices = cc, selected = sel)
    upd(controls$xvar$id, isolate(input$y))
    upd(controls$yvar$id, isolate(input$x))
  })
  
  #
  ##### Outputs ###
  #
  ## The main chart
  #################
  output$plot <- renderPlot({
    xvar <- input$x
    yvar <- input$y
    df <- dtInput()
    
    if (!isTruthy(xvar))
      return()
    
    # pp <- make_plot(df, xvar, yvar)
    gg.aes <- ggplot(df, aes_string(xvar))

    pp <- if (varclass$X == "factor") {
      if (is.null(varclass$Y))
        gg.aes + geom_bar(fill = "darkgreen")
      else if (varclass$Y == "factor") {
        pos <- if (input$stack) "stack" else "dodge"
        gg.aes +
        geom_bar(aes_string(fill = yvar), position = pos)
      }
      else if (.is_num(df, yvar))
        gg.aes + geom_boxplot()
    }
    else if (.is_num(df, xvar)) {
      if (isTruthy(yvar) && .is_num(df, yvar))
        gg.aes + geom_point(aes_string(y = yvar))
      else
        gg.aes + geom_histogram(bins = input$bins)
    }
    
    if (input$rotate)
      pp <- pp + coord_flip()
    
    pp
  })
  
  
  output$xvar <- reactive(varclass$X,
                          label = "Conditional panel controls")
  # browser()
  output$yvar <- reactive(varclass$Y, label = "Additional bivariate panel")
  outputOptions(output, "xvar", suspendWhenHidden = FALSE)
  outputOptions(output, "yvar", suspendWhenHidden = FALSE)
  
  
  
  ####################
  ## The summary table
  ####################
  output$sumtable <- renderTable({
    if (!isTruthy(input$x))
      return()
    .app_table(dtInput(), input$x, input$y)
  })
  
  
  
  #################
  ## The data table
  #################
  output$DT <- renderDataTable({
    if (isFALSE(input$maindata))
      return()
    dtInput()
  })
}