# Source file: ui.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

library(shiny)
library(shinythemes)

fluidPage(
  title = "GBV Dashboard",
  theme = shinytheme("darkly"),
  lang = 'en',
  
  fluidRow(
    column(
      width = 3L,
      img(
        src = "jhpiego-vector-logo.svg",
        height = 100,
        width = 200,
        alt = "Jhpiego logo"
      )
    ),
    column(
      width = 9L,
      titlePanel("Data on the Mapping of GBV Services")
    )
  ), 
  sidebarLayout(
    sidebarPanel(
      width = 3L,
      selectInput(ctrl$project$id, ctrl$project$lab, c(allopts, projectNames)),
      selectInput(ctrl$state$id, ctrl$state$lab, allopts),
      selectInput(
        ctrl$tables$id,
        ctrl$tables$lab,
        names(dbTables),
        selected = "Facilities"
      ),
      
      hr(),
      "Axes:",
      br(),
      selectInput(ctrl$xvar$id, ctrl$xvar$lab, ""),
      selectInput(ctrl$yvar$id, ctrl$yvar$lab, ""),
      actionButton(ctrl$invert$id, ctrl$invert$lab),
      hr(),
      actionButton(ctrl$reset$id, ctrl$reset$lab)
    ), 
    mainPanel(tabsetPanel(
      tabPanel(
        "Chart",
        column(
          width = 9L,
          wellPanel(
            plotOutput("plot"),
            downloadButton(ctrl$saveplot$id, ctrl$saveplot$lab),
            conditionalPanel(
              condition = 'output.xvar == "factor"',
              checkboxInput(ctrl$horiz$id, ctrl$horiz$lab),
              checkboxInput(ctrl$order$id, ctrl$order$lab),
              checkboxInput(ctrl$reverse$id, ctrl$reverse$lab),
              conditionalPanel(
                "output.yvar == 'factor'",
                checkboxInput(ctrl$stack$id, ctrl$stack$lab, TRUE),
                conditionalPanel(
                  "input.stack == true",
                  checkboxInput(ctrl$fill$id, ctrl$fill$lab, FALSE)
                )
              )
            ),
            conditionalPanel(
              "output.xvar == 'integer' || output.var == 'numeric'",
              conditionalPanel(
                "output.yvar == null",
                sliderInput(ctrl$bins$id, ctrl$bins$lab, 20, 80, 30)
              ),
              radioButtons(ctrl$log$id, ctrl$log$lab, c("None", "log2", "log10"), "None")
            )
          )
        ),
        column(
          width = 3L, 
          tableOutput("sumtable")
        )
      ),
      tabPanel(
        "Data Table",
        column(
          width = 9L,
          DT::dataTableOutput("DT")
        )
      )
    ))
  )
)
