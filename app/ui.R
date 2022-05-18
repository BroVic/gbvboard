# Source file: ui.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

library(shiny)

fluidPage(
  title = "GBV Dassboard",
  theme = shinythemes::shinytheme("darkly"),
  lang = 'en',
  
  fluidRow(
    column(
      width = 4,
      img(
        src = "jhpiego-vector-logo.svg",
        height = 200,
        width = 400,
        alt = "Jhpiego logo"
      )
    ),
    column(
      width = 4,
      titlePanel("GBV Services Mapping Data")
    ),
    column(width = 4)
  ),
  
  fluidRow(
    column(
      width = 3,
      inputPanel(
        selectInput("proj", "Project", choices = "NFWP"),
        selectInput("state", "State", choices = c("Taraba", "Kebbi", "Niger")),
        selectInput("data", "Data", choices = "FacilitiesBasic", selected = "FacilitiesBasic"),
        selectInput("x", "x", choices = ""),
        selectInput('y', 'y', choices = '')
      )
    ),
    
    # Plot Area
    column(
      width = 6, 
      wellPanel(
        plotOutput("plot"),
        checkboxInput("rotate", "Horizontal layout")
      )
    ),
    
    # Summary table
    column(width = 3, wellPanel(tableOutput("sumtable")))
  ),
  
  # Data table
  fluidRow(
    column(width = 3),
    column(
      width = 9,
      wellPanel(
        checkboxInput("maindata", "Show data"),
        dataTableOutput("DT")
      )
    )
  )
)
