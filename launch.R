p <- "shiny"

if (!requireNamespace(p, quietly = TRUE))
  install.packages(p, repos = "https://cran.rstudio.com")

shiny::runApp('app', launch.browser = TRUE)
