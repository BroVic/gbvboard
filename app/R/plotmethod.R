# Source file: plotmethod.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

library(ggplot2)

# S4 generic 
setGeneric("plotMethod", function(xcol, ycol, df, inputs)
  standardGeneric("plotMethod"),
  signature = c("xcol", "ycol"))



# S4 methods
setMethod("plotMethod", c("factor", "factor"), function(xcol, ycol, df, inputs) {
  pos <- if (inputs$stack) "stack" else "dodge"
  ggplot(df, aes_string(inputs$x, fill = inputs$y)) +
    geom_bar(position = pos)
})


setMethod("plotMethod", c("factor", "NULL"), function(xcol, ycol, df, inputs) {
  ggplot(df, aes_string(inputs$x)) +
    geom_bar(fill = "brown")
})


setMethod("plotMethod", c("numeric", "NULL"), function(xcol, ycol, df, inputs) {
  gg <- ggplot(df, aes_string(inputs$x)) +
    geom_histogram(bins = inputs$bins)
  .transformLog(gg, inputs)
})


setMethod("plotMethod", c("numeric", "numeric"), function(xcol, ycol, df, inputs) {
  gg <- ggplot(df, aes_string(inputs$x, inputs$y)) +
    geom_point()
  .transformLog(gg, inputs)
})


setMethod("plotMethod", c("factor", "numeric"), function(xcol, ycol, df, inputs) {
  ggplot(df, aes_string(inputs$x, inputs$y)) +
    geom_boxplot()
})



## Internal to methods
.transformLog <- function(gg, inputs) {
  stopifnot(inherits(gg, "gg"))
  logval <- inputs[[ctrl$log$id]]
  if (logval != "None")
    gg <- gg + 
      scale_x_continuous(trans = logval) +
      scale_y_continuous(trans = logval)
  gg
}