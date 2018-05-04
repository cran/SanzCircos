#' color_circos_links
#'
#' @author Matthew Woodruff, Emory University
#'
#' @description A function designed to assign random colors from the `randomcoloR` package to a `make_circos_links()`
#' generated data frame.
#'
#' @param df A data frame containing circos links information - as from `make_circos_links()`.
#' @param hue Desired hue of the links.
#' @param luminosity Desired luminosity of the links.
#'
#' @return Returns original links data frame with an added `$color` column in RGB format for Circos plotting.
#'
#' @export
#'
#' @import randomcoloR
#' @import grDevices
#'
#' @examples
#' df <- data.frame(x = rnorm(50), y = rnorm(50))
#' str(df)
#' df <- color_circos_links(df)
#' str(df)

color_circos_links <- function(df, hue = c(" ", "random", "red", "orange", "yellow",
                                           "green", "blue", "purple", "pink", "monochrome"),
                               luminosity = c(" ", "random", "light", "bright", "dark")) {

  color_vec <- vector(length = nrow(df))

  for (i in seq_along(color_vec)) {

    color <- unlist(col2rgb(randomColor())[,1])
    color_vec[i] <- paste("color=", color[[1]], ",", color[[2]], ",", color[[3]], sep = "")

  }

  names(color_vec) <- "color"
  df$color <- color_vec
  return(df)

}

