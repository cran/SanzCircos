#' write_circos_histogram
#'
#' @author Matthew Woodruff, Emory University
#'
#' @description A function that takes a data.frame, and creates a histogram.txt file for input into circos
#'
#' @param df A database to draw Circos data from
#' @param chromosome_grouping A column defining the circos chromosomes. Passed as column index or "column name"
#' @param start_position A column defining the start positions of the desired value
#' @param end_position A column defining the end positions of the desired value
#' @param values The value to be plotted
#' @param file_path The desired file path destination folder. Defaults to NULL
#' @param file_name The desired file name. Defaults to histogram.txt
#'
#' @return Writes a Circos-compatible histogram file to the desired directory
#'
#' @export
#'
#' @import readr
#'
#' @examples
#'
#' df <- data.frame(chrom = c(1,1,2,2), start = c(1, 5, 1, 8),
#' end = c(5, 10, 8, 13), plotting_value = c(5, 78, 9, 2))
#'
#' write_circos_histogram(df = df, chromosome_grouping = "chrom",
#' start_position = "start", end_position = "end",
#' value = "plotting_value", file_name = "histogram.txt", file_path = tempdir())
#'


write_circos_histogram <- function(df, chromosome_grouping, start_position, end_position, values, file_name = "histogram.txt", file_path = NULL) {

  if (is.null(file_path)) {

    stop("write_circos_histogram() requires a file_path (destination folder)")

  }

  file_dir <- file.path(file_path, file_name)

  hist_df <- df %>% select(chromosome_grouping, start_position, end_position, values)

  write_delim(hist_df, path = file_dir, delim = " ", col_names = FALSE)

}
