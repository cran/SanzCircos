#' make_circos_df_1
#'
#' @author Matthew Woodruff, Emory University
#'
#' @description First of a series of data-frame processing functions that take an initial data set, and processes it
#' into a Circos-compatible format.
#'
#' `make_circos_df_1()` groups sequences by population, isotype, and lineageID, and returns a summary data frame
#' containing lineage start and stop positions, mutation frequencies, and
#' scaled mutation frequencies. Can then be used to establish lineage highlighting,
#' linkage assessment, or mutation plotting. The resulting data frame is finally arranged by population, isotype,
#' mutation freq, and lineageID
#'
#' @param df Data frame to be transformed. Built to read data frames modified from the .db files
#' that IgSeq generates. Must contain columns "population", "isotype", "lineageID", and "mut_freq"
#'
#' @return Returns a data frame suitable for circos plotting.
#'
#' @export
#'
#' @import dplyr
#' @importFrom stats sd
#'
#' @examples
#'
#' df <- data.frame(population = c(rep("pop1", 10), rep("pop2", 10)),
#' isotype = c(rep("M", 4), rep("G", 6), rep("M", 6), "G", "G", "A", "A"),
#' lineageID = c(1, 2, 3, 4, 1, 1, 1, 2, 4, 5, 1, 1, 1, 5, 6, 7, 1, 3, 1, 1),
#' mut_freq = rnorm(20))
#'
#' head(df)
#'
#' circos_df <- make_circos_df_1(df)
#'
#' head(circos_df)

make_circos_df_1 <- function(df) {

  mut_freq <- end_position <- lin_mut_freq <- NULL # This assignment is unnecessary for downstream function, but semantically necassary to pass code checks for CRAN uploading.

  temp_df <- df %>%
    group_by_("population", "isotype", "lineageID") %>%
    summarize(n = n(), lin_mut_freq = mean(mut_freq), lin_mut_freq_sd = sd(mut_freq)) %>%
    ungroup() %>%
    group_by_("population") %>%
    arrange_("population", "isotype", "lin_mut_freq", "lineageID") %>%
    mutate(end_position = cumsum(n), start_position = end_position - n) %>%
    ungroup() %>%
    mutate(scaled_lin_mut_freq = scale(lin_mut_freq)) %>%
    arrange_("population", "isotype", "lin_mut_freq", "lineageID")

  return(temp_df)

}

