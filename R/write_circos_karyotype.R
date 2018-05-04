#' write_circos_karyotype
#'
#' @author Matthew Woodruff, Emory University
#'
#' @description A function that takes a data frame in the format of those returned by the `make_circos_df_*` functions, and writes a "karyotype"
#' file for Circos plotting
#'
#' @name write_circos_karyotype
#' @param df A database to draw Circos data from
#' @param chromosome_grouping A column defining the circos chromosomes. Passed as column index or "column name"
#' @param band_grouping A column defining the circos bands. Passed as column index or "column name"
#' @param start_position A column defining the lineage start positions. Passed as column index or "column name"
#' @param end_position A column defining the lineage start positions. Passed as column index or "column name"
#' @param file_name The desired file name. Defaults to karyotype.txt in the current working directory
#' @param file_path The desired file path destination folder. Defaults to NULL
#'
#' @return Writes a Circos-compatible karyotype file to the desired directory
#'
#' @export
#'
#' @import readr
#'
#' @importFrom utils globalVariables
#'
#' @examples
#'
#' df <- data.frame(chrom = c(1, 1, 2, 2), band = c(1, 2, 1, 2), start = c(1, 5, 1, 8),
#' end = c(5, 10, 8, 13), n = c(5, 5, 8, 5))
#'
#' write_circos_karyotype(df = df, chromosome_grouping = "chrom", band_grouping = "band",
#' start_position = "start", end_position = "end",
#' file_name = "karyotype.txt", file_path = tempdir())

write_circos_karyotype <- function(df, chromosome_grouping = FALSE, band_grouping = FALSE,
                                   start_position, end_position, file_name = "karyotype.txt", file_path = NULL) {

  start <- end <- NULL # This assignment is unnecessary for downstream function, but semantically necassary to pass code checks for CRAN uploading.

  if (is.null(file_path)) {
    stop("write_circos_karyotype() requires a file_path (destination folder)")
  }

  if (chromosome_grouping == FALSE) {
    chromosome_number == 1
  } else {
    chromosome_number <- length(unique(df[[chromosome_grouping]]))
  }

  karyotype_df <- as.tibble(as.data.frame(matrix(nrow = chromosome_number, ncol = 7)))
  karyotype_df[,1] <- c(rep("chr"))
  karyotype_df[,2] <- c(rep("-"))
  karyotype_df[,5] <- c(rep(0))


  chromosome_table <- df %>% group_by(df[[chromosome_grouping]]) %>% summarize(num_seqs = sum(n))

  chr_names <- as.character(unique(df[[chromosome_grouping]]))


  for (i in 1:nrow(chromosome_table)) {

    karyotype_df[i, 3:4] <- c(rep(chr_names[i]))
    karyotype_df[i, 6] <- chromosome_table[i,2]
    karyotype_df[i, 7] <- c(rep("white"))
  }

  if (band_grouping != FALSE) {

    df[[band_grouping]] <- factor(df[[band_grouping]], ordered = TRUE, levels = c("M", "G", "A", "E", "U"))
    temp_df <- data.frame(chromosome = df[[chromosome_grouping]], band = df[[band_grouping]], start = df[[start_position]], end = df[[end_position]])
    band_table <- temp_df %>% group_by_("chromosome", "band") %>% summarize(start = min(start), end = max(end))
    banding_df <- as.tibble(as.data.frame(matrix(nrow = nrow(band_table), ncol = 7)))

    banding_df[,1] <- c(rep("band"))
    banding_df[,2] <- as.character(band_table[["chromosome"]])
    banding_df[,3:4] <- c(rep(as.character(band_table[["band"]])))
    banding_df[,5] <- band_table[["start"]]
    banding_df[,6] <- band_table[["end"]]
    banding_df[,7] <- c(rep("gneg"))

    karyotype_df <- bind_rows(karyotype_df, banding_df)

  }

  file_dir <- file.path(file_path, file_name)

  write_delim(karyotype_df, path = file_dir, delim = " ", col_names = FALSE)

}
