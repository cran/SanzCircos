#' make_circos_links
#'
#' @author Matthew Woodruff, Emory University
#'
#' @description A function that takes a data frame in the format of those returned by the `make_circos_df_*` functions, and returns
#' a data frame containing all possible lineage links, and some additional metadata about each link for filtering purposes.
#'
#' ***NOTE: This function is optimized for parallelized computing. Setting the "status" argument to true will
#' eliminate this feature, and instead provide a status bar to track progress.
#'
#' @param df A data frame in the format of those returned by the `make_circos_df_*` functions
#' @param chromosome_grouping A column defining the circos chromosomes. Passed as column index or "column name".
#' @param band_grouping A column defining the circos chromosome bands. Passed as column index or "column name".
#' @param link_grouping A column defining the circos linkage IDs. Passed as column index or "column name".
#' @param start_position A column defining the start positions of the linkage ID.
#' @param end_position A column defining the end positions of the linkage ID.
#' @param status Defaults to FALSE. Removes parallelized computing, and provides a status bar to track progress.
#'
#' @return Returns a links data frame suitable for filtering and exporting using the `write_circos_links()` function.
#'
#' @export
#'
#' @import pbapply
#' @import parallel
#' @import dplyr
#'
#' @examples
#'
#' links_df <- data.frame(chrom = c(rep("chr1", 5), rep("chr2", 5)),
#' band = c(rep("band1", 3), rep("band2", 2), "band1", rep("band2", 4)),
#' link = c(1, 2, 3, 1, 2, 1, 1, 3, 4, 5),
#' start = c(1, 3, 5, 10, 35, 1, 5, 8, 13, 15),
#' end = c(3, 5, 10, 35, 39, 5, 8, 13, 15, 21))
#'
#' links <- make_circos_links(links_df, "chrom", "band", "link", "start", "end", status = TRUE)
#'
#' print(links)

make_circos_links <- function(df, chromosome_grouping, band_grouping, link_grouping, start_position, end_position, status = FALSE) {

  lineage_table <- as.data.frame(table(df[[link_grouping]]))
  lineage_table[,1] <- as.numeric(as.character(lineage_table[,1]))
  lineage_df <- lineage_table %>% dplyr::filter_("Freq > 1") %>% select(1) # makes a table of all of the lineages, and selects those with > 1 entry
  lineage_ind <- as.numeric(lineage_df[[1]]) # creates a vector of the lineage identities with links
  df <- df[df[[link_grouping]] %in% lineage_ind,] # filters the frame to only include linked entries
  lineage_list <- split(df, as.factor(df[[link_grouping]])) # splits df into a list of matched lineage dfs

  if (status == TRUE) {

     links_df <- bind_rows(pblapply(lineage_list, generate_searchable_links, chromosome_grouping, band_grouping, link_grouping, start_position, end_position))

  } else {

    cl <- makeCluster(detectCores()-1)
    clusterEvalQ(cl, library(tidyverse))
    links_df <- bind_rows(parLapply(cl = cl, lineage_list, generate_searchable_links, chromosome_grouping, band_grouping, link_grouping, start_position, end_position))
    stopCluster(cl)

  }

  return(links_df)

}
