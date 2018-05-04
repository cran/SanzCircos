#' generate_searchable_links
#'
#' @author Matthew Woodruff, Emory University
#'
#' @description A function serving as the core of the make_circos_links function. Takes a data frame containing a single lineage,
#' and generates a links data frame containing all possible combinations of that lineage.
#'
#' @param df A data frame containing all sub-lineages from the same lineageID
#' @param chromosome_grouping A column defining the circos chromosomes. Passed as column index or "column name".
#' @param band_grouping A column defining the circos chromosome bands. Passed as column index or "column name".
#' @param link_grouping A column defining the circos linkage IDs. Passed as column index or "column name".
#' @param start_position A column defining the start positions of the linkage ID.
#' @param end_position A column defining the end positions of the linkage ID.
#'
#' @import tibble
#'
#' @return Returns a data frame in a pre-"circos links" format for later use within the `make_circos_links` function.
#'
#'
#'

generate_searchable_links <- function(df, chromosome_grouping, band_grouping, link_grouping,
                                      start_position, end_position) {

  links_df <- as.tibble(as.data.frame(matrix(nrow = 1, ncol = 11, rep(NA))))

  for (i in 1:(nrow(df)-1)) {

    for (j in (i+1):nrow(df)){

      link <- as.tibble(as.data.frame(matrix(nrow = 1, ncol = 11)))

      link[1,1] <- as.integer(as.character(df[[link_grouping]][i]))
      link[1,2] <- as.character(df[[chromosome_grouping]][i])
      link[1,3] <- as.character(df[[band_grouping]][i])
      link[1,4] <- df[[start_position]][i]
      link[1,5] <- df[[end_position]][i]
      link[1,6] <- df[[end_position]][i] - df[[start_position]][i]
      link[1,7] <- as.character(df[[chromosome_grouping]][j])
      link[1,8] <- as.character(df[[band_grouping]][j])
      link[1,9] <- df[[start_position]][j]
      link[1,10] <- df[[end_position]][j]
      link[1,11] <- df[[end_position]][j] - df[[start_position]][j]

      links_df <- bind_rows(links_df, link)

    }
  }

  links_df <- links_df[2:nrow(links_df),]
  names(links_df) <- c("linkage_id", "chr1", "band1", "chr1_pos_start", "chr1_pos_end", "n1", "chr2", "band2", "chr2_pos_start", "chr2_pos_end", "n2")
  return(links_df)

}

