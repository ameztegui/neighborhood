
#' create_nci_files
#'
#' @title create_nci_files
#'
#' @description function to create the files needed to estimate NCI-growth euqations using
#' the anneal function of the likelihood package.
#'
#'
#' @param df the dataframe containing information about the target trees and their neighbors. It can be provided
#' by the users or obtained using the `get_neighbors` function
#' @param plot_ID optional. Variable that identifies the plots, or experimental units, within which
#' the neighbors will be considered. This information is used to split the calculations per plot, and the results
#' are merged back into a single data frame.
#' @param var The variable that we want to extract from the neighbors. It can be either a numeric, logical or
#' character variable.
#'
#' @return a data frame containing as many rows as target trees. Each row will contain as many values (columns)
#' as neighbors has the corresponding target tree. Cells contain values of the variable "var" for each neighbor tree, and
#' missing values are coded as NA
#'
#' @examples
#'
#' data(neighbors)
#'
#' dbhs <- create_nci_files(neighbors, plot, dbh_neighbor)
#' sps <- create_nci_files(neighbors, plot, sps_neighbour)
#' distances <- create_nci_files(neighbors, plot, dist)
#'
#'
create_nci_files <- function (df, plot_ID, var) {

    df_split <- group_split(df, {{plot_ID}})

    create_file <- function (df_split, var) {
        file <- pivot_wider(df_split,
                            id_cols = ID_target,
                            names_from = n_neighbor,
                            values_from = {{var}})[,-1]
        file[row(file) == col(file)] <- NA
        file
    }

    map_df(df_split, create_file, {{var}})
}
