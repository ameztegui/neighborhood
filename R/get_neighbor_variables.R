
#' get_neighbor_variables
#'
#' @title get_neighbor_variables
#'
#' @description function to obtain the attributes of all the neighbors of a given set of target trees
#'
#' @param df the data frame containing information about the target trees.
#' @param var The variable that we want to extract from the neighbors. It can be either a numeric, logical or
#' character variable.
#' @param self_del If `TRUE`, a tree is not included as a neighbor of itself. If `FALSE` the attribute if the target tree will be also
#' included in the output as a neighbor of itself.
#' @param ... optional. Character or numeric variable that identifies the plots, or experimental units, within which
#' the neighbors will be considered. This information is used to split the calculations per plot, and the results
#' are merged back into a single data frame. A tree `b` will not be considered as a neighbor of tree `a` unless they belong to the same
#' plot. If this argument is missing, all trees in `df` will be considered both targets and neighbors.
#' @return a data frame containing as many rows as target trees, and as many columns as the maximum number of neighbors per tree.
#' Cells contain values of the variable "var" for each neighbor tree, and missing values are coded as `NA`.
#'
#' @examples
#'
#' data(tree_data)
#'
#' # Get dbhs of all the neighbors, as if they were all in the same plot
#' dbhs <-get_neighbor_variables(tree_data, var = dbh, self_del = F)
#'
#' # Get species of all the neighbors (incuding target tree as its own neighbor)
#' species <-get_neighbor_variables(tree_data, var = sps, plot, self_del = T)
#'


get_neighbor_variables <- function (df,  var, self_del = T, ... ) {

    library(tidyverse)

    df_split <- group_split(df, ...)


    create_file <- function (df_split, var) {
        var2 = pull(df_split, {{var}})
        file <-data.frame(matrix(rep(var2, length(var2)), ncol = length(var2), byrow = T ))

        if(self_del == T) file[row(file) == col(file)] <- NA
        file
    }

    map_df(df_split, create_file, {{var}})
}




