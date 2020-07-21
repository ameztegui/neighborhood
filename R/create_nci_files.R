
#' create_nci_files
#'
#' @title create_nci_files
#'
#' @description function to create the files needed to estimate NCI-growth equations using
#' the anneal function of the likelihood package.
#'
#'
#' @param df the data frame containing information about the target trees.
#' @param dbh variable in `df` that contains the diameter of the trees.
#' @param sp variable in `df` that contains the specific identity of trees
#' @param x variable in `df` that contains the X coordinate of the trees. Coordinates can be referred to any reference point (typically
#' they will refer to either one of the plot corners or the plot center), but all of them must be referred to the same point to allow calculating distances
#' between trees.
#' @param y variable in `df` that contains the Y coordinate of the trees.  Coordinates can be referred to any reference point (typically
#' they will refer to either one of the plot corners or the plot center), but all of them must be referred to the same point to allow calculating distances
#' between trees.
#'
#' @param plot_ID optional. Variable that identifies the plots, or experimental units, within which
#' the neighbors will be considered. This information is used to split the calculations per plot, and the results
#' are merged back into a single data frame. A tree `b` will not be considered as a neighbor of tree `a` unless they belong to the same
#' `plot_ID`. If this argument is missing, all trees in `df` will be considered both targets and neighbors.
#'
#' @param self_del If `TRUE`, a tree is not included as a neighbor of itself. If `FALSE` the attribute if the target tree will be also
#' included in the output as a neighbor of itself.
#'
#' @return a list containing three data frames with the values of dbh, species and distance to target tree for each neighbor tree.
#' Each data frame will contain as many rows as target trees, and as many columns as the maximum number of neighbors per tree.Missing values are coded as `NA`.
#'
#' @examples
#'
#' data(tree_data)
#'
#' files <- create_nci_files(tree_data, dbh = dbh, sp = sps,x = x, y = y, plot)
#'
create_nci_files <- function (df, dbh, sp, x, y, plot_ID, self_del = T) {

    dbhs <- get_neighbor_variables(df, {{plot_ID}}, {{dbh}}, self_del = {{self_del}})
    sps <- get_neighbor_variables(df, {{plot_ID}}, {{sp}}, self_del = {{self_del}})
    x_matrix <- get_neighbor_variables(df, {{plot_ID}}, {{x}}, self_del = {{self_del}})
    y_matrix <- get_neighbor_variables(df, {{plot_ID}},  {{y}}, self_del = {{self_del}})

    xdist_matrix <- x_matrix - df %>% arrange({{plot_ID}}) %>% pull({{x}})
    ydist_matrix <- y_matrix - df %>% arrange({{plot_ID}}) %>% pull({{y}})
    distances <- sqrt(xdist_matrix^2 + ydist_matrix^2)


    list(dbhs = dbhs, species = sps, distances = distances)
}



