#' @title get_neighbors
#'
#' @description Function to obtain the neighbors of each tree
#' from a file containing the identities of the trees and, optionally,
#'  a variable that identifies the plots.
#'
#' @param df the data frame containing the trees for which we want to identify
#' the neighbors, and their associated information
#' @param plot_ID **optional**. Variable that identifies the plots, or experimental units, within which
#' the neighbors will be searched. If this variable exists, a tree "a" can only be a neighbor of
#' another tree "b", if it is fulfilled that plot(a) == plot(b).
#' @param coords **optional** Character vector containing the names of the columns in `df`containing the x and y coordinates of the trees.
#' By default `coords = c("x", "y")`, i.e. it assumes taht the columns are called "x" and "y"
#' @param suffixes **optional** character vector containing the suffixes that will be added to the variables in `df`to dientify target trees and neighbours.
#' By default it takes the value `suffixes = c("_target", "_neighbor")`, but can take any other value defined by user.
#' @param max_dist numeric Maximum distance to search for neighbours of target trees. It takes value 10000 by default.
#'
#' @return This function identifies the neighbors of each potential "target" tree and creates a data frame
#' that contains a row for each neighbor of each tree in the original data frame. Variables characterizing
#' target and neighbor trees are identified with the suffixes "_target" and "_neighbour", respectively.
#' It also computes the distance between each target - neighbor pair, in the same units as provided by
#' the "x" and "y" coordinates.
#'
#' @examples
#' data(tree_data)
#'
#' neighbors <- get_neighbors(tree_data, plot)
#'
#' # If not plots are to be considered
#' all_neighbors <- get_neighbors(tree_data)
#'
#' # specify suffix for target and neighbors, and maximum distance
#' neighbors2 <- get_neighbors(tree_data, plot, suffixes = c("cible", "voisin"), max_dist = 10)

#'
#'
get_neighbors <- function (df, plot_ID, coords, suffixes, max_dist = 10000 ) {

    if(missing(coords)) coords <- c("x", "y")
    if(missing(suffixes)) suffixes <- c("target", "neighbor")

    x_target <- paste0(coords[1], suffixes[1])
    y_target <- paste0(coords[2], suffixes[1])
    x_neighbor <- paste0(coords[1], suffixes[2])
    y_neighbor <- paste0(coords[2], suffixes[2])

    #####

    if(missing(plot_ID)) {
        targets <- df %>%
            mutate(zzz = 1) %>%
            mutate(ID = sequence(nrow(df)))

        neighbors <- full_join(targets, targets, by ="zzz",
                               suffix = c(suffixes))

    } else {

        targets <- df %>%
            mutate(zzz = 1) %>%
            arrange({{plot_ID}}) %>%
            mutate(n = sequence(tabulate({{plot_ID}})),
                   ID = paste0({{plot_ID}},"_",
                               str_pad(n, 3, pad = 0)))

        neighbors <- full_join(targets, targets, by = c("zzz",
                                                        quo_name(enquo(plot_ID))),
                               suffix = suffixes)
    }

    neighbors %>%
        select(-zzz) %>%
        mutate(dist = sqrt((!!sym(x_neighbor) - !!sym(x_target))^2 +
                               (!!sym(y_neighbor) - !!sym(y_target))^2)) %>%
        filter(dist <= max_dist)
}
