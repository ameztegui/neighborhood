#' @title get_neighbors
#'
#' @description Function to obtain the neighbors of each tree
#' from a file containing the identities of the trees and, optionally,
#'  a variable that identifies the plots.
#'
#' @param df the data frame containing the trees for which we want to identify
#' the neighbors, and their associated information
#' @param plot_ID optional. Variable that identifies the plots, or experimental units, within which
#' the neighbors will be searched. If this variable exists, a tree "a" can only be a neighbor of
#' another tree "b", if it is fulfilled that plot(a) = plot(b).
#'
#' @return This function identifies the neighbors of each potential "target" tree and creates a data frame
#' that contains a row for each neighbor of each tree in the original data frame. Variables characterizing
#' target and neighbor trees are identified with the suffixes "_target" and "_neighbour", respectively.
#' It also computes the distance between each target - neighbor pair, in the same units as provided by
#' the "x" and "y" coordinates.
#'
#' @examples
#' plot <- c(rep(1:2, 9), rep(3:6, 14), rep(7,6))
#' sps_pool <- c("PINI", "PISY", "ABAL")    # the pool of species
#' sps <- sample(sps_pool, length(plot), replace = T)
#' dbh <- rnorm(length(plot), 15, 5)
#' x <- rnorm(length(plot), 0, 5)     # x coordinates of the tree
#' y <- rnorm(length(plot), 0, 5)     # y coordinates of the tree
#' data <- data.frame(plot, sps, dbh, x, y)
#'
#' neighbors <- get_neighbors(data, plot)
#'
#'
get_neighbors <- function (df, plot_ID) {

    targets <- df %>%
        mutate(zzz = 1) %>%
        arrange({{plot_ID}}) %>%
        mutate(n = sequence(tabulate({{plot_ID}})),
               ID = paste0({{plot_ID}},"_",
                           str_pad(n, 3, pad = 0)))


    neighbors <- full_join(targets, targets, by = c("zzz",
                                                    quo_name(enquo(plot_ID))),
                           suffix = c("_target", "_neighbor")) %>%
        select(-zzz) %>%
        mutate(dist=sqrt((x_neighbor - x_target)^2 +
                             (y_neighbor - y_target)^2) )

}
