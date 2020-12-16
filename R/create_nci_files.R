
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
#'#'
#' @param self_del If `TRUE`, a tree is not included as a neighbor of itself. If `FALSE` the attribute if the target tree will be also
#' included in the output as a neighbor of itself.
#' @param ... optional. Character or numeric variable that identifies the plots, or experimental units, within which
#' the neighbors will be considered. This information is used to split the calculations per plot, and the results
#' are merged back into a single data frame. A tree `b` will not be considered as a neighbor of tree `a` unless they belong to the same
#' plot. If this argument is missing, all trees in `df` will be considered both targets and neighbors.

#
#' @param min.target.dbh numeric The minimum dbh for a tree to be considered as a target. The function will only return neighbors of
#' those target trees with dbh >= min.target.dbh. If not specified, all trees in `df` will be considered targets
#' @param min.neighbor.dbh numeric The minimum dbh for a tree to be considered as a neighbor. All those neighbors of a given target with dbh
#' above this threshold will be ignored. If not specified, all the neighbors of a given target will be considered
#' @param max.neighbor.radius numeric maximum radius to search for neighbors. Only those neighbors located closer than this distance
#' from a target tree will be considered their neighbors.
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
#'
#' # add some restrictions on target size and searching distance
#' files2 <- create_nci_files(tree_data, dbh, sp, x, y, self_del = T, plot,
#' min.target.dbh = 100, min.neighbor.dbh = 20, max.neighbor.radius = 10)
#'
#'
create_nci_files <- function (df, dbh, sp, x, y, self_del = T,...,
                              min.target.dbh = NULL, min.neighbor.dbh = NULL,
                              max.neighbor.radius = NULL) {

    targets <- df
    dbhs <- get_neighbor_variables(df, {{dbh}}, self_del, ...)
    sps <- get_neighbor_variables(df, {{sp}}, self_del, ...)
    x_matrix <- get_neighbor_variables(df, {{x}}, self_del , ...)
    y_matrix <- get_neighbor_variables(df,  {{y}}, self_del, ...)

    xdist_matrix <- x_matrix - df %>% arrange(...) %>% pull({{x}})
    ydist_matrix <- y_matrix - df %>% arrange(...) %>% pull({{y}})
    distances <- sqrt(xdist_matrix^2 + ydist_matrix^2)


    #####################################################
    #  Set up some limits for the analysis by creating vectors that contain TRUE or FALSE for whether each observation
    # meets all of your criteria

    # Minimum target dbh
    if(!is.null(min.target.dbh)) {
        using.targets <- which (targets$dbh >= min.target.dbh)

        # Eliminate rows from the neighbor matrices for targets we're not using
        targets <- df[using.targets,]   # this creates a subset of targets for which "using.targets" is true
        dbhs <- dbhs[using.targets,]
        sps <- sps[using.targets,]
        distances <- distances[using.targets,]
    }

    if(!is.null(max.neighbor.radius)) {
        # Get rid of neighbors beyond the max radius
        distances[distances >= max.neighbor.radius] <- NA
    }
    if(!is.null(min.neighbor.dbh)) {
        distances[dbhs <= min.neighbor.dbh] <- NA
    }
    distances[distances == 0 ] <- NA
    dbhs[is.na(distances)]  <- NA
    sps[is.na(dbhs)] <- NA
    distances[is.na(dbhs)] <- NA

    list(targets = targets,
         dbhs = dbhs %>% select_if(~!all(is.na(.))),
         sps = sps %>% select_if(~!all(is.na(.))),
         distances = distances %>% select_if(~!all(is.na(.))))
}


