
#' Title
#'
#' @title get_neighbours
#'
#' @description Function to compute the RDPI (Relative Distance Plasticity Index,
#' Valladares et al, (2006) Quantitative estimation of phenotypic plasticity:
#'  bridging the gap between the evolutionary concept and its ecological applications,
#'  Journal of Ecology, 94(6):1103-1116.
#'
#'
#' @param df
#' @param plot_ID
#' @param var
#'
#' @return
#' @export
#'
#' @examples
#'
#'
create_nci_files <- function (df, plot_ID, var) {

    df_split <- group_split(df, {{plot_ID}})

    create_file <- function (df_split, var) {
        file <- pivot_wider(df_split,
                            id_cols = ID_target,
                            names_from = n_neighbour,
                            values_from = {{var}})[,-1]
        file[row(file) == col(file)] <- NA
        file
    }

    map_df(df_split, create_file, {{var}})
}
