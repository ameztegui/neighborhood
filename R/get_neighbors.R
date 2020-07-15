#' @title get_neighbors
#'
#' @description Function to compute the RDPI (Relative Distance Plasticity Index,
#' Valladares et al, (2006) Quantitative estimation of phenotypic plasticity:
#'  bridging the gap between the evolutionary concept and its ecological applications,
#'  Journal of Ecology, 94(6):1103-1116.
#'
#' @param df
#' @param plot_ID
#'
#' @return
#' @export
#'
#' @examples
#' plot <-c(rep(1:2, 9), rep(3:6, 14), rep(7,6))
#' sps_pool <- c("PINI", "PISY", "ABAL")
#' sps <- sample(sps_pool, length(plot), replace = T)
#' dbh <- rnorm(length(plot), 15, 5)
#' x <- rnorm(length(plot), 0, 5)
#' y <- rnorm(length(plot), 0, 5)
#' data <- data.frame(plot, sps, dbh, x, y)
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


    neighbors <- full_join(targets, targets, by = c("zzz", "plot"),
                           suffix = c("_target", "_neighbor")) %>%
        select(-zzz) %>%
        mutate(dist=sqrt((x_neighbor - x_target)^2 +
                             (y_neighbor - y_target)^2) )

}
