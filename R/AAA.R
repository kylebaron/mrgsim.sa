#' @importFrom tidyselect vars_select everything all_of
#' @importFrom dplyr mutate left_join select .data %>% n filter rename
#' @importFrom dplyr bind_rows as_tibble group_by ungroup
#' @importFrom purrr map_int map imap map_df flatten flatten_chr
#' @importFrom withr with_environment
#' @importFrom assertthat assert_that
#' @importFrom ggplot2 geom_line geom_point ggplot aes_string xlab ylab
#' @importFrom ggplot2 guides theme_bw scale_y_continuous scale_y_log10
#' @importFrom ggplot2 facet_grid facet_wrap aes scale_color_discrete theme
#' @importFrom ggplot2 scale_color_viridis_c scale_color_brewer
#' @importFrom patchwork wrap_plots
#' @importFrom rlang quos sym set_names enexpr := as_string .env
#' @importFrom tidyr unnest nest pivot_longer
#' @importFrom tibble tibble as_tibble
#' @importMethodsFrom mrgsolve as.list param update as.data.frame
#' @importFrom mrgsolve mrgsim_df ev param mrgsim outvars
#' @importFrom stats as.formula
#' @importFrom graphics plot
#' @import methods
#' 
#' @include utils.R
#' @include sens.R
#' @include parseq.R
#' 
#' 
NULL

#' Sensitivity Analysis with 'mrgsolve'
#' 
#' Perform local sensitivity analysis on ordinary differential 
#' equation based models, including ad-hoc graphical analyses based on 
#' sequences of parameters as well as local sensitivity analysis. Functions 
#' are provided for creating inputs, simulating scenarios and plotting outputs.
#' 
#' @details
#' 
#' - Local sensitivity analysis: [lsa()]
#' - Run ad-hoc sensitivity analyses: [sens_each()], [sens_grid()], [sens_run()]
#'   - Use [sens_each_data()] and [sens_grid_data()] to pass in data sets
#' - Parameter sequence generation: 
#'   - In a pipeline: [parseq_cv()], [parseq_fct()], [parseq_range()], [parseq_manual()]
#'   - Stand alone: [seq_cv()], [seq_fct()], [seq_geo()], [seq_even()]
#' 
#' @rdname mrgsim.sa
#' @name mrgsim.sa
NULL
