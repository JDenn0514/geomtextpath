# StatBump -------------------------------------------------------------

#' @export
#' @format NULL
#' @usage NULL

StatBump <- ggplot2::ggproto(
  "StatBump", ggplot2::Stat,
  setup_data = function(data, params) {
    # Create x_lag, and y_lag to be passed to `compute_group`
    # Factors need this to be able to compute a sigmoid function
    data <- data %>%
      dplyr::mutate(r = dplyr::row_number()) %>%
      dplyr::arrange(x) %>%
      dplyr::group_by_at(vars(-PANEL, -group, -x, -y, -r)) %>%
      dplyr::mutate(x_lag = dplyr::lag(x),
                    y_lag = dplyr::lag(y)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(r) %>%
      dplyr::select(-.data$r) %>%
      as.data.frame()
    data
  },
  compute_group = function(data, scales, smooth = 8, direction = "x") {
    data <- data %>%
      dplyr::arrange(x)

    # Handling of the special case of factors
    # Factors come as a df with one row
    if(nrow(data) == 1) {
      if(is.na(data$x_lag) | is.na(data$y_lag)) {
        return(data %>% dplyr::slice(0))
      } else {
        out <- sigmoid(data$x_lag, data$x, data$y_lag, data$y,
                       smooth = smooth, direction = direction)
        return(as.data.frame(out))
      }
    }

    # Normal case
    out <-rank_sigmoid(data$x, data$y, smooth = smooth, direction = direction) %>%
      dplyr::mutate(key = 1) %>%
      dplyr::left_join(data %>%
                         dplyr::select(-x, -y) %>%
                         dplyr::mutate(key = 1) %>%
                         dplyr::distinct(),
                       by = "key") %>%
      dplyr::select(-key) %>%
      as.data.frame()
    out
  },

  required_aes = c("x", "y")
)

