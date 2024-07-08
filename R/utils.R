#' @title Mixin Utility class
#'
#' @description
#' It holds a few util methods used across all classes in the this R package
MixinUtilities <- R6::R6Class("MixinUtilities", public = list( # nolint

  # // @formatter:off
  #' @description
  #' Create an empty dataframe
  #'
  #' @param col_names vector with the column names of the empty dataframe
  #' @export
  # // @formatter:on
  create_empty_dataframe = function(col_names) {
    df <- data.frame(matrix(nrow = 0, ncol = length(col_names)))
    colnames(df) <- col_names
    return(df)
  },
  # // @formatter:off
  #' @description
  #' Convert a dataframe to matrix without row and column names. Such conversion could only be extensible
  #' to a subset of columns only.
  #'
  #' @param col_names vector with the column names of the empty dataframe
  #' @export
  # // @formatter:on
  df_to_unname_matrix = function(data, from_col = NULL, to_col = NULL) {
    from_col <- ifelse(is.null(from_col), 1, from_col)
    to_col <- ifelse(is.null(to_col), length(names(data)), to_col)
    return(
      unname(as.matrix(data[, from_col:to_col]))
    )
  },
  # @formatter:off
  #' @description
  #' Backtransform the log(mode(x)) into mode(x) applying e^(u - sigma^2)
  #'
  #' @param log_mean lognormal mean
  #' @returns the backtransformed mode
  #' @export
  # @formatter:on
  back_transformed_mode = function(log_mode, log_sigma, rel_mesh_size) {
    log_sigma_square <- log_sigma^2
    return(
      exp(log_mode - log_sigma_square) / rel_mesh_size
    )
  },
  # @formatter:off
  #' @description
  #' Backtransform the log(sqrt(sigma^2)) into sigma applying e^(2* u - sigma^2) * (e^(sigma^2) - 1)
  #'
  #' @param log_mean lognormal mean
  #' @returns the backtransformed standard deviation
  #' @export
  # @formatter:on
  back_transformed_sd = function(log_mode, log_sigma) {
    log_sigma_square <- log_sigma^2
    return(
      sqrt(exp(2 * log_mode + log_sigma_square) * (exp(log_sigma_square) - 1))
    )
  }
))
