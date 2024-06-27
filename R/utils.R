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
  }
))
