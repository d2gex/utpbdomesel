DomeSelectivityEstimator <- R6::R6Class("DomeSelectivityEstimator", public = list( # nolint
  length_fq = NULL,
  fishing_power = NULL,
  mesh_sizes = NULL,
  models = NULL,
  initialize = function(length_fq, fishing_power, mesh_sizes, models) {
    self$length_fq <- length_fq
    self$fishing_power <- fishing_power
    self$mesh_sizes <- mesh_sizes
    self$models <- models
  }
))
