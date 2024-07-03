NormalFamily <- R6::R6Class("NormalFamily", public = list( # nolint
  length_classes = NULL,
  k1 = NULL,
  k2 = NULL,
  mesh_proportion = NULL,
  rel_power = NULL,
  initialize = function(length_classes, k1, k2, mesh_proportion, rel_power) {
    self$length_classes <- length_classes
    self$k1 <- k1
    self$k2 <- k2
    self$mesh_proportion <- mesh_proportion
    self$rel_power <- rel_power
  }
))
NormalLoc <- R6::R6Class("NormalLoc", inherit = NormalFamily, public = list( # nolint
  initialize = function(length_classes, k1, k2, mesh_proportion, rel_power) {
    super$initialize(length_classes, k1, k2, mesh_proportion, rel_power)
  },
  run = function() {
    num <- (self$length_classes - (self$k1 * self$mesh_proportion))^2
    denom <- 2 * (self$k2^2)
    return(self$rel_power * exp(-num / denom))
  }
))
