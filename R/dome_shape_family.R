NormalFamily <- R6::R6Class("NormalFamily", public = list( # nolint
  # @formatter:off
  #' @field length_classes length distribution classes
  length_classes = NULL,
  #' @field k1 SELECT k for nomal.loc; SELECT k1 for normal.sca; backtransformed mode from SELECT lognormal
  k1 = NULL,
  #' @field k2 SELECT σ for nomal.loc; SELECT k2 from normal.sca; SELECT lognormal sigma
  k2 = NULL,
  #' @field mesh_proportion proportional fraction of the mesh in question over referenced mesh
  mesh_proportion = NULL,
  #' @field rel_power relative power of each mesh
  rel_power = NULL,
  # @formatter:on
  initialize = function(length_classes, k1, k2, mesh_proportion, rel_power) {
    self$length_classes <- length_classes
    self$k1 <- k1
    self$k2 <- k2
    self$mesh_proportion <- mesh_proportion
    self$rel_power <- rel_power
  },
  # @formatter:off
  #' @description
  #' Returns the numerator and denominator  of the formula.
  #'
  #' @returns a named list with the numerator and denominator of the
  #' @export
  # @formatter:on
  get_formula_terms = function() {
  },
  # @formatter:off
  #' @description
  #' Returns the selectivity curve relative to the fishing power.
  #'
  #' @returns a vector with the selectivity ogive
  #' @export
  # @formatter:on
  run = function() {
    terms <- self$get_formula_terms()
    return(self$rel_power * exp(-terms$num / (2 * terms$denom)))
  }
))

NormalFixSpread <- R6::R6Class("NormalFixSpread", inherit = NormalFamily, public = list( # nolint
  initialize = function(length_classes, k1, k2, mesh_proportion, rel_power) {
    super$initialize(length_classes, k1, k2, mesh_proportion, rel_power)
  },
  #' @description
  #' Returns the numerator and denominator of the selection curve formula.
  #'
  #' @returns a named list with the numerator and denominator of the
  #' @export
  get_formula_terms = function() {
    mode <- self$k1 * self$mesh_proportion
    sigma_square <- self$k2^2
    return(list(
      num = (self$length_classes - mode)^2,
      denom = sigma_square
    ))
  }
))

LogNormalVariableSpread <- R6::R6Class("LogNormalVariableSpread", inherit = NormalFixSpread, public = list( # nolint
  # @formatter:off
  #' @description
  #' Returns the numerator and denominator of the selection curve formula.. If object's k1 and k2 attributes are
  #' in the log scale, the log domain is only applied to the length classes. Otherwise it does it to all varaibles
  #' of the formula.
  #'
  #' @returns a named list with the numerator and denominator of the
  #' @export
  # @formatter:on
  get_formula_terms = function() {
    log_length_clases <- log(self$length_classes)
    log_mode <- log(self$k1 * self$mesh_proportion)
    return(list(
      num = (log_length_clases - log_mode)^2,
      denom = self$k2^2
    ))
  }
))

NormalVariableSpread <- R6::R6Class("NormalVariableSpread", inherit = NormalFixSpread, public = list( # nolint
  #' @description
  #' Returns the numerator and denominator of the selection curve formula.
  #'
  #' @returns a named list with the numerator and denominator of the
  #' @export
  get_formula_terms = function() {
    mode <- self$k1 * self$mesh_proportion
    return(list(
      num = (self$length_classes - mode)^2,
      denom = (self$k2 * self$mesh_proportion)^2
    ))
  }
))
