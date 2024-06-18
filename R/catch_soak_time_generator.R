CatchBySoakTimeGenerator <- R6::R6Class("CatchBySoakTimeGenerator", public = list(

  # @formatter:off
  #' @description
  #' Initialise the class.
  #' @param catch_wide a dataframe with the catch data in wide format - timestep as columns
  #' @param gears a dataframe holding the information area ration between gears
  #'
  #' @export
  # @formatter:on
  catch_wide = NULL,
  gears = NULL,
  initialize = function(catch_wide, gears) {

  }

))