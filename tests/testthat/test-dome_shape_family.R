test_that("normal.loc produces correct length retentions mesh 1", {
  data <- get_normal_loc_test_data(data_sample, "norm.loc")
  sel_gen <- NormalFixSpread$new(
    length_classes = data$midpoints,
    k1 = data$k1,
    k2 = data$k2,
    mesh_proportion = data$m1$proportion,
    rel_power = data$m1$rel_power
  )
  results <- sel_gen$run()
  expect_equal(results, data$m1$selectivity_curve)
})

test_that("normal.loc produces correct length retentions mesh 2", {
  data <- get_normal_loc_test_data(data_sample, "norm.loc")
  sel_gen <- NormalFixSpread$new(
    length_classes = data$midpoints,
    k1 = data$k1,
    k2 = data$k2,
    mesh_proportion = data$m2$proportion,
    rel_power = data$m2$rel_power
  )
  results <- sel_gen$run()
  expect_equal(results, data$m2$selectivity_curve)
})

test_that("normal.sca produces correct length retentions mesh 1", {
  data <- get_normal_loc_test_data(data_sample, "norm.sca")
  sel_gen <- NormalVariableSpread$new(
    length_classes = data$midpoints,
    k1 = data$k1,
    k2 = data$k2,
    mesh_proportion = data$m1$proportion,
    rel_power = data$m1$rel_power
  )
  results <- sel_gen$run()
  expect_equal(results, data$m1$selectivity_curve)
})

test_that("normal.sca produces correct length retentions mesh 2", {
  data <- get_normal_loc_test_data(data_sample, "norm.sca")
  sel_gen <- NormalVariableSpread$new(
    length_classes = data$midpoints,
    k1 = data$k1,
    k2 = data$k2,
    mesh_proportion = data$m2$proportion,
    rel_power = data$m2$rel_power
  )
  results <- sel_gen$run()
  expect_equal(results, data$m2$selectivity_curve)
})

test_that("lognorm produces correct length retentions mesh 1", {
  data <- get_normal_loc_test_data(data_sample, "lognorm")
  # Lognorm implementation expects bot backtransformed mode and sd
  util_class <- MixinUtilities$new()
  back_mode <- util_class$back_transformed_mode(data$k1, data$k2)
  back_sd <- util_class$back_transformed_sd(data$k1, data$k2)

  sel_gen <- LogNormalFixedSpread$new(
    length_classes = data$midpoints,
    k1 = back_mode,
    k2 = back_sd,
    mesh_proportion = data$m1$proportion,
    rel_power = data$m1$rel_power
  )
  results <- sel_gen$run()

  # Generate expected results by using the exact formula of LBSPR DOME-SHAPE
  sl_mesh <- data$m1$proportion
  rel_power <- data$m2$rel_power
  expected_resuls <- rel_power * exp(-0.5 * ((log(data$midpoints) - log((back_mode) * sl_mesh)) / (data$k2))^2)

  expect_equal(results, expected_resuls)
})

test_that("lognorm produces correct length retentions mesh 1", {
  data <- get_normal_loc_test_data(data_sample, "lognorm")
  # Lognorm implementation expects bot backtransformed mode and sd
  util_class <- MixinUtilities$new()
  back_mode <- util_class$back_transformed_mode(data$k1, data$k2)
  back_sd <- util_class$back_transformed_sd(data$k1, data$k2)

  sel_gen <- LogNormalFixedSpread$new(
    length_classes = data$midpoints,
    k1 = back_mode,
    k2 = back_sd,
    mesh_proportion = data$m2$proportion,
    rel_power = data$m2$rel_power
  )
  results <- sel_gen$run()

  # Generate expected results by using the exact formula of LBSPR DOME-SHAPE
  sl_mesh <- data$m1$proportion
  rel_power <- data$m2$rel_power
  expected_resuls <- rel_power * exp(-0.5 * ((log(data$midpoints) - log((back_mode) * sl_mesh)) / (data$k2))^2)

  expect_equal(results, expected_resuls)
})
