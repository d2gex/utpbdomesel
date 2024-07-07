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
