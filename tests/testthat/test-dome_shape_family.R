test_that("normal.loc produces correct length retentions mesh 1", {
  data <- get_normal_loc_test_data(data_sample, "norm.loc")
  sel_gen <- NormalFixSpread$new(
    length_classes = data$midpoints,
    k1 = data$k1,
    k2 = data$k2,
    mesh_proportion = data$m1$proportion,
    rel_power = 1
  )
  results <- sel_gen$run()
  expect_equal(results, data$m1$selectivity_curve)
})
