#Testing this is actually pretty sophisticated, so first cover the named arguments, then ellipsis separately, then an output.

test_that("bt_reducer stops when it's given bad arguments",{

  #Make an empty array to feed into the mandatory argument (which should be array or df)
  array <- array()
  expect_error(bt_reducer(embeddings = array, return_value = "reduces"), "should be one of")
  expect_error(bt_reducer(embeddings = array, n_neighbors = "15"), "is.numeric.*n_neighb")
  expect_error(bt_reducer(embeddings = array, n_components = TRUE), "is.numeric.*n_components")
  expect_error(bt_reducer(embeddings = array, min_dist = "12"))
  expect_error(bt_reducer(embeddings = array, random_state = "42"), "is.numeric.*random_state")
  expect_error(bt_reducer(embeddings = array, metric = 2), "is.character.*metric" )
})

test_that("bt_reducer accepts an array or a data frame as embeddings", {

})

test_that("bt_reducer's return_value argument is working as expected", {

})

#How to test for side effects, e.g. test for the printing of the progress bar and other text when bt_reducer is called?
