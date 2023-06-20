test_that("bt_base_embedder returns the right object", {

  expect_silent(bt_base_embedder())
  embedder <- bt_base_embedder()
  expect_true(grepl("BaseEmbedder", class(embedder)[[1]]))
  expect_true(all("embed" %in% names(embedder)))

})

test_that("bt_base_reducer returns the right object", {
  expect_silent(bt_base_reducer())
  reducer <- bt_base_reducer()
  expect_true(grepl("BaseDimension", reducer))
})


test_that("bt_base_clusterer returns the right object", {
  expect_silent(bt_base_clusterer())
  clusterer <- bt_base_clusterer()
  expect_true(grepl("BaseCluster", clusterer))
})

#Misread the docs, don't need this

# test_that("bt_base_ctfidf returns the right object", {
#   expect_silent(bt_base_ctfidf())
#   ctfidf <- bt_base_ctfidf()
#   expect_true(grepl("bertopic.*ClassTf", class(ctfidf)[[1]]))
#
#
# })


