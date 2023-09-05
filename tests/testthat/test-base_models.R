test_that("bt_empty_embedder returns the right object", {

  expect_silent(bt_empty_embedder())
  embedder <- bt_empty_embedder()
  expect_true(grepl("emptyEmbedder", class(embedder)[[1]]))
  expect_true(all("embed" %in% names(embedder)))

})

test_that("bt_empty_reducer returns the right object", {
  expect_silent(bt_empty_reducer())
  reducer <- bt_empty_reducer()
  expect_true(grepl("emptyDimension", reducer))
})


test_that("bt_empty_clusterer returns the right object", {
  expect_silent(bt_empty_clusterer())
  clusterer <- bt_empty_clusterer()
  expect_true(grepl("emptyCluster", clusterer))
})

#Misread the docs, don't need this

# test_that("bt_empty_ctfidf returns the right object", {
#   expect_silent(bt_empty_ctfidf())
#   ctfidf <- bt_empty_ctfidf()
#   expect_true(grepl("bertopic.*ClassTf", class(ctfidf)[[1]]))
#
#
# })


