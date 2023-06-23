test_that("bt_make_ctfidf function is working as intended", {

  expect_error(bt_make_ctfidf(bm25_weighting = "hello", reduce_frequent_words = "goodbye"))

  expect_error(bt_make_ctfidf(bm25_weighting = TRUE, reduce_frequent_words = "goodbye"))

  #Function's default args are booleans and function runs with them not set
  expect_silent(bt_make_ctfidf())

  vectoriser <- bt_make_ctfidf()
  expect_false(vectoriser$bm25_weighting)

  vectoriser <- bt_make_ctfidf(bm25_weighting = TRUE)
  expect_true(vectoriser$bm25_weighting)

  #Now make test below pass:
  vectoriser <- bt_make_ctfidf(reduce_frequent_words = FALSE)

  expect_false(vectoriser$reduce_frequent_words)

  expect_true(any(grepl("ClassTfid", S3Class(vectoriser))))
})
