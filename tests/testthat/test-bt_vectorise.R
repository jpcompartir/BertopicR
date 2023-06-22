test_that("bt_make_vectoriser tests", {

  #Passes until stopifnot added
  expect_error(bt_make_vectoriser(ngram_range = c(1, 2, 3)))

  #Cannot input strings
  expect_error(bt_make_vectoriser(ngram_range = c("1", "2")))




})

