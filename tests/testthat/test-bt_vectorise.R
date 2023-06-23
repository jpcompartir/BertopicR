test_that("bt_make_vectoriser tests", {

  #Passes until stopifnot added
  expect_error(bt_make_vectoriser(ngram_range = c(1, 2, 3)))

  #Cannot input strings
  expect_error(bt_make_vectoriser(ngram_range = c("1", "2")))

#stop_words must be a string
expect_error(
  bt_make_vectoriser(
    ngram_range = c(1, 2),
    stop_words = 1
    )
)
#Accepts a string
expect_silent(
  bt_make_vectoriser(
    ngram_range = c(1, 2),
    stop_words = "english"
  )
)

expect_error(
  bt_make_vectoriser(
    ngram_range = c(1, 2),
    stop_words = "english",
    min_frequency = "1"
  )
)

#Default arguments don't raise an error
expect_silent(
  bt_make_vectoriser()
)

vectoriser <- bt_make_vectoriser(
  ngram_range = c(1, 2),
  stop_words = "french",
  min_frequency = 5L
  )

#args are having an effect
expect_true(all(vectoriser$ngram_range[[1]] == 1, vectoriser$ngram_range[[2]] == 2))

#The languae for stopwords has been changed
expect_equal(vectoriser$stop_words, "french")

#min_frequency argument is altering bertopic's min_df (changed name as confusing and we use min_frequency a lot)
expect_equal(vectoriser$min_df, 5L)

#Count vectorisers is in the vectoriser's classes
expect_true(any(grepl( "CountVec", class(vectoriser))))

})
