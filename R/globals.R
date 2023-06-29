global_vars <- c( "py", "created_time", "document", "text_clean", "topic", "word",
                  "n", "total", "tf_idf", "topic_x", "topic_y", "log2_ratio", "direction",
                  "topic", "model","Y" )

utils::globalVariables(unique(global_vars))
