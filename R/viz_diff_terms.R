viz_diff_terms <- function(merged_df,
                          text_var = text_clean,
                          topic_var = topic,
                          stopwords = TRUE,
                          hashtags = TRUE,
                          mentions = TRUE,
                          top_n = 15,
                          n_row = 2,
                          min_freq = 25,
                          include_outliers = FALSE,
                          type = c("lollipips", "bars")){
  
  text_sym <- rlang::ensym(text_var)
  topic_sym <- rlang::ensym(topic_var)
  
  clean_df <- merged_df
  
  # remove stopwords
  if (stopwords){
    clean_df <- merged_df %>%
      dplyr::mutate(!!text_sym := tm::removeWords(!!text_sym, SegmentR::stopwords$stopwords))
  }
  
  # remove hashtags
  if (hashtags){
    clean_df <- clean_df %>%
      dplyr::mutate(!!text_sym := stringr::str_remove_all(!!text_sym, "#\\S+")) # remove hashtags
  }
  
  # remove mentions
  if (mentions){
    clean_df <- clean_df %>%
      LimpiaR::limpiar_tags(text_var = !!text_sym, hashtag = T, user = F) %>%  # remove mention
      dplyr::mutate(!!text_sym := stringr::str_remove_all(!!text_sym, "@user")) # remove mentions
  }
  
  # remove outlier category
  if (include_outliers == FALSE){
    clean_df <- clean_df %>%
      filter(!(!!topic_sym == -1))
  }
  
  # count words
  words <- clean_df %>%
    tidytext::unnest_tokens(word, !!text_sym) %>%
    dplyr::count(!!topic_sym, word, sort = TRUE)
  
  total_words <- words %>% 
    dplyr::group_by(!!topic_sym) %>% 
    dplyr::summarize(total = sum(n))
  
  topic_words <- dplyr::left_join(words, total_words, by = join_by(!!topic_sym))
  
  topic_tf_idf <- topic_words %>%
    tidytext::bind_tf_idf(word, !!topic_sym, n)
  
  tf_idf_wide <- topic_tf_idf %>%
    select(c(topic, word, tf_idf)) %>%
    dplyr::mutate(topic = paste0("topic", !!topic_sym)) %>%
    tidyr::pivot_wider(names_from = !!topic_sym, values_from = tf_idf)
  
  # number of topics
  n_topics <- clean_df %>% 
    dplyr::pull(!!topic_sym) %>% 
    unique() %>% 
    length()
  
  # number of plots
  n_plots <- choose(n = n_topics, k = 2) 
  spread_list <- vector("list", length = n_plots)
  
  # What plots will there be? 
  if (include_outliers){
    plot_combos <- combn(x = -1:(n_topics-2), m = 2) # Each column is a combo
  } else{
    plot_combos <- combn(x = 0:(n_topics-1), m = 2) # Each column is a combo
  }
  
  combo_names <- paste0("topic", plot_combos) # Each pair is a combo
  
  # Assign colour for each topic
  my_colours <- viridis::viridis(n_topics)
  
  if (include_outliers){
    combo_colours <- my_colours[c(plot_combos + 2)]
  } else{
    combo_colours <- my_colours[c(plot_combos + 1)]
  }
  
  
  
  # Empty vector for names
  plot_names <- vector("character", length = n_plots)
  
  for (i in seq(1, length(combo_names), 2)) {
    
    plot_df <- tf_idf_wide %>%
      dplyr::select_at(.vars = tidyselect::all_of(c("word",
                                                    combo_names[i],
                                                    combo_names[i + 1]))) %>%
      dplyr::rename("topic_x" = combo_names[i], "topic_y" = combo_names[i + 1])
    
    
    # if (type == "bars") {
    #   most_likely_topics <- most_likely_topics + ggplot2::geom_col(show.legend = FALSE)
    # } else {
    #   most_likely_topics <- most_likely_topics +
    #     ggplot2::geom_segment(ggplot2::aes(x = word, xend = word,
    #                                        y = 0, yend = tf_idf),
    #                           show.legend = FALSE) +
    #     ggplot2::geom_point(size = 3,
    #                         shape = 21,
    #                         show.legend = FALSE)
    # }
    
    spread_plot <- plot_df %>%
      dplyr::mutate(log2_ratio = log2(topic_x / topic_y)) %>%
      dplyr::group_by(direction = log2_ratio > 0) %>%
      dplyr::top_n(top_n, abs(log2_ratio)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(word = stats::reorder(word, log2_ratio)) %>%
      ggplot2::ggplot(ggplot2::aes(x = word, y = log2_ratio, fill = direction)) +
      ggplot2::geom_col() +
      ggplot2::scale_x_discrete("word", labels = scales::wrap_format(15)) +
      ggplot2::scale_y_continuous(paste0("Log2(",
                                         combo_names[i],
                                         "tf_idf",
                                         combo_names[i + 1],
                                         "tf_idf)")) +
      ggplot2::scale_fill_manual("Topic",
                                 values = c("TRUE" = combo_colours[i],
                                            "FALSE" = combo_colours[i + 1]),
                                 labels = c("TRUE" = combo_names[i],
                                            "FALSE" = combo_names[i + 1])) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::theme(title = ggplot2::element_text(size = 16),
                     text = ggplot2::element_text(size = 14),
                     axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5))
    
    spread_list[[(i + 1)/2]] <- spread_plot
    
    plot_names[(i + 1)/2] <- paste0(combo_names[i], 
                                    "_vs_",
                                    combo_names[i + 1])
    
  }
  
  # Add names
  names(spread_list) <- plot_names
  
  # Output
  return(spread_list)
  
}