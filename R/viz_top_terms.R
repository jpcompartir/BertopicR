#' #' create top terms charts for topic modelling completed using bertopic
#' #'
#' #' @param merged_df df output from makedf function.Can be any df that includes a topic column
#' #' @param text_var text top terms to be extracted from
#' #' @param topic_var column containing topic variable
#' #' @param top_n number of terms to extract
#' #' @param n_row number of rows the plots should take up
#' #' @param min_freq minimum number of times a term should appear for it to be considered
#' #' @param include_outliers include the -1 (outlier) bertopic category
#' #' @param type lollipop or bar chart?
#' #' @return list of all_terms and max_only top term bar charts
#' #' @export
#' #'
#' bt_viz_top_terms <- function(merged_df,
#'                           text_var = text_clean,
#'                           topic_var = topic,
#'                           top_n = 15,
#'                           n_row = 2,
#'                           min_freq = 25,
#'                           include_outliers = FALSE,
#'                           type = c("lollipops", "bars")){
#' 
#'   text_sym <- rlang::ensym(text_var)
#'   topic_sym <- rlang::ensym(topic_var)
#' 
#'   clean_df  <- merged_df
#' 
#'   # include outliers
#'   if (include_outliers == FALSE){
#'     clean_df <- clean_df %>%
#'       dplyr::filter(!(!!topic_sym == -1))
#'   }
#' 
#'   # count words
#'   words <- clean_df %>%
#'     tidytext::unnest_tokens(word, !!text_sym) %>%
#'     dplyr::count(!!topic_sym, word, sort = TRUE)
#' 
#'   # count words per topic
#'   total_words <- words %>%
#'     dplyr::group_by(!!topic_sym) %>%
#'     dplyr::summarize(total = sum(n))
#' 
#'   # Calculate tf_idf score
#'   topic_tf_idf <- dplyr::left_join(words, total_words, by = dplyr::join_by(!!topic_sym)) %>%
#'     tidytext::bind_tf_idf(word, !!topic_sym, n)
#' 
#'   # Which terms have highest tf-idf within each topic?
#'   top_terms <- topic_tf_idf %>%
#'     dplyr::filter(n > min_freq) %>% # remove words that don't meet the min freq
#'     dplyr::group_by(topic) %>%
#'     dplyr::slice_max(n = top_n, order_by = tf_idf, with_ties = FALSE) %>%
#'     dplyr::arrange(topic, -tf_idf)
#' 
#'   # All terms by topic (will duplicate common terms)
#'   all_terms <- top_terms %>%
#'     dplyr::mutate(word = stringr::str_wrap(word),
#'                   word = tidytext::reorder_within(word, tf_idf, topic)) %>%
#'     ggplot2::ggplot(ggplot2::aes(word, tf_idf, fill = factor(topic),
#'                                  colour = factor(topic)))
#' 
#' 
#'   if (type == "bars") {
#'     all_terms <- all_terms + ggplot2::geom_col(show.legend = FALSE)
#'   } else {
#'     all_terms <- all_terms +
#'       ggplot2::geom_segment(ggplot2::aes(x = word, xend = word,
#'                                          y = 0, yend = tf_idf),
#'                             show.legend = FALSE) +
#'       ggplot2::geom_point(size = 3,
#'                           shape = 21,
#'                           show.legend = FALSE)
#'   }
#' 
#'   most_likely_topics <- topic_tf_idf %>%
#'     dplyr::filter(tf_idf == max(tf_idf), .by = word) %>% #group_by term and ungroup
#'     dplyr::slice_max(n = top_n, order_by = tf_idf, by = !!topic_sym, with_ties = FALSE) %>% #group_by topic and ungroup
#'     dplyr::arrange(!!topic_sym, -tf_idf) %>%
#'     dplyr::mutate(word = tidytext::reorder_within(word, tf_idf, !!topic_sym)) %>%
#'     ggplot2::ggplot(ggplot2::aes(word, tf_idf, fill = factor(!!topic_sym),
#'                                  colour = factor(!!topic_sym)))
#' 
#'   if (type == "bars") {
#'     most_likely_topics <- most_likely_topics + ggplot2::geom_col(show.legend = FALSE)
#'   } else {
#'     most_likely_topics <- most_likely_topics +
#'       ggplot2::geom_segment(ggplot2::aes(x = word, xend = word,
#'                                          y = 0, yend = tf_idf),
#'                             show.legend = FALSE) +
#'       ggplot2::geom_point(size = 3,
#'                           shape = 21,
#'                           show.legend = FALSE)
#'   }
#' 
#'   plot_list <- list(
#'     "all_terms" = all_terms,
#'     "max_only" = most_likely_topics)
#' 
#'   #Add the ggplot boilerplate code to both plot types
#'   viridis_cols <- clean_df %>% dplyr::select(!!topic_sym) %>% unique() %>% dplyr::count()
#'   plot_list <- purrr::map(plot_list, ~ .x +
#'                             ggplot2::facet_wrap(ggplot2::vars(!!topic_sym),
#'                                                 scales = "free",
#'                                                 labeller = ggplot2::label_both,
#'                                                 nrow = n_row) +
#'                             ggplot2::coord_flip() +
#'                             tidytext::scale_x_reordered("Term") +
#'                             ggplot2::scale_y_continuous("Probability term belongs to topic (\u03b2)") +
#'                             ggplot2::scale_fill_manual(values = viridis::viridis(viridis_cols)) +
#'                             ggplot2::scale_colour_manual(values = viridis::viridis(viridis_cols)) +
#'                             ggplot2::theme_minimal() +
#'                             ggplot2::theme(title = ggplot2::element_text(size = 16),
#'                                            text = ggplot2::element_text(size = 10),
#'                                            strip.text = ggplot2::element_text(size = 14),
#'                                            axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
#'                                            axis.title.x = ggplot2::element_text(size = 14))
#'   )
#' 
#'   return(plot_list)
#' }
