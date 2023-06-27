#' Function to find terms with the greatest difference between topics
#'
#' @param merged_df output from makedf function.Can be any df that includes a topic column
#' @param text_var text diff_terms to be extracted from
#' @param topic_var column containing topic variable
#' @param top_n number of terms to extract
#' @param min_freq minimum number of times a term should appear for it to be considered
#' @param include_outliers include outlier (-1) bertopic category?
#' @param type lollipop or bar chart
#' @param plots specific plots to output. Should be input as c(x, y) or if you want more than 1
#' chart list(c(x, y), c(u, v)) where x, y, u and v are topic numbers.
#'
#' @return a ggplot object of top different terms between each pair of topics
#' @export
#'
bt_viz_diff_terms <- function(merged_df,
                          text_var = text_clean,
                          topic_var = topic,
                          top_n = 15,
                          min_freq = 25,
                          include_outliers = FALSE,
                          type = c("lollipops", "bars"),
                          plots = NULL){

  text_sym <- rlang::ensym(text_var)
  topic_sym <- rlang::ensym(topic_var)

  clean_df <- merged_df

  if (-1 %in% unlist(plots)){
    include_outliers <- TRUE
  }

  # remove outlier category
  if (include_outliers == FALSE){
    clean_df <- clean_df %>%
      dplyr::filter(!(topic == -1))
  }

  # count words
  words <- clean_df %>%
    tidytext::unnest_tokens(word, !!text_sym) %>%
    dplyr::count(!!topic_sym, word, sort = TRUE)

  # count words per topic
  total_words <- words %>%
    dplyr::group_by(!!topic_sym) %>%
    dplyr::summarize(total = sum(n)) %>%
    dplyr::filter(total >= min_freq) # remove words that don't meet the min freq

  # calculate tf_idf
  topic_tf_idf <- dplyr::left_join(words, total_words, by = dplyr::join_by(!!topic_sym)) %>%
    tidytext::bind_tf_idf(word, !!topic_sym, n)

  # pivot wider
  tf_idf_wide <- topic_tf_idf %>%
    dplyr::select(c(!!topic_sym, word, tf_idf)) %>%
    dplyr::mutate(topic = paste0("topic", !!topic_sym)) %>%
    tidyr::pivot_wider(names_from = topic, values_from = tf_idf)

  if (is.null(plots)){

    # number of topics
    n_topics <- clean_df %>%
      dplyr::pull(topic) %>%
      unique() %>%
      length()

    # number of plots
    n_plots <- choose(n = n_topics, k = 2)
    spread_list <- vector("list", length = n_plots)

    # What plots will there be?
    if (include_outliers){
      plot_combos <- utils::combn(x = -1:(n_topics-2), m = 2) # Each column is a combo
    } else{
      plot_combos <- utils::combn(x = 0:(n_topics-1), m = 2) # Each column is a combo
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

  } else{
    plot_combos <- data.frame()

    for (i in 1:length(plots)){
      plot_combos[1,i] <- plots[[i]][1]
      plot_combos[2,i] <- plots[[i]][2]
    }

    n_topics <- plots %>% unlist() %>% unique() %>% length()
    spread_list <- vector("list", length = length(plots))
    combo_names <- paste0("topic", unlist(plot_combos)) # Each pair is a combo
    my_colours <- viridis::viridis(n_topics) # viridis colour palette

    # Create a mapping between numbers and color codes
    colour_mapping <- stats::setNames(my_colours, unique(as.vector(unlist(plot_combos))))

    # Assign color codes to each number
    combo_colours <- colour_mapping[as.character(as.vector(unlist(plot_combos)))]

    # Empty vector for names
    plot_names <- vector("character", length = length(plots))
  }

  for (i in seq(1, length(combo_names), 2)) {

    plot_df <- tf_idf_wide %>%
      dplyr::select_at(.vars = tidyselect::all_of(c("word",
                                                    combo_names[i],
                                                    combo_names[i + 1]))) %>%
      dplyr::rename("topic_x" = combo_names[i], "topic_y" = combo_names[i + 1]) %>%
      dplyr::mutate(log2_ratio = log2(topic_x / topic_y)) %>%
      dplyr::group_by(direction = log2_ratio > 0) %>%
      dplyr::top_n(top_n, abs(log2_ratio)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(word = stats::reorder(word, log2_ratio))


    if (type == "bars") {
      spread_plot <- plot_df  %>%
        ggplot2::ggplot(ggplot2::aes(x = word, y = log2_ratio, fill = direction)) +
        ggplot2::geom_col(show.legend = TRUE)
    } else {
      spread_plot <- plot_df  %>%
        ggplot2::ggplot(ggplot2::aes(x = word, y = log2_ratio, fill = direction)) +
        ggplot2::geom_segment(ggplot2::aes(x = word, xend = word,
                                           y = 0, yend = log2_ratio,
                                           color = direction),
                              show.legend = FALSE) +
        ggplot2::geom_point(size = 3,
                            stroke = 0,
                            shape = 21,
                            show.legend = TRUE)
    }

    spread_plot <- spread_plot +
      ggplot2::scale_x_discrete("term", labels = scales::wrap_format(15)) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Topic")) +
      ggplot2::scale_y_continuous(paste0("Log2(",
                                         combo_names[i],
                                         " tf_idf/",
                                         combo_names[i + 1],
                                         " tf_idf)")) +
      ggplot2::scale_color_manual(values = c("TRUE" = as.vector(combo_colours[i]),
                                             "FALSE" = as.vector(combo_colours[i + 1])),
                                  guide = "none") +
      ggplot2::scale_fill_manual(values = c("TRUE" = as.vector(combo_colours[i]),
                                            "FALSE" = as.vector(combo_colours[i + 1])),
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
