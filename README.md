
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BertopicR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jpcompartir/BertopicR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jpcompartir/BertopicR/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/jpcompartir/BertopicR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/jpcompartir/BertopicR/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

The goal of BertopicR is to allow R users to access bertopic’s topic
modelling suite in R. The package does not aim to implement every
feature of bertopic, and is designed with specific end users in mind who
may not be experienced programmers or developers. You may submit issues
for feature requests; however, it may be faster to go direct to the
original, Python library which has excellent documentation. \[[BERTopic
documentation](https://maartengr.github.io/BERTopic/index.html)\]

The package currently installs an exact version of bertopic - 0.15.0,
features introduced after this version will take time to, or may never,
reach this package.

## Installation

Before installing bertopic make sure that you have miniconda installed,
if you don’t:

``` r
library(reticulate) #install.packages("reticulate") if you don't have this already or aren't sure how to install.

reticulate::install_miniconda()
```

Once you have reticulate and miniconda installed, you can then install
the development version of BertopicR from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("jpcompartir/BertopicR")

library(BertopicR)

#Check your environment has been loaded correctly and bertopic has been installed:
BertopicR::check_python_dependencies()
```

If you receive the message: “bertopic not in installed packages of
current environment…” run:

``` r
BertopicR::install_python_dependencies()
```

## Quickstart

BertopicR ships with a dataset of unstructured text data
`bert_example_data`

``` r
data <- BertopicR::bert_example_data

embedder <- bt_make_embedder("all-minilm-l6-v2")
embeddings <- bt_do_embedding(embedder, documents = data$message,  batch_size = 16L)
#> 
#> Embedding proccess finished
#> all-minilm-l6-v2 added to embeddings attributes


reducer <- bt_make_reducer(n_neighbors = 10L, n_components = 10L, metric = "cosine")
clusterer <- bt_make_clusterer_hdbscan(min_cluster_size = 20L, metric = "euclidean", cluster_selection_method = "eom", min_samples = 10L)

topic_model <- bt_compile_model(embedding_model = embedder,
                                reduction_model = reducer,
                                clustering_model = clusterer)
#> 
#> No vectorising model provided, creating model with default parameters
#> 
#> No ctfidf model provided, creating model with default parameters
#> 
#> Model built

#Fit the model
fitted_model <- bt_fit_model(model = topic_model, 
                             documents = data$message, 
                             embeddings = embeddings)

fitted_model$get_topic_info() %>%
  dplyr::tibble()
#> # A tibble: 34 × 3
#>    Topic Count Name                                                 
#>    <dbl> <dbl> <chr>                                                
#>  1    -1  1760 -1_music_honor_amazing_spanish                       
#>  2     0   197 0_celebrates_month celebration_october_heritage month
#>  3     1   185 1_heritage night_festival_night_heritage celebration 
#>  4     2   177 2_teachers_grade_students_parents                    
#>  5     3   170 3_white_black_like_really                            
#>  6     4   130 4_world_going_latina_got                             
#>  7     5   125 5_awesome_yes_love_cool                              
#>  8     6   119 6_utsw_hispanics_hispanicheritage_news               
#>  9     7   108 7_latinx_latina_ll_chance                            
#> 10     8    96 8_honored_community_honor_hosting                    
#> # ℹ 24 more rows
```
