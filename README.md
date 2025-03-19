
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BertopicR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jpcompartir/BertopicR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jpcompartir/BertopicR/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/jpcompartir/BertopicR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/jpcompartir/BertopicR/actions/workflows/pkgdown.yaml)

<!-- badges: end -->

# Quickstart

Assuming everything is installed ([click here if not](#installation)),
you can jump in and start using the package like so

    #> BertopicR: Using virtual environment 'BertopicR'

BertopicR ships with a dataset of unstructured text data
`bert_example_data`

``` r
data <- BertopicR::bert_example_data

embedder <- bt_make_embedder_st("all-minilm-l6-v2")
embeddings <- bt_do_embedding(embedder, documents = data$message,  batch_size = 16L)
#> 
#> Embedding proccess finished
#> all-minilm-l6-v2 added to embeddings attributes

reducer <- bt_make_reducer_umap(n_neighbours = 10L, n_components = 10L, metric = "cosine")
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

# fits the model, changing it in-place
bt_fit_model(model = topic_model, 
             documents = data$message, 
             embeddings = embeddings)
#> UMAP(angular_rp_forest=True, low_memory=False, metric='cosine', min_dist=0.0, n_components=10, n_neighbors=10, random_state=42, verbose=True)
#> Wed Mar 19 13:36:10 2025 Construct fuzzy simplicial set
#> Wed Mar 19 13:36:14 2025 Finding Nearest Neighbors
#> Wed Mar 19 13:36:16 2025 Finished Nearest Neighbor Search
#> Wed Mar 19 13:36:17 2025 Construct embedding
#> Wed Mar 19 13:36:20 2025 Finished embedding
#> 
#> The input model has been fitted to the data and updated accordingly

topic_model$get_topic_info() %>%
  dplyr::tibble()
#> # A tibble: 33 × 5
#>    Topic Count Name                           Representation Representative_Docs
#>    <dbl> <dbl> <chr>                          <list>         <list>             
#>  1    -1  1698 -1_hispanicheritagemonth_hisp… <chr [10]>     <chr [3]>          
#>  2     0   278 0_heritage month_month_latino… <chr [10]>     <chr [3]>          
#>  3     1   234 1_hhm_mytimenow_andresdavid_k… <chr [10]>     <chr [3]>          
#>  4     2   214 2_heritage night_night_herita… <chr [10]>     <chr [3]>          
#>  5     3   140 3_students_school_elementary_… <chr [10]>     <chr [3]>          
#>  6     4   137 4_beto_rourke_fake_warren      <chr [10]>     <chr [3]>          
#>  7     5   122 5_latinx_latina_uber_latinas   <chr [10]>     <chr [3]>          
#>  8     6   121 6_hispanicheritagemonth hispa… <chr [10]>     <chr [3]>          
#>  9     7    99 7_mdcps_mdcpscentral_celebrat… <chr [10]>     <chr [3]>          
#> 10     8    97 8_celebrating hispanicheritag… <chr [10]>     <chr [3]>          
#> # ℹ 23 more rows
```

# Context

The goal of {BertopicR} is to allow R users to access `bertopic`’s topic
modelling suite in R via Reticulate.

BertopicR relies heavily on the Python `bertopic` (written and developed
by Maarten Grootendorst (and other contributors!). package and its
dependencies. BertopicR manages its own Conda environment for Python
dependencies. This avoids conflicts with other Python packages you might
have installed and ensures that the correct versions of all required
libraries are available.

The package does not aim to implement every feature of `bertopic`, and
is currently frozen at `bertopic==0.15.0`. The package was created for a
specific set of non-technical users in mind who are not experienced
programmers or developers.

You may submit issues for feature requests; however, it may be faster to
go direct to the original, Python library which has excellent
documentation. \[[BERTopic
documentation](https://maartengr.github.io/BERTopic/index.html)\]

We have tried to stay true to the Python library, whilst developing a
package which has an ‘R feel’ i.e. uses functions more than OOP. In
places we have changed the names of arguments, for example in the Python
library BERTopic() takes `hdbscan_model = x`, but we have opted for
`clustering_model = x`. The reason for this is that `hdbscan_model =` is
an artifact from the early days of bertopic and the author wants to
ensure code is backwards compatible, but if the package were being
developed now it’s likely the author would opt for `clustering_model =`.
There are other such changes to be aware of.

# Installation

1.  **Install `BertopicR`:** Install the R package from GitHub (code
    below).
2.  **Load `BertopicR`:** Call `library(BertopicR)`. This triggers the
    setup wizard.
3.  **Follow Prompts:** If prompted, choose to let BertopicR install the
    environment. If you decline, you’ll need to manually install
    Miniconda and the Python dependencies.
4.  **Restart R:** After the installation (either automatic or manual),
    *restart your R session*. This is crucial for the changes to take
    effect.
5.  **Verify Installation:** After restarting, load `BertopicR` again.
    You should see a message indicating that the BertopicR environment
    is being used. You can also run
    `BertopicR:::check_python_dependencies()` to confirm.
6.  **Use BertopicR:** You’re now ready to use the package!

> The setup wizard will try to assist you in setting up your
> environment. If you have already set up an environment you wish to
> use, let the package know with
> \`Sys.setenv(“BERTOPICR_ENV”=“<your-env-name-here>”)\`

Code for step 1:

``` r
devtools::install_github("jpcompartir/BertopicR")
```

Step 2 - Follow the setup wizard to configure and install the Python
environment. It will instruct you to install Miniconda if it cannot
detect your installation. After each step you will need to restart your
R session and load the package again.

You only need to do these steps the first time you install the package.=

``` r
library(BertopicR)
```

### I Have Already Setup my Environment

If you’ve already setup your Python environment, make sure to set an
environment variable so the setup wizard knows where to find it with:
`Sys.setenv("BERTOPICR_ENV"="<your-env-name-here>")`

## Error Messages and causes

We have tried to provide informative error messages but sometimes you
may be faced with error messages that have arisen from the parent Python
function used. If you get an error that begins with “Error in
py_call_impl(callable, call_args\$unnamed, call_args\$named) :”, you can
use reticulate::py_last_error() to trace the origin of the error
message.

If your console starts being spammed by a message similar to:
\>uggingface/tokenizers: The current process just got forked, after
parallelism has already been used. Disabling parallelism to avoid
deadlocks… To disable this warning, you can either: - Avoid using
`tokenizers` before the fork if possible - Explicitly set the
environment variable TOKENIZERS_PARALLELISM=(true \| false)

You can ignore it with: `Sys.setenv("TOKENIZERS_PARALLELISM"="0")`

Note that one common cause of error messages when working with Python
functions in R arises when the user fails to specify integer numbers as
integer type. In R, integers are of type “numeric” by default, Python
functions typically require them to be explicitly specified as integer
type. For any formal function arguments, this conversion is dealt with
within in the function, however you should be aware of this if
specifying extra arguments to any function. In R, this is achieved by
placing an L after the number or using the as.integer() function:

``` r
# numbers default to "numeric"
class(4)
#> [1] "numeric"

# we can force them to be "integer"
class(4L)
#> [1] "integer"
class(as.integer(4))
#> [1] "integer"
```
