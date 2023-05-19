# This script is for setting up the Python environment:
# https://rstudio.github.io/reticulate/articles/package.html

.onAttach <- function(libname, pkgname) {

  bertopicr_env <- Sys.getenv("BERTOPICR_ENV")

  if(bertopicr_env == "") {
    bertopicr_env <- "BertopicR"
  }

  #Try to use miniconda to
  result <- tryCatch(
    {reticulate::use_miniconda(bertopicr_env, required = TRUE)},
    error = function(e) {e}
  )
  if ("error" %in% class(result)) {
    if(stringr::str_detect(result$message, "Minicoda is not installed")){
      stop(
        paste0( result$message, "\nInstall Miniconda with `reticulate::install_miniconda()` and try again."))
    }
    if(stringr::str_detect(result$message, "Unable to locate conda environment")){
      packageStartupMessage(paste0("\nCreating environment, ", bertopicr_env))

      reticulate::conda_create(
        envname = bertopicr_env,
        conda = paste0(reticulate::miniconda_path(), "/condabin/conda")
      )
      packageStartupMessage(paste0("\nSuccessfully created environment ", bertopicr_env))
    }
    if(!("bertopic" %in% reticulate::py_list_packages(unique(bertopicr_env$package)))){
      warning("Missing Python dependencies. Run `bertopicr::install_python_dependencies()` to install.")
    }
  }

  #Get correct Python path
  python_path <- reticulate::conda_list() %>%
    dplyr::filter(name == bertopicr_env) %>%
    dplyr::pull(python)

  #Set correct Python path for reticulate
  Sys.setenv(RETICULATE_PYTHON = python_path)

  #Load the conda env
  reticulate::use_condaenv(condaenv = bertopicr_env, required = TRUE)

  invisible()
}


#' Install Python Dependencies
#'
#' @return Nothing
#' @export
#'
install_python_dependencies <- function(){
  bertopicr_env <- Sys.getenv("BERTOPICR_ENV")
  if(bertopicr_env == "") {
    bertopicr_env <- "BertopicR"
  }

  #Taken from BERTOPIC setup.py
  #https://github.com/MaartenGr/BERTopic/blob/master/setup.py
  bertopic_0_14_1_deps <- c("bertopic==0.14.1", "numpy>=1.20.0", "hdbscan>=0.8.29", "umap-learn>=0.5.0", "pandas>=1.1.5", "scikit-learn>=0.22.2.post1", "tqdm>=4.41.1", "sentence-transformers>=0.4.1","plotly>=4.7.0")

  reticulate::conda_install(
    bertopicr_env,
    packages = bertopic_0_14_1_deps
  )
}
