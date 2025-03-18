# This script is for setting up the Python environment:
# https://rstudio.github.io/reticulate/articles/package.html

.onLoad <- function(libname, pkgname) {
  bertopicr_env <- bertopic_env_set()
  
  if (!reticulate:::miniconda_exists()) {
    warning("Miniconda is not installed. Install with `reticulate::install_miniconda()` and try again.")
    return(invisible())
  }
  
  conda_envs <- reticulate::conda_list()
  if (!bertopicr_env %in% conda_envs$name) {
    message(paste0("Creating environment ", bertopicr_env))
    
    reticulate::conda_create(
      envname = bertopicr_env,
      python_version = "3.10.16",
      additional_create_args = c("--no-default-packages")
    )
  }
  
  python_path <- reticulate::conda_list()
  python_path <- python_path[python_path["name"] == bertopicr_env, 2]
  Sys.setenv(RETICULATE_PYTHON = python_path)
  reticulate::use_condaenv(condaenv = bertopicr_env, required = TRUE)
  
  if(!check_python_dependencies()) {
    warning("Missing Python dependencies. Run `bertopicr::install_python_dependencies()` to install.")
  }
  
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("BertopicR: Using virtual environment '", bertopic_env_set(), "'")
  invisible()
}

#' Install Python Dependencies
#'
#' @return Nothing
#' @export
#'
install_python_dependencies <- function(){
  bertopicr_env <- bertopic_env_set()

  # First we'll try with the environment.yml, then with the package versions set up
  package_dir <- system.file(package = "BertopicR")

  
  top_level_files <- list.files(package_dir, full.names = TRUE)
  has_environment_yml <- grepl("environment.yml", top_level_files)
  if(any(has_environment_yml)) {
    environment_yml_path <- top_level_files[has_environment_yml]
    
    reticulate::conda_create(
      envname = bertopicr_env,
      file = environment_yml_path
    )
    
    if(check_python_dependencies()){
      return(TRUE)
    }
  }

  #Taken from BERTOPIC setup.py
  #https://github.com/MaartenGr/BERTopic/blob/master/setup.py
  # bertopic_0_15_0_deps <- c("bertopic==0.15.0", "numpy==1.24.3", "hdbscan==0.8.29", "umap-learn==0.5.3", "pandas==2.0.2", "scikit-learn==1.2.2", "pytorch==2.0.0","tqdm==4.65.0", "sentence-transformers==2.2.2","plotly==5.15.0", "openai==0.27.8", "huggingface_hub==0.25.0", "transformers==4.47.0", "scipy==1.11.3")
  
  pip_dependencies = c(
    "torch==2.0.1", 
    "transformers==4.30.2",
    # "transformer-smaller-training-vocab==0.3.2",
    "pytorch-revgrad==0.2.0", 
    "spacy-transformers==1.2.5" )
  
  conda_dependencies = c(
    "bertopic==0.15.0", 
    "numpy==1.24.3", 
    "hdbscan==0.8.29", 
    "umap-learn==0.5.3", 
    "pandas==2.0.2", 
    "scikit-learn==1.2.2", 
    "datasets==2.14.4",
    "tqdm==4.65.0", 
    "pytorch==2.0.0",
    "scipy==1.11.3",
    "sentence-transformers==2.2.2", 
    "huggingface_hub==0.16.4",
    "torchvision==0.15.2", 
    "plotly==5.15.0",
    "openai==0.27.8")
  
  reticulate::py_install(envname = "BertopicR", packages = pip_dependencies, pip = TRUE)

  reticulate::py_install(envname ="BertopicR", packages = conda_dependencies, method = "conda")
}

#' Check that dependencies are loaded
#'
#' @return A message confirming whether or not dependencies are loaded
#' @export
#'
check_python_dependencies <- function(){
  # browser()
  bertopicr_env <- bertopic_env_set()
  installed_packages_auto <- reticulate::py_list_packages(envname = bertopicr_env)
  installed_packages_conda <- reticulate::py_list_packages(envname = bertopicr_env, type = "conda")

  env_packages <- unique(c(installed_packages_auto$package, installed_packages_conda$package))
  if("bertopic" %in% env_packages){
    message("Python package 'bertopic' is installed, setup looks good.")
    
    return(TRUE)
  } else {
    message("bertopic not in installed packages of current environment.\nEither load BertopicR environment or run `bertopicr::install_python_dependencies()`")
    return(FALSE)
  }
}

# get the current python environment
get_current_python_environment <- function() {
  if (Sys.info()["sysname"] == "Windows") {
    reticulate::py_config()$python %>%
      stringr::str_extract(".*(?<=/BertopicR)")
  } else {
    paste0(
      "/",
      reticulate::py_config()$python %>%
        stringr::str_extract("/.*(?<=/bin/python$)") %>%
        stringr::str_remove_all("/bin/python") %>%
        stringr::str_remove("/")
    )
  }
}



import_bertopic <- function(){
  if (!"bertopic" %in% names(reticulate::py)){
    result <- tryCatch({
      reticulate::py_run_string("import bertopic")
    },
    error = function(e) {e}
    )
  }
  if("error" %in% class(result)){
    if(stringr::str_detect(result$message, "No module name")){
      env <- get_current_python_environment()

      stop(paste0("\nMissing Python Library! Run `BertopicR::install_python_dependencies()` before proceeding"))

    }
  }
}

#' Detach bertopic from the python session
#'
#' Call this when you're finished with the topic modelling process. Although, safer may be to simply save your work and then restart your R session, as the Python session is still running (and as far as I know, there's no way to safely close)
#'
#' @return Nothing
#' @export
#'
#' @usage
#' bertopic_detach()
bertopic_detach <- function(){

  #Import sys
  reticulate::py_run_string("import sys")

  #Check if bertopic is inside the Python session and delete it if it is (this follows what spacyr does, not 100% it's good practice.)
  reticulate::py_run_string('if "bertopic" in locals():\n  del bertopic')

  x <- reticulate::py_run_string("locals()")
  py_info <- reticulate::py_to_r(x)
  if(!"bertopic" %in% names(py_info)){
    stop("bertopic was not found in the Python session")
  }

  reticulate::py_run_string("del bertopic")

  y <- reticulate::py_run_string("locals()")
  py_info_updated <- reticulate::py_to_r(y)

  if(!"bertopic" %in% names(py_info)){
    stop("bertopic was detached")
  }

}


bertopic_env_set <- function(){
  # check if the user has already set an environment variable for BERTOPICR_ENV
  bertopicr_env <- Sys.getenv("BERTOPICR_ENV")
  
  if(bertopicr_env == "") {
    bertopicr_env <- "BertopicR"
  } 
  
  return(bertopicr_env)
}
