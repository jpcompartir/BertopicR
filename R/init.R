bertopic_env_set <- function() {
  return(Sys.getenv("BERTOPICR_ENV", unset = "BertopicR"))
}

.onLoad <- function(libname, pkgname) {
  pkg_env <- new.env(parent = emptyenv())
  pkg_env$restart_required <- FALSE
  pkg_env$env_exists <- FALSE
  pkg_env$deps_installed <- FALSE
  assign("pkg_env", pkg_env, envir = asNamespace(pkgname))
  
  pkg_env$miniconda_available <- tryCatch({
    dir.exists(reticulate::miniconda_path())
  }, error = function(e) FALSE)
  
  invisible()
}

.onAttach <- function(libname, pkgname) {
  pkg_env <- get("pkg_env", envir = asNamespace(pkgname))
  bertopicr_env <- bertopic_env_set()
  
  if (!pkg_env$miniconda_available) {
    packageStartupMessage("BertopicR requires miniconda. Install with `reticulate::install_miniconda()` and then restart your R session.")
    return(invisible())
  }

  
  install_choice <- getOption("BertopicR.install_choice", default = FALSE)
  
  conda_envs <- tryCatch({
    reticulate::conda_list()
  }, error = function(e) data.frame(name = character()))
  
  pkg_env$env_exists <- bertopicr_env %in% conda_envs$name
  
  # Only prompt if:
  # 1. We're in an interactive session.
  # 2. The user hasn't already made a choice (install_choice is FALSE).
  # 3. The user hasn't specified a custom environment (BERTOPICR_ENV is unset or "BertopicR").
  # 4. The environment doesn't already exist.
  if (interactive() && !install_choice &&
      Sys.getenv("BERTOPICR_ENV", unset = "BertopicR") == "BertopicR" && !pkg_env$env_exists) {
    
    choice <- utils::menu(
      choices = c("Yes, install the environment and dependencies.", "No, I'll handle it myself."),
      title = "BertopicR needs a Python environment. Can we set it up for you?"
    )
    
    if (choice == 1) {
      options("BertopicR.install_choice" = TRUE)  # if user accepts, make sure this persists 
      install_bertopic()
      return(invisible()) 
    } else {
      options("BertopicR.install_choice" = FALSE) # if user declines, make sure their choice persists
      packageStartupMessage("Okay, you'll need to set up the environment manually. See the package documentation for details.")
      return(invisible())
    }
  }
  
  
  if (!pkg_env$env_exists) {
    packageStartupMessage(
      "BertopicR environment not found. Create with `BertopicR::install_python_dependencies()` and then restart your R session."
    )
    return(invisible())
  }
  
  pkg_env$deps_installed <- tryCatch({
    check_python_dependencies(quietly = TRUE)
  }, error = function(e) FALSE)
  
  if (!pkg_env$deps_installed) {
    packageStartupMessage(
      "BertopicR environment exists but required Python packages are missing. Install with `BertopicR::install_python_dependencies()` and then restart your R session."
    )
    return(invisible())
  }
  
  tryCatch({
    reticulate::use_condaenv(condaenv = bertopicr_env, required = FALSE)
    packageStartupMessage("BertopicR: Using virtual environment '", bertopicr_env, "'")
  }, error = function(e) {
    packageStartupMessage("BertopicR: Error loading environment: ", e$message)
  })
  
  invisible()
}

#' Install BertopicR's Python Environment and Dependencies
#'
#' This function installs Miniconda and creates a Conda
#' environment named "BertopicR" (or the value of the `BERTOPICR_ENV`
#' environment variable if user has set it/installed individually), and installs the required Python packages.
#'
#' @export
install_bertopic <- function() {
  if (!dir.exists(reticulate::miniconda_path())) {
    reticulate::install_miniconda()
  }
  options("BertopicR.install_choice" = TRUE) # set even if miniconda was just installed 
  install_python_dependencies()
}

#' Install Python Dependencies
#'
#' @param quietly Whether to suppress function's own warnings
#' @return Nothing
#' @export
install_python_dependencies <- function(quietly = TRUE) {
  message("Installing Python dependencies for BertopicR, this may take several minutes...")
  bertopicr_env <- bertopic_env_set()
  
  # get what we can from environment.yml
  package_dir <- system.file(package = "BertopicR")
  environment_yml_path <- file.path(package_dir, "environment.yml")
  
  if (file.exists(environment_yml_path)) {
    message("Installing dependencies from `environment.yml`...")
    result <- tryCatch({
      reticulate::conda_create(
        envname = bertopicr_env,
        file = environment_yml_path
      )
      TRUE
    }, error = function(e) {
      message("Error installing from environment.yml: ", e$message)
      FALSE
    })
    
    if (result && check_python_dependencies(quietly = TRUE)) {
      packageStartupMessage(
        "Dependencies installed successfully. Restart your R session and call `library(BertopicR)` to get started."
      )
      return(invisible())
    }
  }
  
  # it still looks bad to me to get torch from pip and a separte version from conda...
  pip_dependencies = c(
    "torch==2.0.1",
    "transformers==4.30.2",
    #"transformer-smaller-training-vocab==0.3.2",
    "pytorch-revgrad==0.2.0",
    "spacy-transformers==1.2.5"
  )
  
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
    "openai==0.27.8"
  )
  
  conda_envs <- tryCatch({
    reticulate::conda_list()
  }, error = function(e) data.frame(name = character()))
  
  if (!bertopicr_env %in% conda_envs$name) {
    message("Creating conda environment: ", bertopicr_env)
    reticulate::conda_create(bertopicr_env, python_version = "3.10")
  }
  
  message("Installing pip dependencies...")
  tryCatch({
    reticulate::py_install(
      envname = bertopicr_env,
      packages = pip_dependencies,
      pip = TRUE,
      pip_args = "--quiet"
    )
  }, error = function(e) {
    message(e)
  })
  
  message("Installing conda dependencies...")
  tryCatch({
    reticulate::py_install(
      envname = bertopicr_env,
      packages = conda_dependencies,
      method = "conda",
      conda_args = "--quiet"
    )
  }, error = function(e) {
    message(e)
  })
  
  if (check_python_dependencies(quietly = TRUE)) {
    packageStartupMessage(
      "✓ Dependencies successfully installed! Restart your R session and call `library(BertopicR)` to get started."
    )
    
  } else {
    packageStartupMessage("× Installation completed but some dependencies may be missing.")
  }
  invisible()
}

#' Check that dependencies are loaded
#'
#' @param quietly Whether to suppress function's own warnings
#' @return A message confirming whether or not dependencies are loaded
#' @export
check_python_dependencies <- function(quietly = FALSE) {
  bertopicr_env <- bertopic_env_set()
  
  conda_envs <- tryCatch({
    reticulate::conda_list()
  }, error = function(e) data.frame(name = character()))
  
  if (!bertopicr_env %in% conda_envs$name) {
    if (!quietly) {
      message("BertopicR environment '", bertopicr_env, "' not found.")
    }
    return(FALSE)
  }
  
  # py_list_packages() returns different packages for auto vs conda, so we need to check both, as that's what the environment has access to. Checking for one not the other will lead us to the wrong idea about what's installed/available in the environment.
  installed_packages_auto <- tryCatch({
    reticulate::py_list_packages(envname = bertopicr_env)
  }, error = function(e) {
    data.frame(package = character())
  })
  installed_packages_conda <- tryCatch({
    reticulate::py_list_packages(envname = bertopicr_env, type = "conda")
  }, error = function(e) {
    data.frame(package = character())
  })
  
  env_packages <- unique(c(
    installed_packages_auto$package, installed_packages_conda$package
  ))
  
  
  required_packages <- c(
    "bertopic", "numpy", "hdbscan", "umap-learn", "pandas",
    "scikit-learn", "torch", "tqdm", "sentence-transformers"
  )
  # finer-grained way to reort what packages are missing.
  missing_packages <- required_packages[!required_packages %in% env_packages]
  
  if (length(missing_packages) == 0) {
    if (!quietly) {
      message("All Python dependencies are installed, setup looks good.")
    }
    return(TRUE)
  } else {
    if (!quietly) {
      message(
        "Missing Python dependencies: ", paste(missing_packages, collapse = ", "),
        "\nRun `BertopicR::install_python_dependencies()` to install all required packages."
      )
    }
    return(FALSE)
  }
}


get_current_python_environment <- function() {
  tryCatch({
    reticulate::conda_list() |> 
      dplyr::filter(name == bertopic_env_set())  |> 
      dplyr::pull(python) |> 
      dirname()
  }, error = function(e) {
    NULL
  })
}


import_bertopic <- function() {
  if (!check_python_dependencies(quietly = TRUE)) {
    stop("Python dependencies are not installed. Run `BertopicR::install_python_dependencies()`.")
  }
  
  if (!reticulate::py_module_available("bertopic")) {
    stop(
      "The 'bertopic' Python module is not available.  Ensure the BertopicR environment is activated."
    )
  }
  reticulate::import("bertopic", delay_load = TRUE)
}

#' Detach bertopic from the python session
#'
#' @export
bertopic_detach <- function() {
  if (reticulate::py_module_available("bertopic")) {
    reticulate::py_run_string('
try:
    del bertopic
except:
    pass
')
  }
}