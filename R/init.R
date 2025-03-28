# We need to take care of a few cases: 1. User has not miniconda. 2. User has Miniconda but no environment for BertopicR 3. User has an environment for BertopicR but does not have any packages. 4. User has an environment for BertopicR but does not have up-to-date packages.
# To reduce pain in installation and versions, we export the conda environment using `conda env export > inst/environment.yml` this outputs a file, like requirements.txt or uv.lock/pyproject.toml which we can use as a formula to rebuild the environment. This ships with the package so we can find it with `system.file()`
# The user has to restart their R session at different steps in the process to see the changes. Reticulate does not allow us to load a Python interpreter, and then load another in the same R Session.

bertopic_env_set <- function() {
  # allows the user to set another environment variable
  # e.g. with Sys.setenv("BERTOPICR_ENV" ="TESTENV")
  env_name <- Sys.getenv("BERTOPICR_ENV", unset = "BertopicR")
  return(env_name)
}


.onLoad <- function(libname, pkgname) {
  # managing state for communication between .onLoad, .onAttach and various installation steps
  pkg_env <- new.env(parent = emptyenv())
  
  # flags for state
  pkg_env$restart_required <- FALSE
  pkg_env$env_exists <- FALSE
  pkg_env$deps_installed <- FALSE
  
  
  pkg_env$miniconda_available <- tryCatch({
    dir.exists(reticulate::miniconda_path())
  }, error = function(e) FALSE)
  
  # if (pkg_env$env_exists) {
  #     reticulate::use_condaenv(condaenv = pkg_env)
  # }
  
  assign("pkg_env", pkg_env, envir = asNamespace(pkgname))
  
  invisible()
}


.onAttach <- function(libname, pkgname) {

  # grabbing the state from .onLoad
  pkg_env <- get("pkg_env", envir = asNamespace(pkgname))
  
  bertopicr_env <- bertopic_env_set()
  
  packageStartupMessage("BertopicR is using environment: '", bertopicr_env, "'")
  
  # we need to make sure we have miniconda for things to go smoothly.
  if (!pkg_env$miniconda_available) {
    packageStartupMessage("BertopicR requires miniconda. Install with `reticulate::install_miniconda()` and then restart your R session.")
    return(invisible())
  }
  
  
  conda_envs <- tryCatch({
    reticulate::conda_list()$name
  }, error = function(e) character())
  
  pkg_env$env_exists <- bertopicr_env %in% conda_envs
  
  # --- Interactive Setup Wizard ---
  # Only prompt if:
  # 1. interactive session (just do it if it's in CI/CD)
  # 2. user hasn't made a choice yet,
  # 3. The user hasn't specified a custom environment (BERTOPICR_ENV is unset or "BertopicR").
  # 4. The environment doesn't already exist.
  install_choice_opt <- getOption("BertopicR.install_choice")
  should_prompt <- interactive() && 
    is.null(install_choice_opt) &&
    bertopicr_env == "BertopicR" && 
    !pkg_env$env_exists
  
  if (should_prompt) {
    packageStartupMessage("BertopicR needs a Python environment ('", bertopicr_env, "') with specific dependencies.")
    choice <- utils::menu(
      choices = c("Yes, set it up for me now.",
                  "No, I'll manage the Python environment myself."),
      title = "Do you want BertopicR to try and install the required Python environment and dependencies using Miniconda?"
    )
    
    if (choice == 1) {
      
      packageStartupMessage("Attempting automatic installation...")
      options("BertopicR.install_choice" = TRUE)
      
  
      install_success <- tryCatch({
        install_bertopic() 
        TRUE
      }, error = function(e){
        packageStartupMessage("Automatic installation failed: ", e$message)
        packageStartupMessage("You may need to run `BertopicR::install_python_dependencies()` manually.")
        FALSE
      })
      
      # make sure we check again after we've tried to install.
      if (install_success) {
        conda_envs_after <- tryCatch(reticulate::conda_list()$name, error = function(e) character())
        pkg_env$env_exists <- bertopicr_env %in% conda_envs_after
        
        if (pkg_env$env_exists) {
          pkg_env$deps_installed <- check_python_dependencies(quietly = TRUE)
        }
        
        if (pkg_env$env_exists && pkg_env$deps_installed) {
          packageStartupMessage("Installation successful. Please restart your R session and reload BertopicR.")
        } else {
          packageStartupMessage("Installation may not have completed successfully.")
          packageStartupMessage("Please check conda messages, run `BertopicR::check_python_dependencies()`, ",
                                "and potentially run `BertopicR::install_python_dependencies()` manually.")
        }
      }
      return(invisible())
    } else {
    
      options("BertopicR.install_choice" = FALSE)
      packageStartupMessage("Okay, automatic installation declined.")
      packageStartupMessage("You will need to ensure a Python environment named '", bertopicr_env,
                            "exists and contains the required dependencies. Or set an environment variable to point to your  environment with `Sys.setenv('BERTOPICR_ENV' = 'YOURENV')`")
      packageStartupMessage("See package documentation or run `BertopicR::check_python_dependencies()` for details.")
      return(invisible())
    }
  }
  

  # after everything, did it work? If not suggest manual installation.
  if (!pkg_env$env_exists) {
    if (identical(getOption("BertopicR.install_choice"), FALSE)) {
      packageStartupMessage("BertopicR environment '", bertopicr_env, "' not found. Manual setup selected.")
    } else {
      packageStartupMessage(
        "BertopicR environment '", bertopicr_env, "' not found.",
        "\nPlease create it and install dependencies by running `BertopicR::install_python_dependencies()`",
        "\nThen restart your R session and reload BertopicR."
      )
    }
    return(invisible())
  }
  
  #  now the environment is up (or should be...) check packages are installed.
  pkg_env$deps_installed <- check_python_dependencies(quietly = TRUE)
  
  if (!pkg_env$deps_installed) {
    packageStartupMessage(
      "BertopicR environment '", bertopicr_env, "' exists but required Python packages are missing.",
      "\nInstall with `BertopicR::install_python_dependencies()` and then restart your R session."
    )
    return(invisible())
  }
  
  # --- Environment Exists and Dependencies Look Good: Activate ---
  packageStartupMessage("Found BertopicR environment: '", bertopicr_env, "' with all required packages.")
  tryCatch({
    # check if reticulate is already configured with the correct environment
    current_py <- Sys.getenv("RETICULATE_PYTHON")
    target_py_prefix <- try(reticulate::conda_python(envname = bertopicr_env), silent = TRUE)
    
    is_correct_env <- !inherits(current_py, "try-error") &&
      !inherits(target_py_prefix, "try-error") &&
      startsWith(current_py, target_py_prefix)
    
    if (is_correct_env) {
      packageStartupMessage("Python environment '", bertopicr_env, "' already active.")
    } else {
      packageStartupMessage("Attempting to activate Python environment '", bertopicr_env, "'...")
      reticulate::use_condaenv(condaenv = bertopicr_env, required = TRUE)
      packageStartupMessage("Successfully activated Python environment '", bertopicr_env, "'.")
      
     # can we load bertopic? If not, we have some issues.
      if (!reticulate::py_module_available("bertopic")) {
        warning("Activated environment '", bertopicr_env,"' but 'bertopic' module still not available. Check installation.")
      }
    }
  }, error = function(e) {
    packageStartupMessage("\nError activating Python environment '", bertopicr_env, "':")
    packageStartupMessage(e$message)
    packageStartupMessage("\nPlease ensure the environment is correctly installed and functional.")
    packageStartupMessage("You might need to run `BertopicR::install_python_dependencies()` and restart R.")
  })
  
  invisible()
}

#' Install BertopicR's Python Environment and Dependencies
#'
#' This function ensures Miniconda is available and then sets up the necessary
#' conda environment using the package's `environment.yml` file.
#'
#' @details
#' After running this function, you **must** restart your R session for the
#' changes to take effect.
#'
#' @export
install_bertopic <- function() {
  pkg_env <- get("pkg_env", envir = asNamespace("BertopicR"))
  
  # step 1: ensure Miniconda is installed
  if (!pkg_env$miniconda_available) {
    message("Miniconda not found. Attempting to install Miniconda...")
    tryCatch({
      reticulate::install_miniconda()
      # update the flag so next steps know
      pkg_env$miniconda_available <- dir.exists(reticulate::miniconda_path())
      message("Miniconda installation successful.")
    }, error = function(e) {
      stop("Miniconda installation failed: ", e$message,
           "\nPlease install Miniconda manually and then re-run `install_bertopic()`.", call. = FALSE)
    })
  } else {
    message("Miniconda installation found.")
  }
  
  # step 2: install/Uudate the conda environment
  options("BertopicR.install_choice" = TRUE)
  install_python_dependencies()
  
  invisible()
}

#' Install or Update Python Dependencies from YAML
#'
#' Creates the BertopicR conda environment (or the one specified by
#' `BERTOPICR_ENV`) if it doesn't exist, or updates it if it does,
#' using the `environment.yml` file included with the package.
#'
#' @param quietly Logical. If `TRUE`, suppresses informational messages. Defaults to `FALSE`.
#' @return Invisibly returns `TRUE` on successful installation, `FALSE` otherwise.
#' @export
install_python_dependencies <- function(quietly = FALSE) {
  pkg_env <- get("pkg_env", envir = asNamespace("BertopicR"))
  bertopicr_env <- bertopic_env_set()
  
  if (!quietly) message("Preparing to install/update Python dependencies for environment: '", bertopicr_env, "'...")
  
  # if making big changes to the package, you need to update the environment.yml file *and* push that change to the package GitHub repo
  # `conda env export > inst/environment.yml`
  package_dir <- system.file(package = "BertopicR")
  environment_yml_path <- file.path(package_dir, "environment.yml")
  
  if (!file.exists(environment_yml_path)) {
    stop("Critical Error: 'environment.yml' not found within the BertopicR package installation.",
         "\nPlease reinstall the BertopicR package.", call. = FALSE)
  }
  
  if (!quietly) message("Using environment definition: ", environment_yml_path)
  
  conda_envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character())
  env_exists_before <- bertopicr_env %in% conda_envs # tracks whether we have the environment or not (do we update or create?)
  
  # flag for tracking conda installation progress
  operation_success <- FALSE
  
  # 
  if (!env_exists_before) {
    
    if (!quietly) message("Creating conda environment '", bertopicr_env, "' from YAML file...")
    if (!quietly) message("This may take several minutes...")
    
    tryCatch({
      conda <- reticulate::conda_binary()
      yml_path <- normalizePath(environment_yml_path)
      cmd <- sprintf('"%s" env create -n %s -f "%s"', conda, bertopicr_env, yml_path)
      
      if (!quietly) message("Running command: ", cmd)
      
      
      status <- system(cmd)
      
      if (status == 0) {
        if (!quietly) message("Environment '", bertopicr_env, "' created successfully.")
        operation_success <- TRUE
      } else {
        if (!quietly) message("Conda returned non-zero exit status: ", status)
        operation_success <- FALSE
      }
    }, error = function(e) {
      message("Error creating conda environment '", bertopicr_env, "':")
      message(e$message)
      message("Please check your Conda installation and network connection.")
      operation_success <- FALSE
    })
  } else {
    
    if (!quietly) message("Updating conda environment '", bertopicr_env, "' from YAML file...")
    if (!quietly) message("This may take several minutes...")
    
    tryCatch({
      # Use direct conda command for better reliability
      conda <- reticulate::conda_binary()
      yml_path <- normalizePath(environment_yml_path)
      cmd <- sprintf('"%s" env update -n %s -f "%s"', conda, bertopicr_env, yml_path)
      
      if (!quietly) message("Running command: ", cmd)
      
      # Execute conda command
      status <- system(cmd)
      
      if (status == 0) {
        if (!quietly) message("Environment '", bertopicr_env, "' updated successfully.")
        operation_success <- TRUE
      } else {
        if (!quietly) message("Conda returned non-zero exit status: ", status)
        operation_success <- FALSE
      }
    }, error = function(e) {
      message("Error updating conda environment '", bertopicr_env, "':")
      message(e$message)
      message("Consider removing the environment manually and re-running.")
      operation_success <- FALSE
    })
  }
  
  # --- Final Check and Status Update ---
  final_check_passed <- FALSE
  
  if (operation_success) {
    if (!quietly) message("Verifying dependency installation...")
    final_check_passed <- check_python_dependencies(quietly = TRUE)
    
    if (final_check_passed) {
      pkg_env$env_exists <- TRUE
      pkg_env$deps_installed <- TRUE
      pkg_env$restart_required <- TRUE
      
      if (!quietly) {
        packageStartupMessage(
          cli::rule(left = cli::style_bold("Installation Complete"), col = "green")
        )
        packageStartupMessage(
          cli::col_green(cli::symbol$tick), " Dependencies successfully installed in environment '", bertopicr_env, "'."
        )
        packageStartupMessage(
          cli::style_bold(cli::col_yellow("IMPORTANT: Please restart your R session now and call `library(BertopicR)`"))
        )
        packageStartupMessage(
          cli::rule(col = "green")
        )
      }
    } else {
      pkg_env$deps_installed <- FALSE
      
      if (!quietly) {
        packageStartupMessage(
          cli::col_red(cli::symbol$cross), " Installation process finished, but dependency check failed."
        )
        # show which of the core packages are missing
        check_python_dependencies(quietly = FALSE)
        packageStartupMessage("Please review any errors above and try troubleshooting your conda setup.")
      }
    }
  } else {
    pkg_env$deps_installed <- FALSE
    
    if (!quietly) {
      packageStartupMessage(
        cli::col_red(cli::symbol$cross), " Failed to create or update the conda environment '", bertopicr_env, "'."
      )
      packageStartupMessage("Please check the error messages above.")
    }
  }
  
  invisible(final_check_passed)
}

#' Check Python Dependencies
#'
#' Verifies if the target conda environment exists and contains the essential
#' Python packages required by BertopicR.
#'
#' @param quietly Logical. If `TRUE`, suppresses messages. If `FALSE` (default), prints informative messages.
#' @return Logical. `TRUE` if the environment exists and all dependencies are found, `FALSE` otherwise.
#' @export
check_python_dependencies <- function(quietly = FALSE) {
  bertopicr_env <- bertopic_env_set()
  
  if (!quietly) message("Checking dependencies for environment: '", bertopicr_env, "'")
  
  # Check 1: does environment exist?
  conda_envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character())
  
  if (!bertopicr_env %in% conda_envs) {
    if (!quietly) {
      message(cli::col_red(cli::symbol$cross), " Environment '", bertopicr_env, "' not found.")
      message("  Run `BertopicR::install_python_dependencies()` to create it.")
    }
    return(FALSE)
  }
  
  # Check 2: Are core packages available?
  required_packages <- c(
    "bertopic",
    "numpy",
    "pandas",
    "scikit-learn", 
    "pytorch",
    "sentence-transformers",
    "hdbscan",
    "umap-learn"
  )
  
  if (!quietly) message("Checking for required packages in environment '", bertopicr_env, "'...")
  
  installed_packages <- tryCatch({
    # get all installed packages in env - auto and conda
    pkgs_df <- reticulate::py_list_packages(envname = bertopicr_env)
    pkgs_conda_df <- reticulate::py_list_packages(envname = bertopicr_env, type="conda")
    all_packages <- unique(tolower(c(pkgs_df$package, pkgs_conda_df$package)))
    
    if (!quietly) message("Found ", length(all_packages), " installed packages.")
    all_packages
  }, error = function(e) {
    if (!quietly) message("Warning: Could not list packages in environment '", bertopicr_env, "': ", e$message)
    character()
  })
  
  # Some packages, have aliases, check them individually
  if ("sklearn" %in% installed_packages) installed_packages <- c(installed_packages, "scikit-learn")
  if ("umap" %in% installed_packages) installed_packages <- c(installed_packages, "umap-learn")
  if (!"pytorch" %in% installed_packages && "torch" %in% installed_packages) installed_packages <- c(installed_packages, "torch")
  
  missing_packages <- required_packages[!tolower(required_packages) %in% installed_packages]
  
  if (length(missing_packages) == 0) {
    if (!quietly) {
      message(cli::col_green(cli::symbol$tick), " All essential Python dependencies found in '", bertopicr_env, "'.")
    }
    return(TRUE)
  } else {
    if (!quietly) {
      message(cli::col_red(cli::symbol$cross), " Missing essential Python dependencies in '", bertopicr_env, "':")
      message("  ", paste(missing_packages, collapse = ", "))
      message("  Run `BertopicR::install_python_dependencies()` to install or update them.")
    }
    return(FALSE)
  }
}

#' Import the main Bertopic Python module
#'
#' @return The imported 'bertopic' Python module.
#' @export
import_bertopic <- function() {
  # Ensure dependencies are available
  pkg_env <- get("pkg_env", envir = asNamespace("BertopicR"))
  
  if (!pkg_env$deps_installed) {
    if (!check_python_dependencies(quietly = FALSE)) {
      stop("Essential Python dependencies are missing or the environment isn't active.\n",
           "Please run `BertopicR::install_python_dependencies()`, restart R, and reload BertopicR.",
           call. = FALSE)
    }
  }
  
  # Check if module is available in active Python session
  if (!reticulate::py_module_available("bertopic")) {
    bertopicr_env <- bertopic_env_set()
    current_py_config <- reticulate::py_config()
    
    stop(
      "The 'bertopic' Python module is not available in the current Python session.\n",
      "Expected environment: '", bertopicr_env, "'\n",
      "Currently active Python: ", current_py_config$python, "\n",
      "Please ensure the correct environment ('", bertopicr_env, "') is activated.\n",
      "You may need to restart R after installation or run:\n",
      "`reticulate::use_condaenv('", bertopicr_env, "', required = TRUE)`",
      call. = FALSE
    )
  }
  
  # Import the module
  reticulate::import("bertopic", delay_load = TRUE)
}

#' Detach Bertopic from the Python session (if loaded)
#'
#' @export
bertopic_detach <- function() {
  if (reticulate::py_module_available("bertopic")) {
    main_vars <- reticulate::py_list_attributes(reticulate::py_main_environment())
    
    if ("bertopic" %in% main_vars) {
      tryCatch({
        reticulate::py_run_string('del bertopic', local = FALSE)
      }, error = function(e) {
        warning("Could not unload 'bertopic' module: ", e$message)
      })
    }
  }
  
  invisible()
}