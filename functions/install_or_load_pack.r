#function to install or load packages
# install_or_load_pack <- function(pack){
#   create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
#   if (length(create.pkg))
#     install.packages(create.pkg, dependencies = TRUE,repos = "http://cran.us.r-project.org")
#   sapply(pack, require, character.only = TRUE)
# }

install_or_load_pack <- function(packages) {
  for (pkg in packages) {
    # install only if missing
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = NA, repos = "https://cloud.r-project.org")
    }
    # always load
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }
  invisible(NULL)
}
