#function to install or load packages
install_or_load_pack <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE,repos = "http://cran.us.r-project.org")
  sapply(pack, require, character.only = TRUE)
}