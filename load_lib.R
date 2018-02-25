load_lib <- function(name_lib) {
  name_lib <- toString(substitute(name_lib))
  if (!require(name_lib, character.only = TRUE)) 
    install.packages(name_lib); 
  base::library(name_lib, character.only = TRUE)
}