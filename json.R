## Functions to check if library is installed or not, if installed then it calls library function
## to load the packages
source('load_lib.R')

load_lib(rjson) # read json format
load_lib(dplyr)
load_lib(splitstackshape) # Split a column to multiple columns

## The json file uses too much memory and the RStudio crashes on my local machine, therefore, 
## I am using a script from go language to select only the subset of the data I need.
url <- './golang/Surroundings.json'
surrounding <- fromJSON(file=url)

## From GO, I have selected all store_codes with at least ONE sublist. 
## Absence of sublist in each name under surroundings means that this surrounding is absent 
## around its store_code. 
## Read simplifies surroundigs.json produced in the GO
url <- './golang/surroundings_simplified.json'
surroundings <- fromJSON(file=url)

## Unlist the content and create a dataframe
surroundings <- as.data.frame(unlist(surroundings))
surroundings <- cbind(Row.Names = rownames(surroundings), surroundings)
surroundings <- cSplit(surroundings, "Row.Names", ".")
colnames(surroundings) <- c('sublist_number', 'store_code', 'surroundings')

## Count number of amenities in each surrounding per store_code
surroundings %>%
  group_by(store_code) %>%
  summarise(amenities_no = n()) -> surroundings

## Save the feature extracted from the surroundings.json into the disk
write.csv(surroundings, './new_surroundings.csv', row.names = FALSE)
