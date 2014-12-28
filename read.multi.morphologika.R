# Source file to use with library(geomorph)
# Emma Sherratt December 2014

# Code reads multiple morphologika files that each contain only a single specimen (such as can be exported from
# Stratovan Checkpoint software https://www.stratovan.com/products/checkpoint

# To use, filelist <- list.files(pattern = "*.txt") # list all morpholgika files

read.multi.morphologika <- function(filelist){
  names <- gsub (".txt", "", filelist) # extracts names of specimens from the file name
  coords <- NULL # make empty object that will be filled with 3D array of coordinate data
  k <- dim(read.morphologika(filelist[1]))[1]
  for (i in 1:length(filelist)){
    temp <- read.morphologika(filelist[i])
    coords <- rbind(coords, two.d.array(temp)) }
  raw.Y <- arrayspecs(coords, k, 3)
  dimnames(raw.Y)[[3]] <- names
  return(raw.Y)
}
