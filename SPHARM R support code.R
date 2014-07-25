## Source file to batch convert ASCII ply files to ASCII stl files
# And to write coordinate data to SPHARM's .landmarkAscii file
# SPHARM (http://www.enallagma.com/SPHARM.php) is a software application to 
# perform 3-dimensional spherical harmonic analyses of triangular mesh surfaces.
# (Shen et al. 2009 Evolution) 

# Emma Sherratt July 2014 emma.sherratt@gmail.com
install.packages(geomorph)
library(geomorph)

# Batch converts and ASCII PLY mesh file to an ASCII STL mesh file using RGL's library function writeSTL
# e.g. usage: PLY2STL(list.files(pattern = "\\.ply$")) # converts all PLY files to STL files

PLY2STL <- function(filelist){
for (i in 1:length(filelist)){
  myply <- read.ply(filelist[i])
  name <- gsub(".ply", "", filelist[i], ignore.case = T)
  name <- paste(name,".stl", sep="")
  writeSTL(name, ascii=TRUE)
  }
}

# Writes a .landmarkAscii file of digitized coordinate data
# coords is a 3D array of coordinate data
# e.g. coords <- readmulti.nts(list.files(pattern = "\\.nts$")) # coordinate data saved in .nts files
# writeland.ascii(coords)
writeland.ascii <- function(coords){
  names <- dimnames(coords)[[3]]
  header <- paste("# This is a header, edit for your use", "\n", "\n", 
                  "define Markers 7","\n", "\n",
                  "Parameters {", "\n",
                  "    NumSets 1,", "\n",
                  "    ContentType ", '"LandmarkSet"', "\n",
                  "}", "\n", "\n",
                  "Markers { float[3] Coordinates } @1", "\n", "\n",
                  "# Data section follows", "\n",
                  "@1")
  for (i in 1:dim(coords)[3]){
    file = paste(names[i], ".landmarkAscii", sep="")
    write(header, file, append = TRUE)
    write.table(coords[,,i], file, append = TRUE, sep = " ", col.names= FALSE, row.names = FALSE)
  }  
}



