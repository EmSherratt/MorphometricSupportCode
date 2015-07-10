## Read a .nas (nastran) file and convert into a mesh3d file for plotting with rgl package

## NASTRAN is a finite element analysis (FEA) program that was originally developed for NASA in the late 1960s under United States government funding for the Aerospace industry

# Emma Sherratt July 2015 emma.sherratt@gmail.com
library(rgl)

read.nastran <- function (file){ 
  nasfile <- scan(file = file, what = "char", sep = "\n", strip.white = TRUE, quiet = TRUE, comment.char="$")
  GRIDline <- grep("GRID*", nasfile, value = FALSE)
  nnodes <- length(GRIDline)
  vertices <- matrix(unlist(strsplit(nasfile[GRIDline[1] : (GRIDline[nnodes]+1)], split = "\\s+")), ncol=6, byrow=T)
  vertices <- matrix(as.numeric(vertices[,c(3,4,6)]), ncol=3, byrow=F)
  # If it's a Quad Mesh
  CTETRAline <- grep("CTETRA", nasfile, value = TRUE)
  if(length(CTETRAline)!=0){ paste("File is quad mesh")
    nbricks <- length(CTETRAline)
    indices <- matrix(unlist(strsplit(grep("CTETRA", nasfile, value = TRUE), 
                                      split = "\\s+")), ncol=7, byrow=T)
    indices <- matrix(as.numeric(indices[,c(4:7)]), ncol=4, byrow=F)
    mesh <- qmesh3d(rbind(t(vertices), 1), indices)
    }
  # If it's a Tri Mesh
  CTRIA3line <- grep("CTRIA3", nasfile, value = TRUE)
  if(length(CTRIA3line)!=0){ paste("File is triangular mesh")
    ntri <- length(CTRIA3line)
    indices <- matrix(unlist(strsplit(grep("CTRIA3", nasfile, value = TRUE), 
                                      split = "\\s+")), ncol=6, byrow=T)
    indices <- matrix(as.numeric(indices[,c(4:6)]), ncol=3, byrow=F)
    mesh <- tmesh3d(rbind(t(vertices), 1), indices)
  }
  return(mesh)
}