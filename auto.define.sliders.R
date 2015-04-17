#' Define points to "slide" along two- or three-dimensional curves.
#'
#' A function to automatically define which landmarks will "slide" along two- or three-dimensional curves.
#'
#'  Function takes a sequence of landmark numbers to be treated as "curve sliders" in Generalized Procrustes analysis
#'  \code{\link{gpagen}}. This type of semilandmark "slides" along curves lacking known landmarks 
#'  (see Bookstein 1997 for algorithm details). 
#'  Each sliding semilandmark ("sliders") will slide between two designated points, along a line 
#'  tangent to the specified curvature. For the parameter 'sliders', the user must provide a vector of numbers corresponding to
#'  the curve sliding semilandmarks in the order they appear on the specimen. 
#'  This can be made by c() or seq() or any other reasonable method.
#'  
#'  If closed = TRUE, then the function assumes that the first and last landmarks in the sliders list are adjacent on the specimen.
#   If closed = FALSE, the the user is expected to have included the fixed landmarks at the start and end of the curve in 'sliders'
#'  e.g. if landmark 1 and 5 are fixed landmarks, and 2, 3 and 4 are semilandmarks, then sliders = c(1:5), and closed=FALSE.
#'  
#' @param sliders A vector containing a sequence of numbers correspnding to the landmarks in the order they appear along the curve
#' @param closed A logical value indicating whether the curve is closed (TRUE) or open (FALSE).
#' @param write.file A logical value indicating whether the matrix is written to file as .csv.
#' @return Function returns a 'nsliders-x-3' matrix containing the landmark address of the curve sliders, indicating the 
#' landmarks between which the slider landmarks will "slide". The matrix is also written to working directory
#' as "curveslide.csv". Matrix (or "curveslide.csv") is designed for use by \code{\link{gpagen}} during GPA.
#' @export
#' @keywords utilities
#' @seealso  \code{\link{gpagen}} \code{\link{digit.curves}}
#' @author Emma Sherratt
#' @references Bookstein, F. J. 1997 Landmark Methods for Forms without Landmarks: Morphometrics of 
#' Group Differences in Outline Shape. Medical Image Analysis 1(3):225-243.
#' 
#' @example
#' ## data(scallops) ## This dataset has a curveslide matrix, which we can create by:
#' 
#' sliders <- c(5, 6:16, 1) # landmarks 5 and 1 are fixed landmarks, and 6:16 are semis around the shell
#' auto.define.sliders(sliders, closed=FALSE, write.file = FALSE)

auto.define.sliders <- function(sliders, closed =FALSE, write.file = TRUE){
  if(closed == TRUE){ sliders <- c(sliders, sliders[1]) }
  nsliders <- length(sliders)
  CV <- matrix(NA, ncol=3, nrow=nsliders-2)
  for (i in 1:(nsliders-2)){
    CV[i,] <- sliders[1:3]
    sliders <- sliders[-1] }
  colnames(CV)<-c("before","slide","after")
  if(write.file == TRUE){write.table(CV,file="curveslide.csv",row.names=FALSE,col.names=TRUE,sep=",")}
  return(CV)
}