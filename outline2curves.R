#' Subdivide an outline into open curves
#'
#' A function to take an outline (open or closed) and a set of landmarks (3 or more) along that outline, and to subdivide the outline into
#'  curves between the landmarks. Each of these curves can then be sampled with semilandmarks using \code{\link{digit.curves}}. If the 
#'  outline is open, landmarks at either end are required.
#'
#' 'outline' is a p-x-k matrix of 2D or 3D coordinates for a set of ordered points defining an open or closed outline. This can be the 
#' pixels of an outline calculated in ImageJ (save xy coordinates) or any other reasonable way of obtaining ordered coordinates along 
#' an outline.
#'
#' @param outline A p-x-k matrix of 2D or 3D coordinates for a set of ordered points defining an outline
#' @param landmarks A p-x-k matrix of 2D or 3D coordinates for the landmarks along the outline
#' @param closed Logical Whether the outline is closed (TRUE) or open (FALSE)
#' @param plot Logical Whether to plot the curves (TRUE) or not (FALSE)
#' @return Function returns a list, where each item in the list is a p-x-k matrix of 2D or 3D coordinates for the ordered points defining one segment (curve) between two landmarks
#' @seealso \code{\link{digit.curves}}
#' @export
#' @keywords digitizing, utilities
#' @author Emma Sherratt

outline2curves <- function(outline, landmarks, closed=TRUE, plot=TRUE){
  checkmat <- is.matrix(outline)
  if (checkmat==FALSE) {stop("Input must be a p-x-k matrix of outline coordinates")}
  p = dim(landmarks)[1]
  if(p<3){stop("There must be at least three landmarks along the outline")}
  checkdim <- dim(outline)[2]
  nOutPoints = nrow(outline)
  nodes <- NULL
  if (checkdim==2) { 
    for(i in 1:p){
      nodes <- c(nodes, which.min(sqrt((landmarks[i,1]-outline[,1])^2+
                                       (landmarks[i,2]-outline[,2])^2))) }  }
  if (checkdim==3) {  
  for(i in 1:p){
    nodes <- c(nodes, which.min(sqrt((landmarks[i,1]-outline[,1])^2+
                                     (landmarks[i,2]-outline[,2])^2+
                                     (landmarks[i,2]-outline[,2])^2))) }  }
  nodes <- sort(nodes)
  if(closed==TRUE){curves <- vector("list", p)
    for(i in 1:(p-1)){
      curves[[i]] <- outline[c(nodes[i]:nodes[i+1]),] }  
      curves[[p]] <- rbind(outline[nodes[p]:nOutPoints,],
                           outline[1:nodes[1],]) 
  }
  if(closed==FALSE){curves <- vector("list", p-1)
    for(i in 1:(p-1)){
      curves[[i]] <- outline[c(nodes[i]:nodes[i+1]),] }  
  }
  if (checkdim==2 && plot==TRUE) { 
    plot(outline, asp=TRUE, pch=19, cex=0.1)
    cols <- rainbow(length(curves))
    for(i in 1:length(curves)){points(curves[[i]], pch=19, col=cols[i])}
    points(landmarks, pch=19, col="black", cex=1)
    legend(0, legend=c(1:length(curves)), pch=19, col=cols)
  }
  if (checkdim==3&& plot==TRUE) { 
    plot3d(outline, asp=TRUE, pch=19, cex=0.1)
    cols <- rainbow(length(curves))
    for(i in 1:length(curves)){points3d(curves[[i]], pch=19, col=cols[i])}
    points3d(landmarks, pch=19, col="black", cex=1)
    legend3d(0, legend=c(1:length(curves)), pch=19, col=cols)
  } # 3D plotting not fully checked yet
  return(curves)
}