#' Plot multiple tangent spaces
#'
#' Function plots a grid of all pairwise PC axes, from a principal components analysis of Procrustes-aligned specimens.
#'
#' The function performs a principal components analysis of shape variation and then plots a grid of all pairwise comparisons of two 
#'  dimensions of tangent space for a set of Procrustes-aligned specimens. If groups are provided, specimens from 
#'  each group are plotted using distinct colors based on the order in which the groups are found in the dataset, 
#'  and using R's standard color palette: black, red, green, blue, cyan, magenta, yellow, and gray. NOTE: to change
#'  the colors of the groups, simply substitute a vector of the desired colors for each specimen (see example below).
#'
#' @param A An array (p x k x n) containing landmark coordinates for a set of aligned specimens 
#' @param PCs How many principal axes to be plotted (2 or more)
#' @param groups An optional factor vector specifying group identity for each specimen
#' @param xlabels Whether the X-axis labels should be plotted at the top or bottom of the plot
#' @export
#' @keywords visualization
#' @author Emma Sherratt (emma.sherratt@gmail.com)
#' @examples
#' library(geomorph)
#' data(plethodon)
#' Y.gpa<-gpagen(plethodon$land) 
#' gp <- as.factor(paste(plethodon$species, plethodon$site)) # group must be a factor
#' plotMultiTangentSpace(Y.gpa$coords, PCs=4, groups=gp)

plotMultiTangentSpace <- function(A, PCs, groups=NULL, xlabels = c("top", "bottom")){
  if(PCs == 1){ stop("PCs must at least 2.")}
  if (length(dim(A)) != 3) {
    stop("Data matrix not a 3D array (see 'arrayspecs').") }
  if(any(is.na(A))==T){
    stop("Data matrix contains missing values. Estimate these first (see 'estimate.missing').") }
  pcdata <- prcomp(two.d.array(A))$x
  ## multiple PCA plot
  N = PCs # number of PC axes to plot
  M = N-1
  P = (N^2 - N)/2 # number of plots overall
  mat <- matrix(0, ncol=N, nrow=N)
  mat[which(lower.tri(mat)==TRUE)] <- 1:P
  mat <- mat[-1,-N] # knock off last column & first row
  if(xlabels == "top"){
    mat <- rbind((P+N): ((P+N) +(M-1)),mat) # sets first row for PC label
    mat <- cbind(c(0,(P+1):(P+M)), mat) # adds column for PC labels
    layout(mat, widths=c(0.2, rep(1, M)), heights=c(0.2, rep(1, M))) # sets up the multi plot
  }
  if(xlabels == "bottom"){
    mat <- rbind(mat, (P+N): ((P+N) +(M-1))) # sets last row for PC label
    mat <- cbind(c((P+1):(P+M),0), mat) # adds column for PC labels
    layout(mat, widths=c(0.2, rep(1, M)), heights=c(rep(1,M),0.2)) # sets up the multi plot
  }
  par(mar=c(2,2,1,2)) # to set up margins 
  # start PC plots
  for (i in 1:M){
    for (j in (i+1):N){
      if(is.null(groups))plot(pcdata[,i], pcdata[,j], pch=21, cex=2, xlab = "", ylab = "", asp=TRUE)
      if(!is.null(groups))plot(pcdata[,i], pcdata[,j], pch=21, cex=2, bg=groups, xlab = "", ylab = "", asp=TRUE)
      } }
  labels = c(paste("PC", 2:N) , paste("PC", 1:M))
  # Place PC labels for y-axis
  par(mar=c(0,0,0,0)) #reset margins
  for(i in 1:(length(labels)/2)) { plot(1, type="n", axes=FALSE, xlab="", ylab="", xlim=c(-1,1), ylim=c(-1,1))
    text(x=0,y=0, labels = labels[i], cex=2, srt=90) }
  # Place PC labels for x-axis
  for(i in (((length(labels)/2)+1): length(labels))) { plot(1, type="n", axes=FALSE, xlab="", ylab="", xlim=c(-1,1), ylim=c(-1,1))
    text(x=0,y=0, labels = labels[i], cex=2) }
  layout(1)
}




