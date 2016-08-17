#' @importFrom magrittr "%>%"
NULL
#' Define a plane given slope, rotation, and intercept
#'
#' This is a special-purpose utility function for translating "user friendly"
#' input values from Shiny app into a rotation+translation matrix. The function
#' accounts for the ideosyncratic definition of slope (which is defined relative
#' to a 2D plane where x=z and y=y in atlas space), rotation (which is
#' specifically around the Inferior-Superior axis), and intercept (which is
#' where the plane crosses the Anterior-Posterior axis when x=z=0) when creating
#' this matrix.
#'
#' The function returns a list containing the normal and origin of the plane as
#' 3D vectors.
#'
#' @param slope Slope parameter from the Shiny interface
#' @param rotation Rotation parameter from the Shiny interface (degrees)
#' @param intercept Intercept parameter from the Shiny interface (point along AP axis)
#' @return Atlas with roi_boundary_{x,y,z} columns defining the plane.
#' @examples
#' \dontrun{
#' input <- list(slope=1,rotation=45, intercept=10)
#' get_plane(input$slope, input$rotation, input$intercept)
#' }
get_plane <- function(slope,rotation,intercept) {
  theta_x <- atan(-slope)
  R_x <- as.matrix(rbind(
    c(1,0,0),
    c(0,cos(theta_x),-sin(theta_x)),
    c(0,sin(theta_x), cos(theta_x))
  ))
  theta_z <- (rotation / 180) * pi
  R_z <- as.matrix(rbind(
    c(cos(theta_z),-sin(theta_z),0),
    c(sin(theta_z), cos(theta_z),0),
    c(0,0,1)
  ))
  R_xz <- R_z %*% R_x
  nv <- (R_xz %*% matrix(c(0,1,0),nrow=3))
  return(list(normal=nv, origin=c(0,intercept,0)))
}

#' Compute point on plane, given two other points on plane
#'
#' Takes the output of \code{get_plane} as the definition of a plane, and values
#' for two coordinates. The missing third coordinate will be computed and
#' returned. If a vector of points is provided for each coordinate the vectors
#' must be the same length.
#'
#' @param plane A list containing \code{normal} and \code{origin} of plane.
#' And any two of the following three:
#' @param x Coordinate along Right-Left axis
#' @param y Coordinate along Anterior-Posterior axis
#' @param z Coordinate along Inferior-Superior axis
#' @return The value for the coordinate not provided
#' @examples
#' \dontrun{
#' plane_point(plane, x=-10:10, z=1) # will return 21 values for y
#' plane_point(plane, x=-10:10, y=1:21) # will return 21 values for z
#' }
plane_point <- function(plane,x,y,z) {
  ncoordin <- sum(c(missing(x),missing(y),missing(z)))
  if (ncoordin > 1) {stop("At most one coordinate can be left unspecified.")}
  else if (ncoordin == 0) {
    warning("All coordinates specified in input. Returning NULL.")
    return(NULL)
  }
  d <- sum(-plane$normal * plane$origin)
  if(missing(x)) {
    ny <- length(y)
    nz <- length(z)
    if (all(c(ny,nz)>1)) {
      if (ny != nz) {
        stop("y and z differ in length.")
      }
    }
    p <- (rowSums(cbind(-plane$normal[2]*y,-plane$normal[3]*z))-d)/plane$normal[1]
  } else if (missing(y)) {
    nx <- length(x)
    nz <- length(z)
    if (all(c(nx,nz)>1)) {
      if (nx != nz) {
        stop("x and z differ in length.")
      }
    }
    p <- (rowSums(cbind(-plane$normal[1]*x,-plane$normal[3]*z))-d)/plane$normal[2]
  } else if (missing(z)) {
    nx <- length(x)
    ny <- length(y)
    if (all(c(nx,ny)>1)) {
      if (nx != ny) {
        stop("x and y differ in length.")
      }
    }
    p <- (rowSums(cbind(-plane$normal[1]*x,-plane$normal[2]*y))-d)/plane$normal[3]
  }
  return(p)
}

#' Launch the shiny application (GUI)
#'
#' @return An interactive GUI with several plots and useful information.
#' @examples
#' ## Not run:
#' \dontrun{
#' ShinyATLROI::launchApp()
#' }
#' ## End(**Not run**)
#' @export
launchApp <- function() {
  shiny::runApp(system.file('app', package='ShinyATLROI'))
}
