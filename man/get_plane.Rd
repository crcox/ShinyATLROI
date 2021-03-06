% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_plane}
\alias{get_plane}
\title{Define a plane given slope, rotation, and intercept}
\usage{
get_plane(slope, rotation, intercept)
}
\arguments{
\item{slope}{Slope parameter from the Shiny interface}

\item{rotation}{Rotation parameter from the Shiny interface (degrees)}

\item{intercept}{Intercept parameter from the Shiny interface (point along AP axis)}
}
\value{
Atlas with roi_boundary_{x,y,z} columns defining the plane.
}
\description{
This is a special-purpose utility function for translating "user friendly"
input values from Shiny app into a rotation+translation matrix. The function
accounts for the ideosyncratic definition of slope (which is defined relative
to a 2D plane where x=z and y=y in atlas space), rotation (which is
specifically around the Inferior-Superior axis), and intercept (which is
where the plane crosses the Anterior-Posterior axis when x=z=0) when creating
this matrix.
}
\details{
The function returns a list containing the normal and origin of the plane as
3D vectors.
}
\examples{
\dontrun{
input <- list(slope=1,rotation=45, intercept=10)
get_plane(input$slope, input$rotation, input$intercept)
}
}

