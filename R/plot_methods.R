#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 "aes"
NULL
#' Plot sagital projections with interactive ROI reference lines
#'
#' Generates a 4 panel (hemisphere by surface [medial,lateral]) figure, where
#' anatomical regions are color coded. The Shiny interface controls the slope,
#' rotation, and position of a plane that is used to mark the posterior extent
#' of the anterior temporal lobe (ATL) as a custom anatomical region of
#' interest. Hovering over data points will update a panel containing the
#' aparc.2009a+aseg code at that position.
#'
#' @param d A dataframe with yz coordinates, hemisphere codes, surface codes,
#'   and a value for each voxel projected onto the surfaces.
#' @param atlas_wplane A dataframe that contains information about the
#'   posterior extent of the ATL in 3 dimensions.
#' @param input Shiny input object containing information from control widgets.
#' @return A 4-panel figure to render and display
#' @examples
#' \dontrun{
#'   output$sagitalPlot <- shiny::renderPlot({
#'     sagitalPlot(sagital_projection, reactiveA(),input)
#'   })
#' }
#'
#' @export
sagitalPlot <- function(d, atlas_wplane, input) {
  cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
  myPal <- cols(length(levels(d$value)))
  plane <- get_plane(input$slope,input$rotation,input$intercept)
  topcolor <- ifelse(abs(input$rotation)>0,'blue','black')
  if (abs(input$slope) > 0) {
    m <- 1/input$slope
    if (abs(m) > 1) {
      dp <- dplyr::filter(atlas_wplane,y==roi_boundary_y)
    } else {
      dp <- dplyr::filter(atlas_wplane,z==roi_boundary_z)
    }
  } else {
    dp <- dplyr::filter(atlas_wplane,y==ceiling(input$intercept))
  }
  if (abs(input$slope) > 0) {
    m <- 1/input$slope
    b_med <- plane_point(plane,x=min(abs(dp$x)),y=0)
    b_lat <- plane_point(plane,x=max(abs(dp$x)),y=0)
    ggplot2::ggplot(d, aes(x=y,y=z,color=value)) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(slope = m, intercept = b_lat, color='red') +
      ggplot2::geom_abline(slope = m, intercept = b_med, color=topcolor) +
      ggplot2::scale_color_manual(values = myPal, drop=FALSE) +
      ggplot2::facet_grid(surface~hemisphere)
  } else {
    b_med <- plane_point(plane,x=min(abs(dp$x)),z=0)
    b_lat <- plane_point(plane,x=max(abs(dp$x)),z=0)
    ggplot2::ggplot(d, aes(x=y,y=z,color=value)) +
      ggplot2::geom_point() +
      ggplot2::geom_vline(xintercept = b_lat, color='red') +
      ggplot2::geom_vline(xintercept = b_med, color=topcolor) +
      ggplot2::scale_color_manual(values = myPal, drop=FALSE) +
      ggplot2::facet_grid(surface~hemisphere)
  }
}

#' Plot a responsive coronal slice
#'
#' In this figure, right=right. The coronal slice will update in response to the
#' control wigits. Hovering over data points will update a panel containing the
#' aparc.2009a+aseg code at that position.
#'
#' @param d A dataframe that contains information about the
#'   posterior extent of the ATL in 3 dimensions.
#' @param input Shiny input object containing information from control widgets.
#' @return A 1-panel figure to render and display
#' @examples
#' \dontrun{
#'   output$sagitalPlot <- shiny::renderPlot({
#'     sagitalPlot(reactiveA(), input)
#'   })
#' }
#'
#' @export
coronalPlot <- function(d, input) {
  cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
  myPal <- cols(length(levels(d$value)))
  if (abs(input$slope) > 0 || abs(input$rotation)>0) {
    m <- 1/input$slope
    if (abs(m) > 1) {
      dp <- dplyr::filter(d,y==roi_boundary_y)
    } else {
      dp <- dplyr::filter(d,z==roi_boundary_z)
    }
  } else {
    dp <- dplyr::filter(d,y==ceiling(input$intercept))
  }
  ggplot2::ggplot(dp, aes(x=x,y=z,color=value)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = myPal, drop=FALSE) +
    ggplot2::coord_cartesian(xlim = c(-70, 70)) +
    ggplot2::scale_x_reverse()
}
