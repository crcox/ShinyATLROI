#' @importFrom magrittr "%>%"
NULL
#' Add a reference plane to a 3D atlas
#'
#' The plane can be rotated around the RL axis ("slope") and the IS axis
#' ("rotation"), with respect to an origin that may be shifted along the AP
#' axis ("intecept").
#'
#' X axis: Left-Right
#' Y axis: Anterior-Posterior
#' Z axis: Inferior-Superior
#'
#' @param atlas A dataframe with xyz coordinates, hemisphere codes, and a value for each voxel.
#' @param input Shiny reactive input object.
#' @param R Shiny reactive conductor to store the plane.
#' @return Atlas with roi_boundary_{x,y,z} columns defining the plane.
#' @examples
#' \dontrun{
#'   coronal_plane(atlas, input, R)
#' }
#'
#' @export
coronal_plane <- function(atlas, input, R) {
  if (abs(input$rotation)>0 || abs(input$slope) >0) {
    plane <- get_plane(input$slope,input$rotation,input$intercept)
    d <- sum(-plane$normal * plane$origin)
    atlas$roi_boundary_x <- Inf
    atlas$roi_boundary_y <- ceiling(plane_point(plane,x=abs(atlas$x),z=atlas$z))
    atlas$roi_boundary_z <- Inf
    if (abs(input$rotation) > 0) {
      atlas$roi_boundary_x <- ceiling(plane_point(plane,y=atlas$y,z=atlas$z))
    }
    if (abs(input$slope) > 0) {
      atlas$roi_boundary_z <- ceiling(plane_point(plane,x=abs(atlas$x),y=atlas$y))
    }
#  } else if (abs(input$slope) > 0) {
#    m <- 1/input$slope
#    b <- (-1 * input$intercept * m)
#    atlas$roi_boundary_x <- Inf
#    atlas$roi_boundary_y <- ceiling((atlas$z - b) / m)
#    atlas$roi_boundary_z <- ceiling((atlas$y * m) + b)
  } else {
    atlas$roi_boundary_x <- Inf
    atlas$roi_boundary_y <- ceiling(input$intercept)
    atlas$roi_boundary_z <- Inf
  }
  R$coronal_slice <- dplyr::select(
    atlas,
    x,
    y,
    z,
    roi_boundary_x,
    roi_boundary_y,
    roi_boundary_z,
    value,
    value_full,
    hemisphere
  )
}

#' Project 3D sagital "shell" onto 2D sagital plane
#'
#' @param atlas A dataframe with xyz coordinates, hemisphere codes, and a value for each voxel.
#' @return dataframe with x-axis of each hemisphere collapsed to two surfaces.
#' @examples
#' \dontrun{
#'   sagital_project(atlas)
#' }
#'
#' @export
sagital_project <- function(atlas, key) {
  sagital_projection <- atlas %>%
    dplyr::group_by(y,z,hemisphere,surface) %>%
    dplyr::summarize(
      li = which.max(abs(x)),
      mi = which.min(abs(x)),
      value_full.lat = value_full[li],
      value_full.med = value_full[mi],
      value.lat = value[li],
      value.med = value[mi]
    ) %>%
    dplyr::mutate(
      value_full = ifelse(surface=='lateral',value_full.lat,value_full.med),
      value = ifelse(surface=='lateral',value.lat,value.med)
    ) %>%
    dplyr::select(y,z,hemisphere,surface,value,value_full) %>%
    dplyr::ungroup()

  # Create factors
  sagital_projection$value <- factor(
    sagital_projection$value,
    levels=unique(key$index),
    labels=unique(key$label)
  )
  sagital_projection$value_full <- factor(
    sagital_projection$value_full,
    levels=unique(rank(key$index_full)),
    labels=unique(key$label_full)
  )
  return(sagital_projection)
}
