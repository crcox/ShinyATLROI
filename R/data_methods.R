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
#' @param atlas Dataframe with xyz coordinates, hemisphere codes, and a value for each voxel.
#' @param input Shiny reactive input object.
#' @param R Shiny reactive conductor to store the plane.
#' @return Atlas with roi_boundary_{x,y,z} columns defining the plane.
#' @examples
#' \dontrun{
#'   coronal_plane(atlas, input, R)
#' }
#'
#' @export
coronal_plane <- function(atlas, input, R=NULL) {
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
  #atlas <- dplyr::select(
  #  atlas,
  #  x,
  #  y,
  #  z,
  #  roi_boundary_x,
  #  roi_boundary_y,
  #  roi_boundary_z,
  #  value,
  #  value_full,
  #  hemisphere
  #)
  if (!is.null(R)) {
    R$coronal_slice <- atlas
  }
  else {
    return(atlas)
  }
}

#' Project 3D sagital "shell" onto 2D sagital plane
#'
#' @param atlas Dataframe with xyz coordinates, hemisphere codes, and a value for each voxel.
#' @return Dataframe with x-axis of each hemisphere collapsed to two surfaces.
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

#' Return portion of atlas that falls on or anterior to plane
#'
#' The plane will be defined by fields \code{roi_boundary_{x,y,z}}. Function
#' assumes coordinates are in RAI order, meaning that y values on the AP-axis
#' are large in the back and small in the front of the brain.
#'
#' X axis: Left-Right
#' Y axis: Anterior-Posterior
#' Z axis: Inferior-Superior
#'
#' @param atlas_wplane Dataframe with xyz coordinates, and \code{roi_boundary_{x,y,z}}
#' @param anterior Flag indicating whether to select region anterior or posterior to the plane. Default: anterior
#' @param raw_index Flag to instruct function to return the original
#'   aparc.a2009s+aseg indexes, rather than the internal indexes that group
#'   together analogous regions in each hemisphere. Default: FALSE
#' @return A subset of atlas_wplane, including voxels that fall on or anterior to plane.
#' @examples
#' \dontrun{
#'   coronal_plane(atlas, input, R)
#' }
#'
#' @export
select_roi <- function(atlas_wplane, anterior=TRUE, raw_index=FALSE){
  if (anterior) {
    roi <- dplyr::filter(atlas_wplane, y<=roi_boundary_y)
  } else {
    roi <- dplyr::filter(atlas_wplane, y>=roi_boundary_y)
  }
  if (raw_index) {
    roi$index <- as.numeric(roi$value_full)
  } else {
    roi$index <- as.numeric(roi$value)
  }
  roi <- dplyr::select(roi, x,y,z,index,value)
  return(roi)
}

#' Return portion of atlas that falls on the plane
#'
#' The plane will be defined by fields \code{roi_boundary_{x,y,z}}.
#'
#' @param atlas_wplane Dataframe with xyz coordinates, and \code{roi_boundary_{x,y,z}}
#' @param raw_index Flag to instruct function to return the original
#'   aparc.a2009s+aseg indexes, rather than the internal indexes that group
#'   together analogous regions in each hemisphere. Default: FALSE
#' @return A subset of atlas_wplane, including voxels that fall on the plane.
#'
#' @export
select_plane <- function(atlas_wplane, raw_index=FALSE){
  plane <- dplyr::filter(atlas_wplane, y==roi_boundary_y)
  if (raw_index) {
    plane$index <- as.numeric(plane$value_full)
  } else {
    plane$index <- as.numeric(plane$value)
  }
  plane <- dplyr::select(plane, x,y,z,index,value)
  return(plane)
}

#' Read atlas and key text files and compose useful dataframes
#'
#' The assumption is that the atlas text file was created with 3dmaskdump
#' (AFNI), which writes out both voxel indexes (\code{i,j,k}) and voxel
#' coordinates (\code{x,y,z}). The \code{i,j,k} values are not used to within this
#' package, but might be included in the text file. If txt_has_ijk=TRUE, the
#' first 3 columns (containing the \code{i,j,k} values) will be dropped.
#'
#' X axis: Left-Right
#' Y axis: Anterior-Posterior
#' Z axis: Inferior-Superior
#'
#' @param atlas_file Output from 3dmaskdump; space-delimited text file with one
#'   row per voxel. The last column of each row is the voxel value. The voxel
#'   values should correspond to indexes found in the first column of the key
#'   file.
#' @param key_file aparc.a2009s+aseg_rank.niml.lt, or some subset thereof. First
#'   column is an index, and the second column is a label. If a key file is not
#'   provided, the atlas cannot be labeled, and atlas regions across hemispheres
#'   cannot be grouped.
#' @param txt_has_index Flag to indicate whether the first column of the
#'   \code{atlas_file} is a 1D voxel index. Default: FALSE
#' @param txt_has_ijk Flag to indicate whether the first three columns of the
#'   \code{atlas_file} is are the 3D voxel indexes \code{i,j,k}. Default: TRUE
#' @return A list containing the parsed and formatted atlas and key.
#' @examples
#' \dontrun{
#'   read_atlas(atlas_file, key_file, txt_has_index=FALSE, txt_has_ijk=TRUE)
#' }
#'
#' @export
read_atlas <- function(atlas_file, key_file=NULL, txt_has_index=FALSE, txt_has_ijk=TRUE) {
  # Read atlas
  if (txt_has_index && txt_has_ijk) {atlas <- read.csv(file=atlas_file, header=FALSE, sep=' ')[,5:8]}
  else if (txt_has_index)           {atlas <- read.csv(file=atlas_file, header=FALSE, sep=' ')[,2:5]}
  else if (txt_has_ijk)             {atlas <- read.csv(file=atlas_file, header=FALSE, sep=' ')[,4:7]}
  else                              {atlas <- read.csv(file=atlas_file, header=FALSE, sep=' ')[,1:4]}
  names(atlas) <- c('x','y','z','value_full')
  # Read key
  if (!is.null(key_file)) {
    key <- read.csv(file=key_file, header=FALSE, sep= ' ')
    names(key) <- c("index_raw","label_full")
    key$index_full <- rank(key$index_raw)
    key$label <- stringr::str_replace(key$label_full,'ctx_[lr]h_','')
    key$index <- dplyr::group_indices(key,label)
    # Create hemisphere factor
    atlas <- dplyr::left_join(atlas,dplyr::select(key, value_full=index_raw, value=index))
    atlas$hemisphere <- factor(
      ifelse(atlas$x>0,1,2),
      levels=c(1,2),
      labels=c('left','right')
    )
    # Create surface factor
    atlas <- atlas %>%
      dplyr::group_by(hemisphere) %>%
      dplyr::mutate(
        surface=factor(
          ifelse(abs(x)<median(abs(x)),1,2),
          levels=c(1,2),
          labels=c('medial','lateral')
        )
      ) %>%
      dplyr::ungroup()
    # Indexes to Factors
    atlas$value_full <- factor(
      atlas$value_full,
      levels=key$index_raw,
      labels=key$label_full
    )
    atlas$value <- factor(
      atlas$value,
      levels=unique(key$index),
      labels=unique(key$label)
    )
  } else {
    key <- NULL
    atlas$value <- atlas$value_full
  }
  return(list(atlas=atlas,key=key))
}
