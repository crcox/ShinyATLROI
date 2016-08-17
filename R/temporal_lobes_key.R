#' Temporal Lobe cortical ROIs from both hemispheres (key)
#'
#' The voxel labeles are taken from the aparc.a2009s+aseg_rank dataset bundled
#' with the TT_N27 SUMA surfaces. Coordinates are in RAI order.
#'
#' @format A data frame with 30 rows and 5 variables:
#' \describe{
#'  item{index_raw} {integer, code from aparc.a2009s+aseg_rank}
#'  item{index_full} {integer, compressed rank index of index_raw}
#'  item{label_full} {character, full roi codes from aparc.a2009s+aseg_rank}
#'  item{index} {integer, index grouping ROIs in each hemisphere as a single item}
#'  item{label} {character, label_full with hemisphere information stripped}
#' }
#' @source \url{https://afni.nimh.nih.gov/pub/dist/tgz/suma_TT_N27.tgz}
"temporal_lobes_key"
