#' Temporal Lobe cortical ROIs from both hemispheres
#'
#' The voxel labeles are taken from the aparc.a2009s+aseg_rank dataset bundled
#' with the TT_N27 SUMA surfaces. Coordinates are in RAI order.
#'
#' @format A data frame with 115414 rows and 10 variables:
#' \describe{
#'  item{i} {Right to Left, voxel index}
#'  item{j} {Anterior to Posterior, voxel index}
#'  item{k} {Inferior to Superior, voxel index}
#'  item{x} {Right to Left, voxel coordinate}
#'  item{y} {Anterior to Posterior, voxel coordinate}
#'  item{z} {Inferior to Superior, voxel coordinate}
#'  item{value_full} {factor, with levels 30 for ROIs separate by hemisphere}
#'  item{value} {factor, with 15 levels collapsed over hemispheres}
#'  item{hemisphere} {factor, with 2 levels [right,left]}
#'  item{surface} {factor,with 2 levels [medial,lateral]}
#' }
#' @source \url{https://afni.nimh.nih.gov/pub/dist/tgz/suma_TT_N27.tgz}
"temporal_lobes_atlas"
