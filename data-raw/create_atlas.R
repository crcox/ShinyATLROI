library('ShinyATLROI')
atlas_file <- './data-raw/temporal_lobes_atlas.txt'
key_file <- './data-raw/temporal_lobes_key.txt'
ATLAS <- ShinyATLROI::read_atlas(atlas_file, key_file, txt_has_ijk=TRUE, txt_has_index=FALSE)
temporal_lobes_atlas <- ATLAS$atlas
temporal_lobes_key <- ATLAS$key
devtools::use_data(temporal_lobes_atlas,pkg="~/src/ShinyATLROI/",overwrite=TRUE)
devtools::use_data(temporal_lobes_key,pkg="~/src/ShinyATLROI/",overwrite=TRUE)

#atlas <- read.csv(file='./data-raw/temporal_lobes_atlas.txt', header=FALSE, sep=' ')
#names(atlas) <- c('i','j','k','x','y','z','value_full')
#key <- read.csv(file="./data-raw/temporal_lobes_key.txt", header=FALSE, sep= ' ')
#names(key) <- c("index_raw","label_full")
#key$index_full <- rank(key$index_raw)
#key$label <- stringr::str_replace(key$label_full,'ctx_[lr]h_','')
#key$index <- dplyr::group_indices(key,label)
#
#atlas <- left_join(atlas,select(key, value_full=index_raw, value=index))
#atlas$hemisphere <- factor(
#  ifelse(atlas$x>0,1,2),
#  levels=c(1,2),
#  labels=c('left','right')
#)
#atlas <- atlas %>%
#  group_by(hemisphere) %>%
#  mutate(
#    surface=factor(
#      ifelse(abs(x)<median(abs(x)),1,2),
#      levels=c(1,2),
#      labels=c('medial','lateral')
#    )
#  ) %>%
#  ungroup()
#
#atlas$value_full <- factor(
#  atlas$value_full,
#  levels=key$index_raw,
#  labels=key$label_full
#)
#atlas$value <- factor(
#  atlas$value,
#  levels=unique(key$index),
#  labels=unique(key$label)
#)
