roi <- filter(
  temporal_lobes_atlas,
  value %in% c(
    "G_oc-temp_med-Lingual",
    "G_oc-temp_lat-fusifor",
    "G_temporal_inf",
    "S_temporal_inf",
    "S_oc-temp_med_and_Lingual",
    "S_oc-temp_lat"
  ),
  y >= 30
)
roi$index <- as.numeric(roi$value)
roi <- select(roi, x,y,z,index,value)
filepath <- "/media/chris/Data1/MRI/Manchester/ROI/Dissertation/visual_roi_y30.csv"
write.csv(x=roi,file = filepath,quote = FALSE,row.names = F)

roi <- filter(
  temporal_lobes_atlas,
  value %in% c(
    "G_oc-temp_med-Lingual",
    "G_oc-temp_lat-fusifor",
    "G_temporal_inf",
    "S_temporal_inf",
    "S_oc-temp_med_and_Lingual",
    "S_oc-temp_lat"
  ),
  y >= 25
)
roi$index <- as.numeric(roi$value)
roi <- select(roi, x,y,z,index,value)
filepath <- "/media/chris/Data1/MRI/Manchester/ROI/Dissertation/visual_roi_y25.csv"
write.csv(x=roi,file = filepath,quote = FALSE,row.names = F)
