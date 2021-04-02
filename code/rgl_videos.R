Chewbacca_event1_chew <- filter(Chewbacca_cranium_crs_20170509$list_of_events_labeled[[1]], CycleType == "Chew")
Chewbacca_event1_chew_tongue_coords <- extract_xyz_df(Chewbacca_event1_chew, tongue_columns_Chewbacca)
Chewbacca_event1_chew_hyoid_coords <- extract_xyz_df(Chewbacca_event1_chew, hyoid_columns_Chewbacca)
Chewbacca_event1_chew_symphysis_coords <- extract_xyz_df(Chewbacca_event1_chew, symphysis_columns_Chewbacca)

map(c(1:length(Chewbacca_event1_chew_tongue_coords$coordinates)), plot_tongue_and_hyoid_points, Chewbacca_event1_chew_tongue_coords, Chewbacca_event1_chew_hyoid_coords, Chewbacca_event1_chew_symphysis_coords, sagittal)


Hestia_event3_chew <- filter(Hestia_cranium_crs_20160715$list_of_events_labeled[[3]], CycleType == "Chew")
Hestia_event3_chew_tongue_coords <- extract_xyz_df(Hestia_event3_chew, tongue_columns_Hestia)
Hestia_event3_chew_hyoid_coords <- extract_xyz_df(Hestia_event3_chew, hyoid_columns_Hestia)
Hestia_event3_chew_symphysis_coords <- extract_xyz_df(Hestia_event3_chew, symphysis_columns_Hestia)

map(c(1:length(Hestia_event3_chew_tongue_coords$coordinates)), plot_tongue_and_hyoid_points, Hestia_event3_chew_tongue_coords, Hestia_event3_chew_hyoid_coords, Hestia_event3_chew_symphysis_coords, sagittal)
