source("code/Analysis_template.R")

# SAMPLE SIZES ----
chewbacca_nrightchews <- filter(chewbacca_analysis$cranium_CRS$cleaned_cycles, 
                                   CycleType_fromMinGape=="Chew" & CycleSide_fromMinGape == "right") %>% 
  select(Event, Cycle_fromMinGape) %>%
  unique() %>%
  nrow()

chewbacca_nleftchews <- filter(chewbacca_analysis$cranium_CRS$cleaned_cycles, 
                                CycleType_fromMinGape=="Chew" & CycleSide_fromMinGape == "left") %>% 
  select(Event, Cycle_fromMinGape) %>%
  unique() %>%
  nrow()


chewbacca_nswallows <- filter(chewbacca_analysis$cranium_CRS$cleaned_cycles, 
                           CycleType_fromMinGape=="Swallow") %>% 
  select(Event, Cycle_fromMinGape) %>%
  unique() %>%
  nrow()

hestia_nrightchews <- filter(hestia_analysis$cranium_CRS$cleaned_cycles, 
                                CycleType_fromMinGape=="Chew" & CycleSide_fromMinGape == "right") %>% 
  select(Event, Cycle_fromMinGape) %>%
  unique() %>%
  nrow()

hestia_nleftchews <- filter(hestia_analysis$cranium_CRS$cleaned_cycles, 
                             CycleType_fromMinGape=="Chew" & CycleSide_fromMinGape == "left") %>% 
  select(Event, Cycle_fromMinGape) %>%
  unique() %>%
  nrow()

hestia_nswallows <- filter(hestia_analysis$cranium_CRS$cleaned_cycles, 
                              CycleType_fromMinGape=="Swallow") %>% 
  select(Event, Cycle_fromMinGape) %>%
  unique() %>%
  nrow()

jb_nrightchews <- filter(jb_analysis$cranium_CRS$cleaned_cycles, 
                             CycleType_fromMinGape=="Chew" & CycleSide_fromMinGape == "right") %>% 
  select(Event, Cycle_fromMinGape) %>%
  unique() %>%
  nrow()

jb_nleftchews <- filter(jb_analysis$cranium_CRS$cleaned_cycles, 
                            CycleType_fromMinGape=="Chew" & CycleSide_fromMinGape == "left") %>% 
  select(Event, Cycle_fromMinGape) %>%
  unique() %>%
  nrow()

jb_nswallows <- filter(jb_analysis$cranium_CRS$cleaned_cycles, 
                           CycleType_fromMinGape=="Swallow") %>% 
  select(Event, Cycle_fromMinGape) %>%
  unique() %>%
  nrow()

kiki_nrightchews <- filter(kiki_analysis$cranium_CRS$cleaned_cycles, 
                          CycleType_fromMinGape=="Chew" & CycleSide_fromMinGape == "right") %>% 
  select(Event, Cycle_fromMinGape) %>%
  unique() %>%
  nrow()


kiki_nleftchews <- filter(kiki_analysis$cranium_CRS$cleaned_cycles, 
                        CycleType_fromMinGape=="Chew" & CycleSide_fromMinGape == "left") %>% 
  select(Event, Cycle_fromMinGape) %>%
  unique() %>%
  nrow()

kiki_nswallows <- filter(kiki_analysis$cranium_CRS$cleaned_cycles, 
                       CycleType_fromMinGape=="Swallow") %>% 
  select(Event, Cycle_fromMinGape) %>%
  unique() %>%
  nrow()




# GAPE PLOTS ----
gape_limits <- find_limits("mandiblePitch", chewbacca_summary, hestia_summary, jb_summary, kiki_summary)
chewbacca_rightchews_gape <- plot_gape_pitch(chewbacca_summary, chewbacca_analysis, gape_limits)
hestia_rightchews_gape <- plot_gape_pitch(hestia_summary, hestia_analysis, gape_limits)
jb_rightchews_gape <- plot_gape_pitch(jb_summary, jb_analysis, gape_limits)
kiki_leftchews_gape <- plot_gape_pitch(kiki_summary, kiki_analysis, gape_limits)

# TONGUE TIP PLOTS ----
tip_limits <- find_limits("^[Tt]ongue[.]*[Aa]nterior_[0-9]+Hz_X", 
                          chewbacca_summary, hestia_summary, jb_summary, kiki_summary)
chewbacca_rightchews_tip <- plot_tongue_tip(chewbacca_summary, chewbacca_analysis, tip_limits)
hestia_rightchews_tip <- plot_tongue_tip(hestia_summary, hestia_analysis, tip_limits)
jb_rightchews_tip <- plot_tongue_tip(jb_summary, jb_analysis, tip_limits)
kiki_leftchews_tip <- plot_tongue_tip(kiki_summary, kiki_analysis, tip_limits)

# ROTATION PLOTS ----
right_middle_limits <- multicolumn_find_limits(c("WorkingMiddleHorizontalRot", "WorkingMiddleVerticalRot"), chewbacca_summary, hestia_summary, jb_summary)
left_middle_limits <- multicolumn_find_limits(c("WorkingMiddleHorizontalRot", "WorkingMiddleVerticalRot"), kiki_summary)
middle_limits <- compare_limits(right_middle_limits, left_middle_limits)

right_post_limits <- multicolumn_find_limits(c("WorkingPosteriorHorizontalRot", "WorkingPosteriorVerticalRot"), chewbacca_summary, hestia_summary, jb_summary)
left_post_limits <- multicolumn_find_limits(c("WorkingPosteriorHorizontalRot", "WorkingPosteriorVerticalRot"), kiki_summary)
post_limits <- compare_limits(right_post_limits, left_post_limits)

rotation_plot_limits <- compare_limits(middle_limits, post_limits)
chewbacca_rightchews_middlerot <-  plot_rotations(chewbacca_summary, "middle", chewbacca_analysis, rotation_plot_limits)
hestia_rightchews_middlerot <- plot_rotations(hestia_summary, "middle", hestia_analysis, rotation_plot_limits)
jb_rightchews_middlerot <- plot_rotations(jb_summary, "middle", jb_analysis, rotation_plot_limits)
kiki_leftchews_middlerot <- plot_rotations(kiki_summary, "middle", kiki_analysis, rotation_plot_limits)


chewbacca_rightchews_postrot <- plot_rotations(chewbacca_summary, "posterior", chewbacca_analysis, rotation_plot_limits)
hestia_rightchews_postrot <- plot_rotations(hestia_summary, "posterior", hestia_analysis, rotation_plot_limits)
jb_rightchews_postrot <- plot_rotations(jb_summary, "posterior", jb_analysis, rotation_plot_limits)
kiki_leftchews_postrot <- plot_rotations(kiki_summary, "posterior", kiki_analysis, rotation_plot_limits)

# STRAIN PLOTS ----

ant_lr_limits <- multicolumn_find_limits(c("WorkingAnteriorStrain", "BalancingAnteriorStrain"), chewbacca_summary, hestia_summary, kiki_summary, jb_summary)
post_lr_limits <- multicolumn_find_limits(c("WorkingPosteriorStrain", "BalancingPosteriorStrain"), chewbacca_summary, hestia_summary, kiki_summary, jb_summary)

lr_limits <- compare_limits(ant_lr_limits, post_lr_limits)
chewbacca_ant_distances <- plot_lr_midtoant_distance(chewbacca_summary, chewbacca_analysis, lr_limits)
hestia_ant_distances <- plot_lr_midtoant_distance(hestia_summary, hestia_analysis, lr_limits)
jb_ant_distances <- plot_lr_midtoant_distance(jb_summary, jb_analysis, lr_limits)
kiki_ant_distances <- plot_lr_midtoant_distance(kiki_summary, kiki_analysis, lr_limits)

chewbacca_post_distances <- plot_lr_midtopost_distance(chewbacca_summary, chewbacca_analysis, lr_limits)
hestia_post_distances <- plot_lr_midtopost_distance(hestia_summary, hestia_analysis, lr_limits)
jb_post_distances <- plot_lr_midtopost_distance(jb_summary, jb_analysis,lr_limits)
kiki_post_distances <- plot_lr_midtopost_distance(kiki_summary, kiki_analysis, lr_limits)

# ANGLE PLOTS ----
angle_limits <- find_limits("TongueAngle", chewbacca_summary, hestia_summary, jb_summary, kiki_summary)
chewbacca_angle <- plot_angle(chewbacca_summary, chewbacca_analysis, angle_limits)
hestia_angle <- plot_angle(hestia_summary, hestia_analysis,angle_limits)
jb_angle <- plot_angle(jb_summary, jb_analysis, angle_limits)
kiki_angle <- plot_angle(kiki_summary, kiki_analysis, angle_limits)


# BIG COMPOSITE PLOT ----

# svg(filename = paste0(paste0("figures/TongueRotations_", gsub("-", "",Sys.Date())), ".svg"), width = 8, height = 8)
# plot_grid(chewbacca_rightchews_gape, hestia_rightchews_gape, jb_rightchews_gape, kiki_leftchews_gape,
#           chewbacca_rightchews_tip, hestia_rightchews_tip, jb_rightchews_tip, kiki_leftchews_tip,
#           chewbacca_rightchews_middlerot, hestia_rightchews_middlerot, jb_rightchews_middlerot, kiki_leftchews_middlerot, 
#           chewbacca_rightchews_postrot, hestia_rightchews_postrot, jb_rightchews_postrot, kiki_leftchews_postrot, 
#           ncol = 4)
# dev.off()
#
# svg(filename = paste0(paste0("figures/TongueDistances_", gsub("-", "",Sys.Date())), ".svg"), width = 8, height = 4)
# plot_grid(chewbacca_ant_distances, hestia_ant_distances, jb_ant_distances, kiki_ant_distances,
#           chewbacca_post_distances, hestia_post_distances, jb_post_distances, kiki_post_distances,
#           ncol = 4)
# dev.off()
# 
# svg(filename = paste0(paste0("figures/TongueAngles_", gsub("-", "",Sys.Date())), ".svg"), width = 8, height = 2)
# plot_grid(chewbacca_angle, hestia_angle, jb_angle, kiki_angle, ncol = 4)
# dev.off()

svg(filename = paste0(paste0("figures/Composite_", gsub("-", "",Sys.Date())), ".svg"), width = 6, height = 10)
plot_grid(chewbacca_rightchews_gape, hestia_rightchews_gape, jb_rightchews_gape, kiki_leftchews_gape,
          chewbacca_rightchews_tip, hestia_rightchews_tip, jb_rightchews_tip, kiki_leftchews_tip,
          chewbacca_rightchews_middlerot, hestia_rightchews_middlerot, jb_rightchews_middlerot, kiki_leftchews_middlerot, 
          chewbacca_rightchews_postrot, hestia_rightchews_postrot, jb_rightchews_postrot, kiki_leftchews_postrot,
          chewbacca_ant_distances, hestia_ant_distances, jb_ant_distances, kiki_ant_distances,
          chewbacca_post_distances, hestia_post_distances, jb_post_distances, kiki_post_distances,
          chewbacca_angle, hestia_angle, jb_angle, kiki_angle, align = 'hv',
          ncol = 4)
dev.off()

# PHASE PLOTS ----
# mandiblepitch_phase<-plot_circular_histogram("mandiblePitch_mintime", list("Chewbacca" = chewbacca_analysis, 
#                                                       "Hestia"=hestia_analysis, 
#                                                       "JB"=jb_analysis, 
#                                                       "Kiki" = kiki_analysis))
# workingantstrain_phase <- plot_circular_histogram("WorkingAnteriorStrain_mintime", list("Chewbacca" = chewbacca_analysis, 
#                                                                              "Hestia"=hestia_analysis, 
#                                                                              "JB"=jb_analysis, 
#                                                                              "Kiki" = kiki_analysis))
# balancingantstrain_phase <- plot_circular_histogram("BalancingAnteriorStrain_mintime", list("Chewbacca" = chewbacca_analysis, 
#                                                                                   "Hestia"=hestia_analysis, 
#                                                                                   "JB"=jb_analysis, 
#                                                                                   "Kiki" = kiki_analysis))
# workingpoststrain_phase <- plot_circular_histogram("WorkingPosteriorStrain_mintime", list("Chewbacca" = chewbacca_analysis, 
#                                                                                  "Hestia"=hestia_analysis, 
#                                                                                  "JB"=jb_analysis, 
#                                                                                  "Kiki" = kiki_analysis))
# 
# balancingpoststrain_phase <- plot_circular_histogram("BalancingPosteriorStrain_mintime", list("Chewbacca" = chewbacca_analysis, 
#                                                                                    "Hestia"=hestia_analysis, 
#                                                                                    "JB"=jb_analysis, 
#                                                                                    "Kiki" = kiki_analysis))
# workingmidhorzrot_phase <- plot_circular_histogram("WorkingMiddleHorizontalRot_mintime", list("Chewbacca" = chewbacca_analysis, 
#                                                                                  "Hestia"=hestia_analysis, 
#                                                                                  "JB"=jb_analysis, 
#                                                                                  "Kiki" = kiki_analysis))
# workingmidvertrot_phase <- plot_circular_histogram("WorkingMiddleVerticalRot_mintime", list("Chewbacca" = chewbacca_analysis, 
#                                                                            "Hestia"=hestia_analysis, 
#                                                                            "JB"=jb_analysis, 
#                                                                            "Kiki" = kiki_analysis))
# workingposthorzrot_phase <- plot_circular_histogram("WorkingPosteriorHorizontalRot_mintime", list("Chewbacca" = chewbacca_analysis, 
#                                                                             "Hestia"=hestia_analysis, 
#                                                                             "JB"=jb_analysis, 
#                                                                             "Kiki" = kiki_analysis))
# workingpostvertrot_phase <- plot_circular_histogram("WorkingPosteriorVerticalRot_mintime", list("Chewbacca" = chewbacca_analysis, 
#                                                                             "Hestia"=hestia_analysis, 
#                                                                             "JB"=jb_analysis, 
#                                                                             "Kiki" = kiki_analysis))
# tongueangle_phase <- plot_circular_histogram("TongueAngle_mintime", list("Chewbacca" = chewbacca_analysis, 
#                                                                                          "Hestia"=hestia_analysis, 
#                                                                                          "JB"=jb_analysis, 
#                                                                                          "Kiki" = kiki_analysis))
# 
# plot_grid(mandiblepitch_phase, tongueangle_phase, workingantstrain_phase, balancingantstrain_phase,
#           workingpoststrain_phase, balancingpoststrain_phase, workingmidhorzrot_phase, 
#           workingmidvertrot_phase, workingposthorzrot_phase, workingpostvertrot_phase, ncol = 2)
# 
# 
# # BASE GRAPHICS ----
#  
# 
# plot_gape_baseR <- function(summary_object, title){
# par(mar = c(5,5,2,5))
# plot(summary_object$ScaledTime_fromMinGape,
#      summary_object$mandiblePitch_mean,
#      main = title,
#      xlab = "Scaled Time",
#      ylab = "Mandible Pitch",
#      ylim = c(-12,4),
#      pch = 16,
#      col = "gray33",
#      xaxs = "i"
#      )
# arrows(x0 = summary_object$ScaledTime_fromMinGape,
#        y0 = summary_object$mandiblePitch_mean - summary_object$mandiblePitch_sd,
#        x1 = summary_object$ScaledTime_fromMinGape,
#        y1 = summary_object$mandiblePitch_mean + summary_object$mandiblePitch_sd,
#        code = 3,
#        angle = 90,
#        length = 0.01,
#        col = "gray33")
# par(new=T)
# plot(summary_object$ScaledTime_fromMinGape,
#      summary_object$mandibleYaw_mean,
#      axes = F, xlab=NA, ylab = NA, ylim=c(-2, 0.5),
#      pch = 16, col = "gray78")
# arrows(x0 = summary_object$ScaledTime_fromMinGape,
#        y0 = summary_object$mandibleYaw_mean - summary_object$mandibleYaw_sd,
#        x1 = summary_object$ScaledTime_fromMinGape,
#        y1 = summary_object$mandibleYaw_mean + summary_object$mandibleYaw_sd,
#        code = 3,
#        angle = 90,
#        length = 0.01,
#        col = "gray78")
# axis(side = 4)
# mtext(side = 4, line = 3, "Mandible Yaw")
# 
# }
