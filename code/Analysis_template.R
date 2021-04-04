# Step 0: Import Libraries and Custom Functions ----
library(tidyverse)
library(cowplot)
library(circular)
source("code/All_Functions_20210326.R")
source("code/CircularBoxplot_stats.R")


# Step 1: Define analysis variables ----


chewbacca <- list(cranium_filepath = "data/20170509_Chewbacca/Feeding_Chewbacca_20170509_Cranium_20190905.csv",
                  mandible_filepath = "data/20170509_Chewbacca/Feeding_Chewbacca_20170509_Mandible_20190905.csv",
                  mandible_pitch_correction = FALSE,
                  working_side = "right",
                  cycles_to_exclude = NULL)

hestia <- list(cranium_filepath = "data/20160715_Hestia/Feeding_Hestia_20160715_Cranium_20180320.csv",
               mandible_filepath = "data/20160715_Hestia/Feeding_Hestia_20160715_Mandible_20180320.csv",
               mandible_pitch_correction = TRUE, 
               working_side = "right",
               cycles_to_exclude = list(list("Event" = "Evt19", "Cycle" = 11, "Rationale" = "No cycle side or type")))

jb <-list(cranium_filepath = "data/20160512_JB/Feeding_JB_20160512_Cranium_20180320.csv",
          mandible_filepath = "data/20160512_JB/Feeding_JB_20160512_Mandible_20180320.csv",
          mandible_pitch_correction = TRUE, 
          working_side = "right",
          cycles_to_exclude = list(list("Event" = "Evt01", "Cycle" = 8, "Rationale" = "end of sequence, incomplete cycle"),
                                   list("Event" = "Evt07", "Cycle" = 1, "Rationale" = "interrupted cycles"),
                                   list("Event" = "Evt07", "Cycle" = 5, "Rationale" = "interrupted cycles"),
                                   list("Event" = "Evt07", "Cycle" = 11, "Rationale" = "end of sequence, incomplete cycle"),
                                   list("Event" = "Evt08", "Cycle" = 4, "Rationale" = "interrupted cycles"),
                                   list("Event" = "Evt08", "Cycle" = 10, "Rationale" = "end of sequence, incomplete cycle"),
                                   list("Event" = "Evt09", "Cycle" = 1, "Rationale" = "end of sequence, incomplete cycle"),
                                   list("Event" = "Evt10", "Cycle" = 1, "Rationale" = "interrupted cycles"),
                                   list("Event" = "Evt10", "Cycle" = 2, "Rationale" = "end of sequence, incomplete cycle"),
                                   list("Event" = "Evt12", "Cycle" = 2, "Rationale" = "end of sequence, incomplete cycle"),
                                   list("Event" = "Evt15", "Cycle" = 3, "Rationale" = "end of sequence, incomplete cycle"),
                                   list("Event" = "Evt17", "Cycle" = 4, "Rationale" = "end of sequence, incomplete cycle"),
                                   list("Event" = "Evt19", "Cycle" = 10, "Rationale" = "end of sequence, incomplete cycle"),
                                   list("Event" = "Evt20", "Cycle" = 9, "Rationale" = "end of sequence, incomplete cycle")))
          

kiki <- list(cranium_filepath = "data/20151201_Kiki/Feeding_Kiki_20151201_Cranium_20180320.csv",
             mandible_filepath = "data/20151201_Kiki/Feeding_Kiki_20151201_Mandible_20180320.csv",
             mandible_pitch_correction = TRUE,
             working_side = "left",
             cycles_to_exclude = list(list("Event" = "Evt69", "Cycle" = 5, "Rationale" = "end of sequence, incomplete cycle"),
                                      list("Event" = "Evt70", "Cycle" = 4, "Rationale" = "end of sequence, incomplete cycle"),
                                      list("Event" = "Evt71", "Cycle" = 7, "Rationale" = "end of sequence, incomplete cycle"),
                                      list("Event" = "Evt73", "Cycle" = 12, "Rationale" = "interrupted cycles"),
                                      list("Event" = "Evt73", "Cycle" = 14, "Rationale" = "flat cycle"),
                                      list("Event" = "Evt73", "Cycle" = 15, "Rationale" = "end of sequence, incomplete cycle"),
                                      list("Event" = "Evt74", "Cycle" = 8, "Rationale" = "interrupted cycles"),
                                      list("Event" = "Evt74", "Cycle" = 11, "Rationale" = "end of sequence, incomplete cycle"),
                                      list("Event" = "Evt76", "Cycle" = 1, "Rationale" = "interrupted cycles"),
                                      list("Event" = "Evt76", "Cycle" = 2, "Rationale" = "end of sequence, incomplete cycle"),
                                      list("Event" = "Evt77", "Cycle" = 1, "Rationale" = "end of sequence, incomplete cycle"),
                                      list("Event" = "Evt80", "Cycle" = 1, "Rationale" = "interrupted cycles"),
                                      list("Event" = "Evt80", "Cycle" = 2, "Rationale" = "interrupted cycles"),
                                      list("Event" = "Evt80", "Cycle" = 3, "Rationale" = "end of sequence, incomplete cycle"),
                                      list("Event" = "Evt81", "Cycle" = 1, "Rationale" = "end of sequence, incomplete cycle"),
                                      list("Event" = "Evt82", "Cycle" = 1, "Rationale" = "end of sequence, incomplete cycle"),
                                      list("Event" = "Evt83", "Cycle" = 3, "Rationale" = "end of sequence, incomplete cycle")))


 
# Step 2: Run analysis----
print("analyzing chewbacca's data")
chewbacca_analysis <- chew_analysis(chewbacca)

print("analyzing hestia's data")
hestia_analysis <- chew_analysis(hestia)

print("analyzing jb's data")
jb_analysis <- chew_analysis(jb)

print("analyzing kiki's data")
kiki_analysis <- chew_analysis(kiki)


# Summary stats
chewbacca_summary <- summarize_cycles(chewbacca_analysis, "right")
hestia_summary <- summarize_cycles(hestia_analysis, "right")
jb_summary <- summarize_cycles(jb_analysis, "right")
kiki_summary <- summarize_cycles(kiki_analysis, "left")


#circular stats 
chewbacca_stats <- linear_circular_boxplot(chewbacca_analysis$cranium_CRS$times_to_minima_cyclic, "chewbacca")
hestia_stats <- linear_circular_boxplot(hestia_analysis$cranium_CRS$times_to_minima_cyclic, "hestia")
jb_stats <- linear_circular_boxplot(jb_analysis$cranium_CRS$times_to_minima_cyclic, "jb")
kiki_stats<- linear_circular_boxplot(kiki_analysis$cranium_CRS$times_to_minima_cyclic, "kiki")

# all_stats <- bind_rows(chewbacca_stats[[1]], hestia_stats[[1]], jb_stats[[1]], kiki_stats[[1]]) %>% 
#   mutate(variable = as.factor(variable))
