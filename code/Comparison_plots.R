source("code/Chewbacca_20210310.R")
source("code/Hestia_20210310.R")
source("code/JB_20210310.R")
source("code/Kiki_20210310.R")

blank_panel <-ggplot(Hestia_right_chews) + geom_blank() + theme_classic()


# ROTATION PLOTS ----
##  Left Chews ---- 
### Left Chews, Middle Rotations ----
pdf("figures/LeftChews_MiddleRotations.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_left_chew_middle_rot_plot,
          JB_left_chew_middle_rot_plot, Kiki_left_chew_middle_rot_plot, ncol = 2)
dev.off()

### Left Chews, Post Rotations ----
pdf("figures/LeftChews_PosteriorRotations.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_left_chew_post_rot_plot,
          JB_left_chew_post_rot_plot, Kiki_left_chew_post_rot_plot, ncol = 2)
dev.off()

## Right Chews ----
### Right Chews, Middle Rotations ----
pdf("figures/RightChews_MiddleRotations.pdf", width = 9, height = 5)
plot_grid(Chewbacca_right_chew_middle_rot_plot, Hestia_right_chew_middle_rot_plot,
          JB_right_chew_middle_rot_plot, Kiki_right_chew_middle_rot_plot, ncol = 2)
dev.off()

### Right Chews, Post Rotations ----
pdf("figures/RightChews_PosteriorRotations.pdf", width = 9, height = 5)
plot_grid(Chewbacca_right_chew_post_rot_plot, Hestia_right_chew_post_rot_plot,
          JB_right_chew_post_rot_plot, Kiki_right_chew_post_rot_plot, ncol = 2)
dev.off()

## Swallows ----
### Swallows, Middle Rotations ----
pdf("figures/Swallows_MiddleRotations.pdf", width = 9, height = 5)
plot_grid(Chewbacca_swallow_middle_rot_plot, Hestia_swallow_middle_rot_plot,
          JB_swallow_middle_rot_plot, Kiki_swallow_middle_rot_plot, ncol = 2)
dev.off()

### Swallows, Post Rotations ----
pdf("figures/Swallows_PosteriorRotations.pdf", width = 9, height = 5)
plot_grid(Chewbacca_swallow_post_rot_plot, Hestia_swallow_post_rot_plot,
          JB_swallow_post_rot_plot, Kiki_swallow_post_rot_plot, ncol = 2)
dev.off()




# EMG PLOTS ----
## Right Chews ----
### Right Chews Genioglossus ----
#### Right Chews Right Genioglossus ----
pdf("figures/RightChews_RGG.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RGG_RChews, Hestia_RGG_RChews,
          JB_GG_RChews, NULL, ncol = 2)
dev.off()

#### Right Chews Left Genioglossus ----
pdf("figures/RightChews_LGG.pdf", width = 9, height = 5)
plot_grid(Chewbacca_LGG_RChews, Hestia_LGG_RChews,
          JB_GG_RChews, NULL, ncol = 2)
dev.off()

### Right Chews Geniohyoid ----
#### Right Chews Right Geniohyoid ----
pdf("figures/RightChews_RGH.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RGH_RChews, Hestia_RGH_RChews,
          JB_RGH_RChews, NULL, ncol = 2)
dev.off()

#### Right Chews Left Geniohyoid ----
pdf("figures/RightChews_LGH.pdf", width = 9, height = 5)
plot_grid(Chewbacca_LGH_RChews, Hestia_LGH_RChews,
          NULL, NULL, ncol = 2)
dev.off()
### Right Chews Hyoglossus ----
#### Right Chews Right Hyoglossus ----
pdf("figures/RightChews_RHG.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RHG_RChews, NULL,
          JB_RHG_RChews, NULL, ncol = 2)
dev.off()

#### Right Chews Left Hyoglosssus ----
pdf("figures/RightChews_LHG.pdf", width = 9, height = 5)
plot_grid(Chewbacca_LHG_RChews, NULL,
          NULL, NULL, ncol = 2)
dev.off()

### Right Chews Sternohyoid ----
#### Right Chews Right Sternohyoid ----
pdf("figures/RightChews_RSternoH.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RSternoH_RChews, Hestia_RSternoH_RChews,
          JB_SternoH_RChews, NULL, ncol = 2)
dev.off()

#### Right Chews Left Sternohyoid ----
pdf("figures/RightChews_LSternoH.pdf", width = 9, height = 5)
plot_grid(Chewbacca_LSternoH_RChews, Hestia_LSternoH_RChews,
          JB_SternoH_RChews, NULL, ncol = 2)
dev.off()

### Right Chews Ant. Digastric ----
#### Right Chews Right Ant. Digastric ----
pdf("figures/RightChews_RABD.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RABD_RChews, Hestia_RABD_RChews,
          JB_ABD_RChews, NULL, ncol = 2)
dev.off()

#### Right Chews Left Ant. Digastric ----
pdf("figures/RightChews_LABD.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_LABD_RChews,
          JB_ABD_RChews, NULL, ncol = 2)
dev.off()

### Right Chews  Post. Digastric ----
#### Right Chews Right Post. Digastric ----
pdf("figures/RightChews_RPBD.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RPBD_RChews, NULL,
          JB_PBD_RChews, NULL, ncol = 2)
dev.off()

#### Right Chews Left Post. Digastric ----
pdf("figures/RightChews_LPBD.pdf", width = 9, height = 5)
plot_grid(NULL, NULL,
          JB_PBD_RChews, NULL, ncol = 2)
dev.off()

### Right Chews Post Mylohyoid ----
#### Right Chews Right Post Mylohyoid ----
pdf("figures/RightChews_RPMH.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RPMH_RChews, Hestia_RMH_RChews,
          JB_RPMH_RChews, NULL, ncol = 2)
dev.off()

#### Right Chews Left Post Mylohyoid ----
pdf("figures/RightChews_LPMH.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_LMH_RChews,
          NULL, NULL, ncol = 2)
dev.off()

### Right Chews Sup. Masseter ----
#### Right Chews Right Sup. Masseter ----
pdf("figures/RightChews_RSM.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RSM_RChews, NULL,
          JB_RSM_RChews, NULL, ncol = 2)
dev.off()

#### Right Chews Left Sup. Masseter ---- 
pdf("figures/RightChews_LSM.pdf", width = 9, height = 5)
plot_grid(NULL, NULL,
          JB_LSM_RChews, NULL, ncol = 2)
dev.off()


## Left Chews ----
### Left Chews Genioglossus ----
#### Left Chews Right Genioglossus ----
pdf("figures/LeftChews_RGG.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_RGG_LChews,
          NULL, Kiki_RGG_LChews, ncol = 2)
dev.off()

#### Left Chews Left Genioglossus ----
pdf("figures/LeftChews_LGG.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_LGG_LChews, 
          NULL, NULL, ncol = 2)
dev.off()

### Left Chews Geniohyoid ----
#### Left Chews Right Geniohyoid ----
pdf("figures/LeftChews_RGH.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_RGH_LChews,
          NULL, Kiki_RGH_LChews, ncol = 2)
dev.off()

#### Left Chews Left Geniohyoid ----
pdf("figures/LeftChews_LGH.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_LGH_LChews,
          NULL, NULL, ncol = 2)
dev.off()

### Left Chews Hyoglossus ----
#### Left Chews Right Hyoglossus ----
pdf("figures/LeftChews_RHG.pdf", width = 9, height = 5)
plot_grid(NULL, NULL,
          NULL, NULL, ncol = 2)
dev.off()

#### Left Chews Left Hyoglosssus ----
pdf("figures/LeftChews_LHG.pdf", width = 9, height = 5)
plot_grid(NULL, NULL,
          NULL, NULL, ncol = 2)
dev.off()

### Left Chews Sternohyoid ----
#### Left Chews Right Sternohyoid ----
pdf("figures/LeftChews_RSternoH.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_RSternoH_LChews,
          NULL, Kiki_RSternoH_LChews, ncol = 2)
dev.off()

#### Left Chews Left Sternohyoid ----
pdf("figures/LeftChews_LSternoH.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_LSternoH_LChews,
          NULL, NULL, ncol = 2)
dev.off()

### Left Chews Ant. Digastric ----
#### Left Chews Right Ant. Digastric ----
pdf("figures/LeftChews_RABD.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_RABD_LChews,
          NULL, Kiki_RABD_LChews, ncol = 2)
dev.off()

#### Left Chews Left Ant. Digastric ----
pdf("figures/LeftChews_LABD.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_LABD_LChews,
          NULL, NULL, ncol = 2)
dev.off()

### Left Chews  Post. Digastric ----
#### Left Chews Right Post. Digastric ----
pdf("figures/LeftChews_RPBD.pdf", width = 9, height = 5)
plot_grid(NULL, NULL,
          NULL, NULL, ncol = 2)
dev.off()

#### Left Chews Left Post. Digastric ----
pdf("figures/LeftChews_LPBD.pdf", width = 9, height = 5)
plot_grid(NULL, NULL,
          NULL, NULL, ncol = 2)
dev.off()

### Left Chews Post Mylohyoid ----
#### Left Chews Right Post Mylohyoid ----
pdf("figures/LeftChews_RPMH.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_RMH_LChews,
          NULL, Kiki_RPMH_LChews, ncol = 2)
dev.off()

#### Left Chews Left Post Mylohyoid ----
pdf("figures/LeftChews_LPMH.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_LMH_LChews,
          NULL, NULL, ncol = 2)
dev.off()


### Left Chews Sup. Masseter ----
#### Left Chews Right Sup. Masseter ----
pdf("figures/LeftChews_RSM.pdf", width = 9, height = 5)
plot_grid(NULL, NULL,
          NULL, Kiki_RSM_LChews, ncol = 2)
dev.off()

#### Left Chews Left Sup. Masseter ---- 
pdf("figures/LeftChews_LSM.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_LSM_LChews,
          NULL, NULL, ncol = 2)
dev.off()





## Swallows ----
### Swallows Genioglossus ----
#### Swallows Right Genioglossus ----
pdf("figures/Swallows_RGG.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RGG_Swallows, Hestia_RGG_Swallows,
          JB_GG_Swallows, Kiki_RGG_Swallows, ncol = 2)
dev.off()

#### Swallows Left Genioglossus ----
pdf("figures/Swallows_LGG.pdf", width = 9, height = 5)
plot_grid(Chewbacca_LGG_Swallows, Hestia_LGG_Swallows,
          JB_GG_Swallows, NULL, ncol = 2)
dev.off()

### Swallows Geniohyoid ----
#### Right Chews Right Geniohyoid ----
pdf("figures/Swallows_RGH.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RGH_Swallows, Hestia_RGH_Swallows,
          JB_RGH_Swallows, Kiki_RGH_Swallows, ncol = 2)
dev.off()

#### Swallows Left Geniohyoid ----
pdf("figures/Swallows_LGH.pdf", width = 9, height = 5)
plot_grid(Chewbacca_LGH_Swallows, Hestia_LGH_Swallows,
          NULL, NULL, ncol = 2)
dev.off()
### Swallows Hyoglossus ----
#### Swallows Right Hyoglossus ----
pdf("figures/Swallows_RHG.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RHG_Swallows, NULL,
          JB_RHG_Swallows, NULL, ncol = 2)
dev.off()

#### Swallows Left Hyoglosssus ----
pdf("figures/Swallows_LHG.pdf", width = 9, height = 5)
plot_grid(Chewbacca_LHG_Swallows, NULL,
          NULL, NULL, ncol = 2)
dev.off()

### Swallows Sternohyoid ----
#### Swallows Right Sternohyoid ----
pdf("figures/Swallows_RSternoH.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RSternoH_Swallows, Hestia_RSternoH_Swallows,
          JB_SternoH_Swallows, Kiki_RSternoH_Swallows, ncol = 2)
dev.off()

#### Swallows Left Sternohyoid ----
pdf("figures/Swallows_LSternoH.pdf", width = 9, height = 5)
plot_grid(Chewbacca_LSternoH_Swallows, Hestia_LSternoH_Swallows,
          JB_SternoH_Swallows, NULL, ncol = 2)
dev.off()

### Swallows Ant. Digastric ----
#### Swallows Right Ant. Digastric ----
pdf("figures/Swallows_RABD.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RABD_Swallows, Hestia_RABD_Swallows,
          JB_ABD_Swallows, Kiki_RABD_Swallows, ncol = 2)
dev.off()

#### Swallows Left Ant. Digastric ----
pdf("figures/Swallows_LABD.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_LABD_Swallows,
          JB_ABD_Swallows, NULL, ncol = 2)
dev.off()

### Swallows Post. Digastric ----
#### Swallows Right Post. Digastric ----
pdf("figures/Swallows_RPBD.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RPBD_Swallows, NULL,
          JB_PBD_Swallows, NULL, ncol = 2)
dev.off()

#### Swallows Left Post. Digastric ----
pdf("figures/Swallows_LPBD.pdf", width = 9, height = 5)
plot_grid(NULL, NULL,
          JB_PBD_Swallows, NULL, ncol = 2)
dev.off()

### Swallows Post Mylohyoid ----
#### Swallows Right Post Mylohyoid ----
pdf("figures/Swallows_RPMH.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RPMH_Swallows, Hestia_RMH_Swallows,
          JB_RPMH_Swallows, Kiki_RPMH_Swallows, ncol = 2)
dev.off()

#### Swallows Left Post Mylohyoid ----
pdf("figures/Swallows_LPMH.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_LMH_Swallows,
          NULL, NULL, ncol = 2)
dev.off()

### Swallows Sup. Masseter ----
#### Swallows Right Sup. Masseter ----
pdf("figures/Swallows_RSM.pdf", width = 9, height = 5)
plot_grid(Chewbacca_RSM_Swallows, NULL,
          JB_RSM_Swallows, Kiki_RSM_Swallows, ncol = 2)
dev.off()

#### Swallows Left Sup. Masseter ---- 
pdf("figures/Swallows_LSM.pdf", width = 9, height = 5)
plot_grid(NULL, NULL,
          JB_LSM_Swallows, NULL, ncol = 2)
dev.off()


# DISTANCE PLOTS ----
##  Left Chews ---- 
### Left Chews, Posterior to Middle Distance ----
pdf("figures/LeftChews_PosteriortoMiddleDistance.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_posterior_to_middle_left_chews,
          JB_posterior_to_middle_left_chews, Kiki_posterior_to_middle_left_chews, ncol = 2)
dev.off()

### Left Chews, Middle to Anterior Distance ----
pdf("figures/LeftChews_MiddletoAnteriorDistance.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_middle_to_anterior_left_chews,
          JB_middle_to_anterior_left_chews, Kiki_middle_to_anterior_left_chews, ncol = 2)
dev.off()

## Right Chews ----
### Right Chews, Posterior to Middle Distance ----
pdf("figures/RightChews_PosteriortoMiddleDistance.pdf", width = 9, height = 5)
plot_grid(Chewbacca_posterior_to_middle_right_chews, Hestia_posterior_to_middle_right_chews,
          JB_posterior_to_middle_right_chews, Kiki_posterior_to_middle_right_chews, ncol = 2)
dev.off()

### Right Chews, Middle to Anterior Distance ----
pdf("figures/RightChews_MiddletoAnteriorDistance.pdf", width = 9, height = 5)
plot_grid(Chewbacca_middle_to_anterior_right_chews, Hestia_middle_to_anterior_right_chews,
          JB_middle_to_anterior_right_chews, Kiki_middle_to_anterior_right_chews, ncol = 2)
dev.off()

## Swallows ----
### Swallows, Posterior to Middle Distance ----
pdf("figures/Swallows_PosteriortoMiddleDistance.pdf", width = 9, height = 5)
plot_grid(Chewbacca_posterior_to_middle_swallows, Hestia_posterior_to_middle_swallows,
          JB_posterior_to_middle_swallows, Kiki_posterior_to_middle_swallows, ncol = 2)
dev.off()

### Swallows, Middle to Anterior Distance ----
pdf("figures/Swallows_MiddletoAnteriorDistance.pdf", width = 9, height = 5)
plot_grid(Chewbacca_middle_to_anterior_swallows, Hestia_middle_to_anterior_swallows,
          JB_middle_to_anterior_swallows, Kiki_middle_to_anterior_swallows, ncol = 2)
dev.off()

# SHAPE PLOTS ----
## Left Chews ----
pdf("figures/LeftChews_AspectRatios.pdf", width = 9, height = 5)
plot_grid(NULL, Hestia_AR_left_chews,
          JB_AR_left_chews, Kiki_AR_left_chews, ncol = 2)
 dev.off()

## Right chews ----
pdf("figures/RightChews_AspectRatios.pdf", width = 9, height = 5)
plot_grid(Chewbacca_AR_right_chews, Hestia_AR_right_chews,
          JB_AR_right_chews, Kiki_AR_right_chews, ncol = 2)
dev.off()

## Swallows ---- 
pdf("figures/Swallows_AspectRatios.pdf", width = 9, height = 5)
plot_grid(Chewbacca_AR_swallows, Hestia_AR_swallows,
          JB_AR_swallows, Kiki_AR_swallows, ncol = 2)
dev.off()

 

