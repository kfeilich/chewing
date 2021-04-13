# ANALYSIS FUNCTIONS ----

# label_phases() ----
#' Labels gape cycle phases
#' 
label_phases <- function(event){
  
  event$GapeCyclePhase <- NA  # Initialize empty column
  
  # Label Max Gape as Courtney's cycle start time
  event$GapeCyclePhase[which(diff(event$Cycle) == 1) + 1] <- "MaxGape"
  
  # Label Min Gape as minimum mandible pitch for each cycle
  for (i in unique(event$Cycle)){
    cycle_df <- filter(event, Cycle == i)
    min_gape_frame <- cycle_df$XROMMframe[which.max(cycle_df$mandiblePitch)]
    event$GapeCyclePhase[which(event$XROMMframe == min_gape_frame)] <- "MinGape"
  }
  
  # Only label remaining phases for chews!
  chews_only <- filter(event, CycleType == "Chew")
  
  for (i in unique(chews_only$Cycle)){
    cycle_df <- filter(chews_only, Cycle == i)
    
    # Find FC-SC: first trough in second derivative between max gape and min gape
    max_gape <- which(cycle_df$GapeCyclePhase == "MaxGape")
    if(length(max_gape) == 0){max_gape <- 1}
    min_gape <- which(cycle_df$GapeCyclePhase == "MinGape")
    fc_sc <- which.min(diff(diff(cycle_df$mandiblePitch[max_gape:min_gape]))) + 2
    fc_sc_frame <- cycle_df$XROMMframe[fc_sc]
    
    # Find SO-FO: first trough in second derivative after min gape
    so_fo <- which.min(diff(diff(cycle_df$mandiblePitch[min_gape:nrow(cycle_df)]))) + 2 + min_gape
    so_fo_frame <- cycle_df$XROMMframe[so_fo]
    
    # Define reference frames
    min_gape_frame <- cycle_df$XROMMframe[min_gape]
    max_gape_frame <- cycle_df$XROMMframe[max_gape]
    last_cycle_frame <- cycle_df$XROMMframe[nrow(cycle_df)]
    
    # Label in between transitions
    event$GapeCyclePhase[which(event$XROMMframe == fc_sc_frame)] <- "FC-SC"
    event$GapeCyclePhase[which(event$XROMMframe == so_fo_frame)] <- "SO-FO"
    event$GapeCyclePhase[which(event$XROMMframe > max_gape_frame & event$XROMMframe < fc_sc_frame)] <- "FC"
    event$GapeCyclePhase[which(event$XROMMframe > fc_sc_frame & event$XROMMframe < min_gape_frame)] <- "SC"
    event$GapeCyclePhase[which(event$XROMMframe > min_gape_frame & event$XROMMframe < so_fo_frame)] <- "SO"
    event$GapeCyclePhase[which(event$XROMMframe > so_fo_frame & event$XROMMframe <= last_cycle_frame)] <- "FO"

  }
  return(event)
}


# number_from_min_gape() ----
#' Numbers gape cycles from min-gape to min-gape
number_from_min_gape <- function(event){
  event$Cycle_fromMinGape <- NA
  min_gapes <- filter(event, GapeCyclePhase == "MinGape")
  for (i in c(1:nrow(min_gapes))){
    start_frame <- min_gapes$XROMMframe[i]
    if (i < nrow(min_gapes)){
    end_frame <- min_gapes$XROMMframe[i + 1]}
    else{
      end_frame <- max(event$XROMMframe)
    }
    event$Cycle_fromMinGape[which(event$XROMMframe >= start_frame & event$XROMMframe < end_frame)] <- i
  }
  event
}


# type_from_min_gape() ----
#' labels gape cycle type from min-gape to min-gape
type_from_min_gape <- function(event){
  
  
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  event$CycleType_fromMinGape <- NA
  for (i in unique(event$Cycle_fromMinGape)){
    cycle_SC <- filter(event, Cycle_fromMinGape == i & GapeCyclePhase == "SC")
    cycle_swallow <- filter(event, Cycle_fromMinGape == i) %>% 
      pull(CycleType) %>%
      getmode
    
    cycle_swallow <- switch(cycle_swallow, "Swallow" = TRUE, FALSE)

    if(nrow(cycle_SC) > 0){
      event$CycleType_fromMinGape[which(event$Cycle_fromMinGape == i)] <- "Chew"
    } else if(cycle_swallow == TRUE){ 
    event$CycleType_fromMinGape[which(event$Cycle_fromMinGape == i)] <- "Swallow"
    }
    }
  return(event)
}

# get_resting_position() ----
#' averages low movement frames
get_resting_position <- function(interpolated_data){
  kinematics_columns <- names(interpolated_data)[str_detect(names(interpolated_data), 
  pattern = "^[A-z]+[._]*[A-z]*[._]*[A-z]*_[0-9]+Hz_[XYZ]$")]
  rotation_columns <- c("MiddleHorizontalRot", "MiddleVerticalRot", "PostHorizontalRot", "PostVerticalRot")
  distance_columns <- c("LeftPosteriorToMiddle" , "RightPosteriorToMiddle", 
                        "SurfacePosteriorToMiddle", "DeepPosteriorToMiddle", "LeftMiddleToAnterior",
                        "RightMiddleToAnterior", "SurfaceMiddleToAnterior", "DeepMiddleToAnterior",
                        "MiddleLeftToRight", "MiddleSurfaceToDeep", "PosteriorLeftToRight", "PosteriorSurfaceToDeep")
  mandible_columns <- c("mandiblePitch", "mandibleYaw")
  tongue_columns <- c("TongueAngle")
  
  resting_position <- interpolated_data %>% 
    filter(ScaledTime_fromMinGape == 0) %>%
    dplyr::select(all_of(c(kinematics_columns, rotation_columns, distance_columns, mandible_columns, tongue_columns, "ScaledTime_fromMinGape", "Cycle_fromMinGape", "Event"))) 
  
  resting_position
  
}

subtract_resting_position <- function(data_object){
  tongue_anterior_tip <- names(data_object$interpolated_data)[which(str_detect(names(data_object$interpolated_data), "^[Tt]ongue[.]*[Aa]nterior_[0-9]+Hz_X$"))]
  
  for (event in unique(data_object$interpolated_data$Event)){
    for (cycle in unique(data_object$interpolated_data$Cycle_fromMinGape[which(data_object$interpolated_data$Event == event)])){
      indices <- which(data_object$interpolated_data$Cycle_fromMinGape == cycle & data_object$interpolated_data$Event == event)
      
      rest_position <- data_object$resting_position[which(data_object$resting_position$Cycle_fromMinGape == cycle  & data_object$resting_position$Event == event),]
      
      data_object$interpolated_data$MiddleHorizontalRot[indices] = data_object$interpolated_data$MiddleHorizontalRot[indices] - rest_position$MiddleHorizontalRot
      data_object$interpolated_data$MiddleVerticalRot[indices] = data_object$interpolated_data$MiddleVerticalRot[indices] - rest_position$MiddleVerticalRot
      data_object$interpolated_data$PostHorizontalRot[indices] = data_object$interpolated_data$PostHorizontalRot[indices] - rest_position$PostHorizontalRot
      data_object$interpolated_data$PostVerticalRot[indices] = data_object$interpolated_data$PostVerticalRot[indices] - rest_position$PostVerticalRot
      data_object$interpolated_data$LeftPosteriorToMiddle[indices] = data_object$interpolated_data$LeftPosteriorToMiddle[indices] / rest_position$LeftPosteriorToMiddle 
      data_object$interpolated_data$RightPosteriorToMiddle[indices] = data_object$interpolated_data$RightPosteriorToMiddle[indices] / rest_position$RightPosteriorToMiddle 
      data_object$interpolated_data$SurfacePosteriorToMiddle[indices] = data_object$interpolated_data$SurfacePosteriorToMiddle[indices] / rest_position$SurfacePosteriorToMiddle 
      data_object$interpolated_data$DeepPosteriorToMiddle[indices] = data_object$interpolated_data$DeepPosteriorToMiddle[indices] / rest_position$DeepPosteriorToMiddle 
      data_object$interpolated_data$LeftMiddleToAnterior[indices] = data_object$interpolated_data$LeftMiddleToAnterior[indices] / rest_position$LeftMiddleToAnterior
      data_object$interpolated_data$RightMiddleToAnterior[indices] = data_object$interpolated_data$RightMiddleToAnterior[indices] / rest_position$RightMiddleToAnterior
      data_object$interpolated_data$SurfaceMiddleToAnterior[indices] = data_object$interpolated_data$SurfaceMiddleToAnterior[indices] / rest_position$SurfaceMiddleToAnterior
      data_object$interpolated_data$DeepMiddleToAnterior[indices] = data_object$interpolated_data$DeepMiddleToAnterior[indices] / rest_position$DeepMiddleToAnterior
      data_object$interpolated_data$MiddleLeftToRight[indices] = data_object$interpolated_data$MiddleLeftToRight[indices] / rest_position$MiddleLeftToRight
      data_object$interpolated_data$MiddleSurfaceToDeep[indices] = data_object$interpolated_data$MiddleSurfaceToDeep[indices] / rest_position$MiddleSurfaceToDeep
      data_object$interpolated_data$PosteriorLeftToRight[indices] = data_object$interpolated_data$PosteriorLeftToRight[indices] / rest_position$PosteriorLeftToRight
      data_object$interpolated_data$PosteriorSurfaceToDeep[indices] = data_object$interpolated_data$PosteriorSurfaceToDeep[indices] / rest_position$PosteriorSurfaceToDeep
      data_object$interpolated_data$mandiblePitch[indices] = data_object$interpolated_data$mandiblePitch[indices] - rest_position$mandiblePitch
      data_object$interpolated_data$mandibleYaw[indices] = data_object$interpolated_data$mandibleYaw[indices] - rest_position$mandibleYaw
      data_object$interpolated_data[,tongue_anterior_tip][indices] = data_object$interpolated_data[,tongue_anterior_tip][indices] - rest_position[,tongue_anterior_tip]
      data_object$interpolated_data$TongueAngle[indices] = data_object$interpolated_data$TongueAngle[indices] - rest_position$TongueAngle
     }}
  data_object
}


# replace_origin_coordinates()
replace_origin_coordinates <- function(data_object){
  kinematics_columns <- names(data_object$trial_data)[str_detect(names(data_object$trial_data), 
                                                     pattern = "^[A-z]+[._]*[A-z]*[._]*[A-z]*_[0-9]+Hz_[XYZ]$")]
  kinematics <- data_object$trial_data[,kinematics_columns]
  kinematics[kinematics == 0] <- NA
  data_object$trial_data[,kinematics_columns] <- kinematics
  data_object
}

# calculate_rotations 
calculate_rotations <- function(trial_data){
  find_angle_to_z <- function(pt1_z, pt1_y, pt2_z, pt2_y){
    deltaZ <- pt2_z - pt1_z
    deltaY <- pt2_y - pt1_y
    out <- rep(NA, length.out = length(pt1_z))
    for (i in 1:length(deltaZ)){
      if(sum(is.na(c(deltaY[i], deltaZ[i]))) > 0){
        out[i] <- NA
      } else {
        option_a <- atan2(deltaY[i], deltaZ[i]) * 180 / pi
        option_b <- option_a + 360
        option_c <- option_a - 360
        
        options <- c(option_a, option_b, option_c)
        out[i] <- options[which.min(abs(options))]
      }
    }
    out
  }
  
  find_angle_to_y <- function(pt1_z, pt1_y, pt2_z, pt2_y){
    
    out <- rep(NA, length.out = length(pt1_z))
    for (i in 1:length(pt1_z)){
      if(sum(is.na(c(pt1_z[i], pt1_y[i], pt2_z[i], pt2_y[i]))) > 0){
        out[i] <- NA
      } else {
        option_a <- find_angle_to_z(pt1_z[i], pt1_y[i], pt2_z[i], pt2_y[i]) - 90
        option_b <- option_a + 360
        option_c <- option_a - 360
        
        options <- c(option_a, option_b, option_c)
        out[i] <- options[which.min(abs(options))]
      }}
    out
  }
  
  middle_left_y <- names(trial_data)[str_detect(names(trial_data), 
                                                pattern = "^[Tt]ongue[.]*[Ll]eft[.]*[Ll]ateral_[0-9]+Hz_Y$")]
  middle_left_z <- names(trial_data)[str_detect(names(trial_data), 
                                                pattern = "^[Tt]ongue[.]*[Ll]eft[.]*[Ll]ateral_[0-9]+Hz_Z$")]
  middle_right_y <- names(trial_data)[str_detect(names(trial_data),
                                                 pattern = "^[Tt]ongue[.]*[Rr]ight[.]*[Ll]ateral_[0-9]+Hz_Y$")]
  middle_right_z <- names(trial_data)[str_detect(names(trial_data),
                                                 pattern = "^[Tt]ongue[.]*[Rr]ight[.]*[Ll]ateral_[0-9]+Hz_Z$")]
  middle_surface_y <- names(trial_data)[str_detect(names(trial_data),
                                                  pattern = "^[Tt]ongue[.]*[Mm]iddle[.]*[Ss]urface_[0-9]+Hz_Y$")]
  middle_surface_z <- names(trial_data)[str_detect(names(trial_data),
                                                   pattern = "^[Tt]ongue[.]*[Mm]iddle[.]*[Ss]urface_[0-9]+Hz_Z$")]
  middle_deep_y <- names(trial_data)[str_detect(names(trial_data),
                                                pattern = "^[Tt]ongue[.]*[Mm]iddle[.]*[Dd]eep_[0-9]+Hz_Y$")]
  middle_deep_z <- names(trial_data)[str_detect(names(trial_data),
                                                pattern = "^[Tt]ongue[.]*[Mm]iddle[.]*[Dd]eep_[0-9]+Hz_Z$")]
  
  post_left_y <- names(trial_data)[str_detect(names(trial_data), 
                                                pattern = "^[Tt]ongue[.]*[PG]*[.]*[Ll]eft+[PG]*_[0-9]+Hz_Y$")]
  post_left_z <- names(trial_data)[str_detect(names(trial_data), 
                                                pattern = "^[Tt]ongue[.]*[PG]*[.]*[Ll]eft+[PG]*_[0-9]+Hz_Z$")]
  post_right_y <- names(trial_data)[str_detect(names(trial_data),
                                               pattern = "^[Tt]ongue[.]*[PG]*[.]*[Rr]ight+[PG]*_[0-9]+Hz_Y$")]
  post_right_z <- names(trial_data)[str_detect(names(trial_data),
                                               pattern = "^[Tt]ongue[.]*[PG]*[.]*[Rr]ight+[PG]*_[0-9]+Hz_Z$")]
  post_surface_y <- names(trial_data)[str_detect(names(trial_data),
                                                 pattern = "^[Tt]ongue[.]*[Pp]osterior[.]*[Ss]urface_[0-9]+Hz_Y$")]
  post_surface_z <- names(trial_data)[str_detect(names(trial_data),
                                                 pattern = "^[Tt]ongue[.]*[Pp]osterior[.]*[Ss]urface_[0-9]+Hz_Z$")]
  post_deep_y <- names(trial_data)[str_detect(names(trial_data),
                                              pattern = "[Hh]yoid[.]*[Mm]iddle_[0-9]+Hz_Y$")]
  post_deep_z <- names(trial_data)[str_detect(names(trial_data),
                                             pattern = "[Hh]yoid[.]*[Mm]iddle_[0-9]+Hz_Z$")]
  
  # NB: Negative to correct for axis of rotation polarity
  trial_data <- trial_data %>% 
    mutate(MiddleHorizontalRot = -find_angle_to_z(.data[[middle_left_z]], .data[[middle_left_y]],
                                                 .data[[middle_right_z]], .data[[middle_right_y]]),
           MiddleVerticalRot = -find_angle_to_y(.data[[middle_deep_z]], .data[[middle_deep_y]],
                                               .data[[middle_surface_z]], .data[[middle_surface_y]]),
           PostHorizontalRot = -find_angle_to_z(.data[[post_left_z]], .data[[post_left_y]],
                                               .data[[post_right_z]], .data[[post_right_y]]),
           PostVerticalRot = -find_angle_to_y(.data[[post_deep_z]], .data[[post_deep_y]],
                                             .data[[post_surface_z]], .data[[post_surface_y]]))
  trial_data
  
}

# calculate_tongue_angle
calculate_tongue_angle <- function(trial_data){
  hyoid_middle_marker_x <- names(trial_data)[str_detect(names(trial_data), pattern = "[Hh]yoid[.]*[Mm]iddle_[0-9]+Hz_X$")]

  hyoid_middle_marker_y <- names(trial_data)[str_detect(names(trial_data), pattern = "[Hh]yoid[.]*[Mm]iddle_[0-9]+Hz_Y$")]
  hyoid_middle_marker_z <- names(trial_data)[str_detect(names(trial_data), pattern = "[Hh]yoid[.]*[Mm]iddle_[0-9]+Hz_Z$")]
  middle_superficial_marker_x <- names(trial_data)[str_detect(names(trial_data),
                                                              pattern = "^[Tt]ongue[.]*[Mm]iddle[.]*[Ss]urface_[0-9]+Hz_X$")]
  middle_superficial_marker_y <- names(trial_data)[str_detect(names(trial_data),
                                                              pattern = "^[Tt]ongue[.]*[Mm]iddle[.]*[Ss]urface_[0-9]+Hz_Y$")]
  middle_superficial_marker_z <- names(trial_data)[str_detect(names(trial_data),
                                                              pattern = "^[Tt]ongue[.]*[Mm]iddle[.]*[Ss]urface_[0-9]+Hz_Z$")]
  anterior_marker_x <- names(trial_data)[str_detect(names(trial_data),
                                                    pattern = "^[Tt]ongue[.]*[Aa]nterior_[0-9]+Hz_X$")]
  anterior_marker_y <- names(trial_data)[str_detect(names(trial_data),
                                                    pattern = "^[Tt]ongue[.]*[Aa]nterior_[0-9]+Hz_Y$")]
  anterior_marker_z <- names(trial_data)[str_detect(names(trial_data),
                                                    pattern = "^[Tt]ongue[.]*[Aa]nterior_[0-9]+Hz_Z$")]
  #BA = middle to hyoid
  vector_BA_x <- pull(trial_data, .data[[hyoid_middle_marker_x]]) - pull(trial_data, .data[[middle_superficial_marker_x]])
  vector_BA_y <- pull(trial_data, .data[[hyoid_middle_marker_y]]) - pull(trial_data, .data[[middle_superficial_marker_y]])
  vector_BA_z <- pull(trial_data, .data[[hyoid_middle_marker_z]]) - pull(trial_data, .data[[middle_superficial_marker_z]])
  
  #BC = middle to anterior
  vector_BC_x <- pull(trial_data, .data[[anterior_marker_x]]) - pull(trial_data, .data[[middle_superficial_marker_x]])
  vector_BC_y <- pull(trial_data, .data[[anterior_marker_y]]) - pull(trial_data, .data[[middle_superficial_marker_y]])
  vector_BC_z <- pull(trial_data, .data[[anterior_marker_z]]) - pull(trial_data, .data[[middle_superficial_marker_z]])
  
  dot_product_BA_BC <- (vector_BA_x * vector_BC_x) + (vector_BA_y * vector_BC_y)+ (vector_BA_z*vector_BC_z)
  length_BA <- sqrt(vector_BA_x**2 + vector_BA_y**2 + vector_BA_z**2)
  length_BC <- sqrt(vector_BC_x**2 + vector_BC_y**2 + vector_BC_z**2)
  
  angle <- acos(dot_product_BA_BC / (length_BA * length_BC)) * 180 / pi
  trial_data$TongueAngle <- angle
  trial_data
  }

# calculate_distances
calculate_distances <- function(trial_data){
  anterior_x <- names(trial_data)[str_detect(names(trial_data),
                                            pattern = "^[Tt]ongue[.]*[Aa]nterior_[0-9]+Hz_X$")]
  anterior_y <- names(trial_data)[str_detect(names(trial_data),
                                             pattern = "^[Tt]ongue[.]*[Aa]nterior_[0-9]+Hz_Y$")]
  anterior_z <- names(trial_data)[str_detect(names(trial_data),
                                             pattern = "^[Tt]ongue[.]*[Aa]nterior_[0-9]+Hz_Z$")]
  middle_left_x <- names(trial_data)[str_detect(names(trial_data), 
                                                pattern = "^[Tt]ongue[.]*[Ll]eft[.]*[Ll]ateral_[0-9]+Hz_X$")]
  middle_left_y <- names(trial_data)[str_detect(names(trial_data), 
                                                pattern = "^[Tt]ongue[.]*[Ll]eft[.]*[Ll]ateral_[0-9]+Hz_Y$")]
  middle_left_z <- names(trial_data)[str_detect(names(trial_data), 
                                                pattern = "^[Tt]ongue[.]*[Ll]eft[.]*[Ll]ateral_[0-9]+Hz_Z$")]
  middle_right_x <- names(trial_data)[str_detect(names(trial_data),
                                                 pattern = "^[Tt]ongue[.]*[Rr]ight[.]*[Ll]ateral_[0-9]+Hz_X$")]
  middle_right_y <- names(trial_data)[str_detect(names(trial_data),
                                                 pattern = "^[Tt]ongue[.]*[Rr]ight[.]*[Ll]ateral_[0-9]+Hz_Y$")]
  middle_right_z <- names(trial_data)[str_detect(names(trial_data),
                                                 pattern = "^[Tt]ongue[.]*[Rr]ight[.]*[Ll]ateral_[0-9]+Hz_Z$")]
  middle_surface_x <- names(trial_data)[str_detect(names(trial_data),
                                                   pattern = "^[Tt]ongue[.]*[Mm]iddle[.]*[Ss]urface_[0-9]+Hz_X$")]
  middle_surface_y <- names(trial_data)[str_detect(names(trial_data),
                                                   pattern = "^[Tt]ongue[.]*[Mm]iddle[.]*[Ss]urface_[0-9]+Hz_Y$")]
  middle_surface_z <- names(trial_data)[str_detect(names(trial_data),
                                                   pattern = "^[Tt]ongue[.]*[Mm]iddle[.]*[Ss]urface_[0-9]+Hz_Z$")]
  middle_deep_x <- names(trial_data)[str_detect(names(trial_data),
                                                pattern = "^[Tt]ongue[.]*[Mm]iddle[.]*[Dd]eep_[0-9]+Hz_X$")]
  middle_deep_y <- names(trial_data)[str_detect(names(trial_data),
                                                pattern = "^[Tt]ongue[.]*[Mm]iddle[.]*[Dd]eep_[0-9]+Hz_Y$")]
  middle_deep_z <- names(trial_data)[str_detect(names(trial_data),
                                                pattern = "^[Tt]ongue[.]*[Mm]iddle[.]*[Dd]eep_[0-9]+Hz_Z$")]
  post_left_x <- names(trial_data)[str_detect(names(trial_data), 
                                              pattern = "^[Tt]ongue[.]*[PG]*[.]*[Ll]eft+[PG]*_[0-9]+Hz_X$")]
  post_left_y <- names(trial_data)[str_detect(names(trial_data), 
                                              pattern = "^[Tt]ongue[.]*[PG]*[.]*[Ll]eft+[PG]*_[0-9]+Hz_Y$")]
  post_left_z <- names(trial_data)[str_detect(names(trial_data), 
                                              pattern = "^[Tt]ongue[.]*[PG]*[.]*[Ll]eft+[PG]*_[0-9]+Hz_Z$")]
  post_right_x <- names(trial_data)[str_detect(names(trial_data),
                                               pattern = "^[Tt]ongue[.]*[PG]*[.]*[Rr]ight+[PG]*_[0-9]+Hz_X$")]
  post_right_y <- names(trial_data)[str_detect(names(trial_data),
                                               pattern = "^[Tt]ongue[.]*[PG]*[.]*[Rr]ight+[PG]*_[0-9]+Hz_Y$")]
  post_right_z <- names(trial_data)[str_detect(names(trial_data),
                                               pattern = "^[Tt]ongue[.]*[PG]*[.]*[Rr]ight+[PG]*_[0-9]+Hz_Z$")]
  post_surface_x <- names(trial_data)[str_detect(names(trial_data),
                                                 pattern = "^[Tt]ongue[.]*[Pp]osterior[.]*[Ss]urface_[0-9]+Hz_X$")]
  post_surface_y <- names(trial_data)[str_detect(names(trial_data),
                                                 pattern = "^[Tt]ongue[.]*[Pp]osterior[.]*[Ss]urface_[0-9]+Hz_Y$")]
  post_surface_z <- names(trial_data)[str_detect(names(trial_data),
                                                 pattern = "^[Tt]ongue[.]*[Pp]osterior[.]*[Ss]urface_[0-9]+Hz_Z$")]
  post_deep_x <- names(trial_data)[str_detect(names(trial_data),
                                              pattern = "[Tt]ongue[.]*[Pp]osterior[.]*[Dd]eep_[0-9]+Hz_X$")]
  post_deep_y <- names(trial_data)[str_detect(names(trial_data),
                                              pattern = "[Tt]ongue[.]*[Pp]osterior[.]*[Dd]eep_[0-9]+Hz_Y$")]
  post_deep_z <- names(trial_data)[str_detect(names(trial_data),
                                              pattern = "[Tt]ongue[.]*[Pp]osterior[.]*[Dd]eep_[0-9]+Hz_Z$")]
  
  trial_data <- trial_data %>%
    mutate(LeftPosteriorToMiddle = sqrt((.data[[middle_left_x]] - .data[[post_left_x]])**2 + 
                                             (.data[[middle_left_y]] - .data[[post_left_y]])**2 +
                                             (.data[[middle_left_z]] - .data[[post_left_z]])**2),
           RightPosteriorToMiddle = sqrt((.data[[middle_right_x]] - .data[[post_right_x]])**2 + 
                                              (.data[[middle_right_y]] - .data[[post_right_y]])**2 +
                                              (.data[[middle_right_z]] - .data[[post_right_z]])**2),
           SurfacePosteriorToMiddle = sqrt((.data[[middle_surface_x]] - .data[[post_surface_x]])**2 + 
                                                (.data[[middle_surface_y]] - .data[[post_surface_y]])**2 +
                                                (.data[[middle_surface_z]] - .data[[post_surface_z]])**2),
           DeepPosteriorToMiddle = sqrt((.data[[middle_deep_x]] - .data[[post_deep_x]])**2 + 
                                             (.data[[middle_deep_y]] - .data[[post_deep_y]])**2 +
                                             (.data[[middle_deep_z]] - .data[[post_deep_z]])**2),
           LeftMiddleToAnterior = sqrt((.data[[middle_left_x]] - .data[[anterior_x]])**2 + 
                                            (.data[[middle_left_y]] - .data[[anterior_y]])**2 +
                                            (.data[[middle_left_z]] - .data[[anterior_z]])**2),
           RightMiddleToAnterior = sqrt((.data[[middle_right_x]] - .data[[anterior_x]])**2 + 
                                             (.data[[middle_right_y]] - .data[[anterior_y]])**2 +
                                             (.data[[middle_right_z]] - .data[[anterior_z]])**2),
           SurfaceMiddleToAnterior = sqrt((.data[[middle_surface_x]] - .data[[anterior_x]])**2 + 
                                               (.data[[middle_surface_y]] - .data[[anterior_y]])**2 +
                                               (.data[[middle_surface_z]] - .data[[anterior_z]])**2),
           DeepMiddleToAnterior = sqrt((.data[[middle_deep_x]] - .data[[anterior_x]])**2 + 
                                            (.data[[middle_deep_y]] - .data[[anterior_y]])**2 +
                                            (.data[[middle_deep_z]] - .data[[anterior_z]])**2), 
           MiddleLeftToRight = sqrt((.data[[middle_left_z]] - .data[[middle_right_z]])**2 +
                                         (.data[[middle_left_y]] - .data[[middle_right_y]])**2),
           MiddleSurfaceToDeep = sqrt((.data[[middle_surface_z]] - .data[[middle_deep_z]])**2 +
                                           (.data[[middle_surface_y]] - .data[[middle_deep_y]])**2),
           PosteriorLeftToRight = sqrt((.data[[post_left_z]] - .data[[post_right_z]])**2 +
                                            (.data[[post_left_y]] - .data[[post_right_y]])**2),
           PosteriorSurfaceToDeep = sqrt((.data[[post_surface_z]] - .data[[post_deep_z]])**2 +
                                              (.data[[post_surface_y]] - .data[[post_deep_y]])**2))
  trial_data
  
}


# scale_time
scale_time <- function(event){
  event$ScaledTime_fromMaxGape <- NA
  event$ScaledTime_fromMinGape <- NA
  for (i in c(1:max(event$Cycle, na.rm = TRUE))) {
    indices <- which(event$Cycle == i)
    scaled_to_minimum <- event$Time[indices] - min(event$Time[indices], na.rm = TRUE)
    scaled_to_maximum <- scaled_to_minimum / max(scaled_to_minimum, na.rm = TRUE)
    event$ScaledTime_fromMaxGape[indices] <- scaled_to_maximum
  }
  
  if(is.infinite(max(event$Cycle_fromMinGape, na.rm=TRUE))){
    event$ScaledTime_fromMinGape <- NA
  } else{
  for (i in c(1:max(event$Cycle_fromMinGape, na.rm = TRUE))) {
    indices <- which(event$Cycle_fromMinGape == i)
    scaled_to_minimum <-
      event$Time[indices] - min(event$Time[indices], na.rm = TRUE)
    scaled_to_maximum <-
      scaled_to_minimum / max(scaled_to_minimum, na.rm = TRUE)
    event$ScaledTime_fromMinGape[indices] <- scaled_to_maximum
  }
  }
  event
}


# side_cycles
side_cycles <- function(event){
  event$CycleSide_fromMaxGape <- NA
  event$CycleSide_fromMinGape <- NA
  symphysis_yaw_column <- names(event)[str_detect(names(event), 
                                                  pattern = "[Ss]ymphysis_[0-9]+Hz_Z_velocity$" )]
  
  # For each cycle from Max Gape
  for (i in c(1:max(event$Cycle, na.rm = TRUE))){
    indices <- which(event$Cycle == i & event$CycleType == "Chew")
    min_gape_index <- which(event$Cycle == i & event$CycleType == "Chew" & event$GapeCyclePhase == "MinGape")
    min_gape_time <- event$Time[min_gape_index]
    
    # Pull out the last 50ms before min gape
    SC_indices <- which(event$Time < min_gape_time & event$Time > min_gape_time - 0.05 & event$Cycle == i)
      #If there is no slow close section, skip it
    if(length(SC_indices) == 0){
      next
    }
    
    # Determine direction of yaw (Symphysis z velocity)
    # Yaw is opposite the direction of chew. So, positive yaw = negative side chew.
    # Positive is to animal right. Negative yaw = positive chew = right chew. 
    sum_symphysis_yaw_velocity <- sum(event[SC_indices,symphysis_yaw_column])
    event$CycleSide_fromMaxGape[indices] <- -sign(sum_symphysis_yaw_velocity) #Negated because opposite the chew
  }
  
  # For each cycle from Min Gape
  if(!is.infinite(max(event$Cycle_fromMinGape, na.rm = TRUE))){
  for (i in c(1:max(event$Cycle_fromMinGape, na.rm = TRUE))){
    indices <- which(event$Cycle_fromMinGape == i & event$CycleType_fromMinGape == "Chew")
    
    min_gape_index <- max(which(event$Cycle_fromMinGape == i & event$GapeCyclePhase == "SC"), na.rm= TRUE)
    
    min_gape_time <- event$Time[min_gape_index]

    # Pull out the last 50ms before min gape
    SC_indices <- which(event$Time <= min_gape_time & event$Time > min_gape_time - 0.05 & event$Cycle_fromMinGape== i)
    #If there is no slow close section, skip it
    if(length(SC_indices) == 0){
      next
    }
    
    # Determine direction of yaw (Symphysis z velocity)
    # Yaw is opposite the direction of chew. So, positive yaw = negative side chew.
    # Positive is to animal right. Negative yaw = positive chew = right chew. 
    sum_symphysis_yaw_velocity <- sum(event[SC_indices,symphysis_yaw_column])
    event$CycleSide_fromMinGape[indices] <- -sign(sum_symphysis_yaw_velocity) #Negated because opposite the chew
  }
  }
  event$CycleSide_fromMaxGape[which(event$CycleSide_fromMaxGape == 1)] <- "right"
  event$CycleSide_fromMaxGape[which(event$CycleSide_fromMaxGape == -1)] <- "left"
  event$CycleSide_fromMaxGape[which(event$CycleSide_fromMaxGape == 0)] <- "unknown"
  
  event$CycleSide_fromMinGape[which(event$CycleSide_fromMinGape == 1)] <- "right"
  event$CycleSide_fromMinGape[which(event$CycleSide_fromMinGape == -1)] <- "left"
  event$CycleSide_fromMinGape[which(event$CycleSide_fromMinGape == 0)] <- "unknown"
  event
  }


# interpolate_cycles 
interpolate_cycles <- function(list_of_events){
  
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  approx_flipped <- function(y, x) {
    if(not_isna(y)){
    return(approx(x, y, n = 101)$y)}
    else{
      return(rep_len(NA, 101))
    }
  }
  
  not_isna <- function(x) {
    sum(is.na(x)) < 2
  }
  
  make_NAs <- function(x){
    x
  }
  
  num_events <- length(list_of_events)
  kinematics_columns <- names(list_of_events[[1]])[str_detect(names(list_of_events[[1]]), 
                                                              pattern = "^[A-z]*[.]*[A-z]*[.]*[A-z]*_[0-9]+Hz_[XYZ]$")]
  emg_columns <- names(list_of_events[[1]])[str_detect(names(list_of_events[[1]]),
                                                       pattern = "^Ch[0-9]+")]
  mandible_columns <- c("mandiblePitch", "mandibleYaw")
  rotation_columns <- c("MiddleHorizontalRot", "MiddleVerticalRot", "PostHorizontalRot", "PostVerticalRot")
  distance_columns <- c("LeftPosteriorToMiddle" , "RightPosteriorToMiddle", 
                        "SurfacePosteriorToMiddle", "DeepPosteriorToMiddle", "LeftMiddleToAnterior",
                        "RightMiddleToAnterior", "SurfaceMiddleToAnterior", "DeepMiddleToAnterior",
                        "MiddleLeftToRight", "MiddleSurfaceToDeep", "PosteriorLeftToRight", "PosteriorSurfaceToDeep")
  angle_columns <- c("TongueAngle")
  
  # Initialize an empty list (which will later be filled and concatenated)
  events_cycles <- vector(mode = "list", length = num_events)
  
  
  # The following code takes a list of labeled event dataframes and 1) interpolates 
  # each cycle into 101 timepoints, 2) recombines this list into one bulk dataframe
  # including the interpolated data and the cycle and event labels. 
  
  # Set up iteration "For each event..." 
  for (i in 1:num_events){
    event <- list_of_events[[i]]  # Pull event data
    
    # If the event is not empty, break it into a list of dataframes where each represents
    # one cycle. 
    
    if (!is.na(event)){  
      event <- event # %>% 
      by_cycle <- event %>% group_by(Cycle_fromMinGape) %>% group_split
      
      # Set up iteration "For each cycle..."
      num_cycles <- length(by_cycle)
      cycles <- vector(mode = "list", length = num_cycles)
      for (j in 1:num_cycles){
        # If the cycle is not empty, interpolate the cycle into 101 data points
        if(!is.na(by_cycle[[j]]$Cycle_fromMinGape) & !is.na(by_cycle[[j]]$ScaledTime_fromMinGape)){
           out <- dplyr::select(by_cycle[[j]], all_of(c(kinematics_columns, emg_columns, rotation_columns, distance_columns, mandible_columns, angle_columns))) %>%
            map(approx_flipped, by_cycle[[j]]$ScaledTime_fromMinGape)
          out <- data.frame(do.call("cbind", out))  # Concatenate the list of interpolated columns into a dataframe
          
          out$ScaledTime_fromMinGape <- round(approx(by_cycle[[j]]$ScaledTime_fromMinGape, n = 101)$y, digits = 2)  # Calculate the timepoints
          out$Cycle_fromMinGape <- j  # Identify cycle in dataframe
          out$Event <- getmode(by_cycle[[j]]$Event)  # Identify event in dataframe 
          out$CycleType_fromMinGape <- getmode(by_cycle[[j]]$CycleType_fromMinGape)  # Define cycle type as the modal cycle type for that cycle
          out$CycleSide_fromMinGape <- getmode(by_cycle[[j]]$CycleSide_fromMinGape)  # Define side as the modal side
          cycles[[j]] <- out  # Enter each cycle's dataframe into the list of dataframes for that event
        } else {next}}  # Skip empty cycles
    } else{next}  # Skip empty events
    events_cycles[[i]] <- do.call("rbind", cycles)  # Concatenate the list of cycle dataframes into a dataframe of that event
  }
  
  # Concatenate the list of event dataframes into a dataframe of that individual
  events_cycles <- data.frame(do.call("rbind", events_cycles))
  events_cycles
}


chew_analysis <- function(animal_info){
  
  # Step 2: Import Data ----
  cranium_CRS_object <- list()
  mandible_CRS_object <- list()
  cranium_CRS_object$trial_data <- read_csv(animal_info$cranium_filepath)
  mandible_CRS_object$trial_data <- read_csv(animal_info$mandible_filepath)
  working_side <- animal_info$working_side
  
  if(animal_info$mandible_pitch_correction == TRUE){
    cranium_CRS_object$trial_data$mandiblePitch <- -cranium_CRS_object$trial_data$mandiblePitch
  }
  
  # Step 3: Replace (0,0,0) coordinates with NA
  cranium_CRS_object <- replace_origin_coordinates(cranium_CRS_object)
  mandible_CRS_object <- replace_origin_coordinates(mandible_CRS_object)
  
  # Step 4: Calculate rotations ----
  cranium_CRS_object$trial_data <- calculate_rotations(cranium_CRS_object$trial_data)
  mandible_CRS_object$trial_data <- calculate_rotations(mandible_CRS_object$trial_data)
  
  # Step 5: Calculate distances between points ----
  cranium_CRS_object$trial_data <- calculate_distances(cranium_CRS_object$trial_data)
  mandible_CRS_object$trial_data <- calculate_distances(mandible_CRS_object$trial_data)
  
  # Step 6: Calculate tongue angle ---
  cranium_CRS_object$trial_data <- calculate_tongue_angle(cranium_CRS_object$trial_data)
  mandible_CRS_object$trial_data <- calculate_tongue_angle(mandible_CRS_object$trial_data)
  
  # Step 7: Split events  ----
  cranium_CRS_object$list_of_events <- cranium_CRS_object$trial_data %>%
    group_by(Event) %>%
    group_split(.keep = TRUE)
  
  # Step 8: Define cycle phases from second derivative (mod. accuracy) ----
  cranium_CRS_object$list_of_events <- lapply(cranium_CRS_object$list_of_events, label_phases)
  
  # Step 9: Define Cycles From Min Gape ----
  cranium_CRS_object$list_of_events <- lapply(cranium_CRS_object$list_of_events, number_from_min_gape)
  
  # Step 10: Label Cycle Type from Min Gape ----
  cranium_CRS_object$list_of_events <- lapply(cranium_CRS_object$list_of_events, type_from_min_gape)
  
  # Step 11: Scale Time for each Cycle
  cranium_CRS_object$list_of_events <- lapply(cranium_CRS_object$list_of_events, scale_time)
  
  # Step 12: Side Cycle based on yaw during slow close
  cranium_CRS_object$list_of_events <- lapply(cranium_CRS_object$list_of_events, side_cycles)
  
  # Step 13: Interpolate each cycle to 100 points ----
  cranium_CRS_object$interpolated_data <- interpolate_cycles(cranium_CRS_object$list_of_events)

  # Step 14: Establish resting position
  cranium_CRS_object$resting_position <- get_resting_position(cranium_CRS_object$interpolated_data)
  
  # Step 15: Subtract resting position
  cranium_CRS_object<- subtract_resting_position(cranium_CRS_object) 
  
  # Step 16: Working vs balancing labels
  cranium_CRS_object <- relabel_sides(cranium_CRS_object, working_side)
  
  # Step 17: filter broken cycles
  cranium_CRS_object$cleaned_cycles <- filter_broken_cycles(cranium_CRS_object, animal_info)
  
  # Step 18: find times to minima 
  cranium_CRS_object$times_to_minima <- find_times_to_minima(cranium_CRS_object, animal_info$working_side)
  
  # Step 19: rescale min times to degrees
  cranium_CRS_object$times_to_minima_cyclic <- rescale_min_times(cranium_CRS_object$times_to_minima)
  
  # Step 20: find summary transition times
  cranium_CRS_object$summary_transition_times <- find_transition_times(cranium_CRS_object, animal_info)
  
  list("cranium_CRS" = cranium_CRS_object, "mandible_CRS" = mandible_CRS_object)
}


filter_broken_cycles <- function(cranium_CRS, animal_info){
  if (!is.null(animal_info$cycles_to_exclude)){
    for (i in c(1:length(animal_info$cycles_to_exclude))){
      cranium_CRS$interpolated_data <- filter(cranium_CRS$interpolated_data,
                                                              !(.data[["Event"]] == animal_info$cycles_to_exclude[[i]]$Event & .data[["Cycle_fromMinGape"]] == animal_info$cycles_to_exclude[[i]]$Cycle))
    } 
  } else {return(cranium_CRS$interpolated_data)}
  cranium_CRS$interpolated_data
}


summarize_cycles <- function(analysis_object, cycle_side = c('right', 'left')){
  mean_nona <- function(x){mean(x, na.rm = TRUE)}
  sd_nona <- function(x){sd(x, na.rm = TRUE)}
  
  summary_object <- analysis_object$cranium_CRS$cleaned_cycles %>% 
    filter(CycleType_fromMinGape == "Chew" & CycleSide_fromMinGape == cycle_side) %>%
    group_by(ScaledTime_fromMinGape) %>%
    summarize_if(is.numeric, list(mean = mean_nona, sd = sd_nona))
  
  summary_object
}


# PLOTTING FUNCTIONS
plot_rotations <- function(analysis_summary,
                           which_rot = c("middle", "posterior"), analysis_object,
                           limits){

  horz_rotation_to_plot <- switch(which_rot, "middle" = "WorkingMiddleHorizontalRot",
                                  "posterior" = "WorkingPosteriorHorizontalRot")
  vert_rotation_to_plot <- switch(which_rot, "middle" = "WorkingMiddleVerticalRot",
                                  "posterior" = "WorkingPosteriorVerticalRot")
  
  out_plot <- analysis_summary %>% 
    ggplot() +
    geom_rect(data = analysis_object$cranium_CRS$summary_transition_times,
              aes(xmin = (.data[['FC-SC_mean']] - .data[['FC-SC_se']]) *100,
                  xmax = (.data[['FC-SC_mean']] + .data[['FC-SC_se']]) *100,
                  ymin = -Inf, ymax = Inf), alpha = 0.1)+
    geom_rect(data = analysis_object$cranium_CRS$summary_transition_times,
              aes(xmin = (.data[['SO-FO_mean']] - .data[['SO-FO_se']]) *100,
                  xmax = (.data[['SO-FO_mean']] + .data[['SO-FO_se']]) *100,
                  ymin = -Inf, ymax = Inf), alpha = 0.1)+
    geom_vline(data = analysis_object$cranium_CRS$summary_transition_times,
               aes(xintercept = .data[['FC-SC_mean']] * 100), linetype = "dashed", col= "gray61")+
    geom_vline(data = analysis_object$cranium_CRS$summary_transition_times,
               aes(xintercept = .data[['SO-FO_mean']] * 100), linetype = "dashed", col= "gray61")+
    geom_line(aes(x = .data[["ScaledTime_fromMinGape"]] * 100, y = .data[[paste0(horz_rotation_to_plot,"_mean")]], col = "blue"), lwd = 2) +
    geom_ribbon(aes(x = .data[["ScaledTime_fromMinGape"]] * 100,
                    ymin = .data[[paste0(horz_rotation_to_plot,"_mean")]] - .data[[paste0(horz_rotation_to_plot,"_sd")]],
                    ymax = .data[[paste0(horz_rotation_to_plot,"_mean")]] + .data[[paste0(horz_rotation_to_plot,"_sd")]]),
                fill = "blue", alpha = 0.2)+
    geom_line(aes(x = ScaledTime_fromMinGape* 100, y = .data[[paste0(vert_rotation_to_plot, "_mean")]], col = "red"), lwd = 2) +
    geom_ribbon(aes(x = .data[["ScaledTime_fromMinGape"]] * 100,
                    ymin = .data[[paste0(vert_rotation_to_plot,"_mean")]] - .data[[paste0(vert_rotation_to_plot,"_sd")]],
                    ymax = .data[[paste0(vert_rotation_to_plot,"_mean")]] + .data[[paste0(vert_rotation_to_plot,"_sd")]]),
                fill = "red", alpha = 0.2)+
    xlab("Scaled Time")+
    ylab("Rotation (degrees)")+
    scale_x_continuous(limits = c(0,100), expand = c(0,0))+
    scale_y_continuous(limits = limits, expand = c(0.1,0.1))+
    scale_color_identity(name = "Rotation from",
                        breaks = c("blue", "red"),
                        labels = c("Horizontal", "Vertical"),
                        guide = "legend")+
    theme_classic()+
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = unit(c(1,1,1,1), "lines"))

  out_plot
}


plot_gape_pitch <- function(summary_object, analysis_object, limits){
  out_plot <- summary_object %>%
    ggplot() +
    geom_rect(data = analysis_object$cranium_CRS$summary_transition_times,
              aes(xmin = (.data[['FC-SC_mean']] - .data[['FC-SC_se']]) *100,
                  xmax = (.data[['FC-SC_mean']] + .data[['FC-SC_se']]) *100,
                  ymin = -Inf, ymax = Inf), alpha = 0.1)+
    geom_rect(data = analysis_object$cranium_CRS$summary_transition_times,
              aes(xmin = (.data[['SO-FO_mean']] - .data[['SO-FO_se']]) *100,
                  xmax = (.data[['SO-FO_mean']] + .data[['SO-FO_se']]) *100,
                  ymin = -Inf, ymax = Inf), alpha = 0.1)+
    geom_vline(data = analysis_object$cranium_CRS$summary_transition_times,
               aes(xintercept = .data[['FC-SC_mean']] * 100), linetype = "dashed", col= "gray61")+
    geom_vline(data = analysis_object$cranium_CRS$summary_transition_times,
               aes(xintercept = .data[['SO-FO_mean']] * 100), linetype = "dashed", col= "gray61")+
    geom_line(aes(x = ScaledTime_fromMinGape * 100, y = mandiblePitch_mean), col = "#B07AA1", lwd = 2) +
    geom_ribbon(aes(x = ScaledTime_fromMinGape* 100,
                      ymin = mandiblePitch_mean - mandiblePitch_sd,
                      ymax = mandiblePitch_mean + mandiblePitch_sd), fill = "#B07AA1", alpha = 0.3) +
    xlab("Scaled Time")+
    ylab("Pitch (degrees)")+
    scale_x_continuous(limits = c(0,100), expand = c(0,0))+
    scale_y_continuous(limits = limits, expand = c(0.05, 0.05))+
    theme_classic()+
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(),  plot.margin = unit(c(1,1,1,1), "lines"))
  
  out_plot
}

plot_tongue_tip <- function(summary_object, analysis_object, limits){
  tongue_column <- names(summary_object)[which(str_detect(names(summary_object), 
                                                          pattern = "^[Tt]ongue[.]*[Aa]nterior_[0-9]+Hz_X"))]
  out_plot <- summary_object %>%
    ggplot() +
    geom_rect(data = analysis_object$cranium_CRS$summary_transition_times,
              aes(xmin = (.data[['FC-SC_mean']] - .data[['FC-SC_se']]) *100,
                  xmax = (.data[['FC-SC_mean']] + .data[['FC-SC_se']]) *100,
                  ymin = -Inf, ymax = Inf), alpha = 0.1)+
    geom_rect(data = analysis_object$cranium_CRS$summary_transition_times,
              aes(xmin = (.data[['SO-FO_mean']] - .data[['SO-FO_se']]) *100,
                  xmax = (.data[['SO-FO_mean']] + .data[['SO-FO_se']]) *100,
                  ymin = -Inf, ymax = Inf), alpha = 0.1)+
    geom_vline(data = analysis_object$cranium_CRS$summary_transition_times,
               aes(xintercept = .data[['FC-SC_mean']] * 100), linetype = "dashed", col= "gray61")+
    geom_vline(data = analysis_object$cranium_CRS$summary_transition_times,
               aes(xintercept = .data[['SO-FO_mean']] * 100), linetype = "dashed", col= "gray61")+
    geom_line(aes(x = ScaledTime_fromMinGape * 100, y = .data[[tongue_column[1]]] *10), col = "#59a14f", lwd = 2)+
    geom_ribbon(aes(x = ScaledTime_fromMinGape * 100, 
                    ymin =(.data[[tongue_column[1]]] - .data[[tongue_column[2]]])*10,
                    ymax = (.data[[tongue_column[1]]] + .data[[tongue_column[2]]])*10),
                fill = "#59a14F", alpha = 0.3)+
    xlab("Scaled Time")+
    ylab("Position (mm)")+
    scale_x_continuous(limits = c(0,100), expand = c(0,0))+
    scale_y_continuous(limits = limits*10, expand = c(0.05, 0.05))+
    theme_classic()+
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(),  plot.margin = unit(c(1,1,1,1), "lines"))
  
  out_plot
}




plot_lr_midtoant_distance <- function(summary_object, analysis_object, limits){
  out_plot <- summary_object %>%
    ggplot() +
    geom_rect(data = analysis_object$cranium_CRS$summary_transition_times,
              aes(xmin = (.data[['FC-SC_mean']] - .data[['FC-SC_se']]) *100,
                  xmax = (.data[['FC-SC_mean']] + .data[['FC-SC_se']]) *100,
                  ymin = -Inf, ymax = Inf), alpha = 0.1)+
    geom_rect(data = analysis_object$cranium_CRS$summary_transition_times,
              aes(xmin = (.data[['SO-FO_mean']] - .data[['SO-FO_se']]) *100,
                  xmax = (.data[['SO-FO_mean']] + .data[['SO-FO_se']]) *100,
                  ymin = -Inf, ymax = Inf), alpha = 0.1)+
    geom_vline(data = analysis_object$cranium_CRS$summary_transition_times,
               aes(xintercept = .data[['FC-SC_mean']] * 100), linetype = "dashed", col= "gray61")+
    geom_vline(data = analysis_object$cranium_CRS$summary_transition_times,
               aes(xintercept = .data[['SO-FO_mean']] * 100), linetype = "dashed", col= "gray61")+
    geom_line(aes(x = ScaledTime_fromMinGape * 100, 
                   y = (WorkingAnteriorStrain_mean -1)*100, col = "#FF9DA7"), lwd = 2) + 
    geom_ribbon(aes(x = ScaledTime_fromMinGape * 100, 
                      ymin = (WorkingAnteriorStrain_mean -WorkingAnteriorStrain_sd -1) *100 , 
                      ymax = (WorkingAnteriorStrain_mean  + WorkingAnteriorStrain_sd-1)*100 ), alpha = 0.3, fill = "#FF9DA7")+
    geom_line(aes(x = ScaledTime_fromMinGape * 100, y = (BalancingAnteriorStrain_mean-1)*100, col = "#9C755F"), lwd = 2) + 
    geom_ribbon(aes(x = ScaledTime_fromMinGape * 100,
                      ymin = (BalancingAnteriorStrain_mean - BalancingAnteriorStrain_sd-1)*100 , 
                      ymax = (BalancingAnteriorStrain_mean  + BalancingAnteriorStrain_sd-1)*100 ), alpha = 0.3, fill = "#9C755F")+
    xlab("Scaled Time From Min Gape")+
    ylab("Length Change (mm)")+
    scale_x_continuous(limits = c(0,100), expand = c(0,0))+
    scale_y_continuous(limits = (limits-1)*100, expand = c(0.1, 0.1))+
    scale_color_identity(name = "Rotation from",
                         breaks = c("#FF9DA7", "#9C755F"),
                         labels = c("Working", "Balancing"),
                         guide = "legend")+
    theme_classic()+
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(),  plot.margin = unit(c(1,1,1,1), "lines"))
  
  out_plot
}


plot_lr_midtopost_distance <- function(summary_object, analysis_object, limits){
  out_plot <- summary_object %>%
    ggplot() +
    geom_rect(data = analysis_object$cranium_CRS$summary_transition_times,
              aes(xmin = (.data[['FC-SC_mean']] - .data[['FC-SC_se']]) *100,
                  xmax = (.data[['FC-SC_mean']] + .data[['FC-SC_se']]) *100,
                  ymin = -Inf, ymax = Inf), alpha = 0.1)+
    geom_rect(data = analysis_object$cranium_CRS$summary_transition_times,
              aes(xmin = (.data[['SO-FO_mean']] - .data[['SO-FO_se']]) *100,
                  xmax = (.data[['SO-FO_mean']] + .data[['SO-FO_se']]) *100,
                  ymin = -Inf, ymax = Inf), alpha = 0.1)+
    geom_vline(data = analysis_object$cranium_CRS$summary_transition_times,
               aes(xintercept = .data[['FC-SC_mean']] * 100), linetype = "dashed", col= "gray61")+
    geom_vline(data = analysis_object$cranium_CRS$summary_transition_times,
               aes(xintercept = .data[['SO-FO_mean']] * 100), linetype = "dashed", col= "gray61")+
    geom_line(aes(x = ScaledTime_fromMinGape*100, 
                   y = (WorkingPosteriorStrain_mean - 1)*100 , col = "#FF9DA7"), lwd = 2) + 
    geom_ribbon(aes(x = ScaledTime_fromMinGape * 100, 
                      ymin = (WorkingPosteriorStrain_mean - WorkingPosteriorStrain_sd - 1)*100 , 
                      ymax = (WorkingPosteriorStrain_mean + WorkingPosteriorStrain_sd-1)*100), fill = "#FF9DA7", alpha = 0.3)+
    geom_line(aes(x = ScaledTime_fromMinGape * 100, y = (BalancingPosteriorStrain_mean-1)*100, col = "#9C755F"), lwd = 2) + 
    geom_ribbon(aes(x = ScaledTime_fromMinGape* 100,
                      ymin = (BalancingPosteriorStrain_mean - BalancingPosteriorStrain_sd-1)*100, 
                      ymax = (BalancingPosteriorStrain_mean + BalancingPosteriorStrain_sd-1)*100), fill = "#9C755F", alpha = 0.3)+
    xlab("Scaled Time From Min Gape")+
    ylab("Strain (%)")+
    scale_x_continuous(limits = c(0,100), expand = c(0,0))+
    scale_y_continuous(limits = (limits - 1)*100, expand = c(0.1, 0.1))+
    scale_color_identity(name = "Rotation from",
                         breaks = c("#FF9DA7", "#9C755F"),
                         labels = c("Working", "Balancing"),
                         guide = "legend")+
    theme_classic()+
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = unit(c(1,1,1,1), "lines"))
  
  out_plot
}

plot_angle <- function(summary_object, analysis_object, limits) {
  out_plot <- summary_object %>%
    ggplot() + 
    geom_rect(data = analysis_object$cranium_CRS$summary_transition_times,
              aes(xmin = (.data[['FC-SC_mean']] - .data[['FC-SC_se']]) *100,
                  xmax = (.data[['FC-SC_mean']] + .data[['FC-SC_se']]) *100,
                  ymin = -Inf, ymax = Inf), alpha = 0.1)+
    geom_rect(data = analysis_object$cranium_CRS$summary_transition_times,
              aes(xmin = (.data[['SO-FO_mean']] - .data[['SO-FO_se']]) *100,
                  xmax = (.data[['SO-FO_mean']] + .data[['SO-FO_se']]) *100,
                  ymin = -Inf, ymax = Inf), alpha = 0.1)+
    geom_vline(data = analysis_object$cranium_CRS$summary_transition_times,
               aes(xintercept = .data[['FC-SC_mean']] * 100), linetype = "dashed", col= "gray61")+
    geom_vline(data = analysis_object$cranium_CRS$summary_transition_times,
               aes(xintercept = .data[['SO-FO_mean']] * 100), linetype = "dashed", col= "gray61")+
    geom_line(aes(x = ScaledTime_fromMinGape * 100, y = TongueAngle_mean), lwd = 2) +
    geom_ribbon(aes(x = ScaledTime_fromMinGape *100, 
                    ymin = TongueAngle_mean - TongueAngle_sd,
                    ymax = TongueAngle_mean + TongueAngle_sd), alpha = 0.3) +
    xlab("Scaled Time")+
    ylab("Angle (degrees)")+
    scale_x_continuous(limits = c(0,100), expand = c(0,0))+
    scale_y_continuous(limits = limits, expand = c(0.1, 0.1))+
    theme_classic()+
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = unit(c(1,1,1,1), "lines"))
  
  out_plot
}

find_limits<-function(col_pattern, ...){
  x <- list(...)
  max_vals <- vector(mode = "numeric", length = length(x))
  min_vals <- vector(mode = "numeric", length = length(x))
  for (j in 1:length(x)) {
    columns <- names(x[[j]])[str_detect(names(x[[j]]), col_pattern)]
    column_mean <- columns[str_detect(columns, "_mean")]
    column_sd <- columns[str_detect(columns, "_sd")]
    
    max_vals[j] <- max((x[[j]][,column_mean] + x[[j]][,column_sd]), na.rm = TRUE)
    min_vals[j] <- min((x[[j]][,column_mean] - x[[j]][,column_sd]), na.rm = TRUE)
  }
  c(min(min_vals), max(max_vals))
}

multicolumn_find_limits <- function(col_patterns, ...){
  x <- list(...)
  max_vals <- vector(mode = "numeric", length = length(x))
  min_vals <- vector(mode = "numeric", length = length(x))
  
  for (j in 1:length(x)) {
    columns_a <- names(x[[j]])[str_detect(names(x[[j]]), col_patterns[1])]
    column_a_mean <- columns_a[str_detect(columns_a, "_mean")]
    column_a_sd <- columns_a[str_detect(columns_a, "_sd")]
    
    columns_b <- names(x[[j]])[str_detect(names(x[[j]]), col_patterns[2])]
    column_b_mean <- columns_b[str_detect(columns_b, "_mean")]
    column_b_sd <- columns_b[str_detect(columns_b, "_sd")]
    
    max_vals_a <- max((x[[j]][,column_a_mean] + x[[j]][,column_a_sd]), na.rm = TRUE)
    max_vals_b <- max((x[[j]][,column_b_mean] + x[[j]][,column_b_sd]), na.rm = TRUE)
    
    min_vals_a <- min((x[[j]][,column_a_mean] - x[[j]][,column_a_sd]), na.rm = TRUE)
    min_vals_b <- min((x[[j]][,column_b_mean] - x[[j]][,column_b_sd]), na.rm = TRUE)
    
    max_vals[j] <- max(max_vals_a, max_vals_b)
    min_vals[j] <- min(min_vals_a, min_vals_b)
  }
  
  c(min(min_vals), max(max_vals))
}

compare_limits <- function(...){
  x <- list(...)
  x<- do.call(rbind, x)
  
  min_x<- min(x[,1])
  max_x <- max(x[,2])
    
  c(min_x, max_x)
}





find_times_to_minima <- function(cranium_CRS_object, side){
  bulk_find_time_to_minimum <- function(cranium_CRS_object, pattern, side){
    find_time_to_minimum<-function(cycle_df, pattern){
      column <- names(cycle_df)[str_detect(names(cycle_df), pattern )]
      if(sum(!is.na(pull(cycle_df, .data[[column]])) == 0)){
        time <- NA
      } else{
      time <- cycle_df$ScaledTime_fromMinGape[which.min(pull(cycle_df,.data[[column]]))]}
      time
    }
    if(side == "right"){
      cycle_list <- cranium_CRS_object$cleaned_cycles %>%
        filter(CycleSide_fromMinGape == "right" & CycleType_fromMinGape == "Chew") %>%
        group_by(Event, Cycle_fromMinGape) %>% 
        group_split()
    } else {
      cycle_list <- cranium_CRS_object$cleaned_cycles %>%
        filter(CycleSide_fromMinGape == "left" & CycleType_fromMinGape == "Chew") %>%
        group_by(Event, Cycle_fromMinGape) %>% 
        group_split()
    }
    
    times_list <- unlist(lapply(cycle_list, find_time_to_minimum, pattern))
    times_list
  }
  
  
   columns <- c("WorkingMiddleHorizontalRot", "WorkingMiddleVerticalRot", "WorkingPosteriorHorizontalRot",
               "WorkingPosteriorVerticalRot", "WorkingPosteriorStrain", "BalancingPosteriorStrain", 
               "WorkingAnteriorStrain", "BalancingAnteriorStrain", "mandiblePitch", 
               "TongueAngle")
  
  if(side == "right"){
  filtered_cycles  <- cranium_CRS_object$cleaned_cycles %>%
    filter(CycleType_fromMinGape == "Chew" & CycleSide_fromMinGape == "right")}
  else{
    filtered_cycles  <- cranium_CRS_object$cleaned_cycles %>%
      filter(CycleType_fromMinGape == "Chew" & CycleSide_fromMinGape == "left")}
  
  nadir_times_df <- unique(filtered_cycles[, c("Cycle_fromMinGape", "Event")])
  print(nrow(nadir_times_df))

  
  for (column in columns){
    nadir_times_df[, paste0(column, "_mintime")] <- bulk_find_time_to_minimum(cranium_CRS_object, column, side)
  }
  nadir_times_df
}


plot_times_to_minima <- function(analysis_object){
  plotting_df <- analysis_object$cranium_CRS$times_to_minima %>%
    pivot_longer(names(analysis_object$cranium_CRS$times_to_minima)[which(!(names(analysis_object$cranium_CRS$times_to_minima) %in% c("Cycle_fromMinGape", "Event")))]) 
  plotting_df$name <- factor(plotting_df$name, levels = c("TongueAngle_mintime",
                                                          "WorkingPosteriorVerticalRot_mintime", 
                                                          "WorkingPosteriorHorizontalRot_mintime",
                                                          "WorkingMiddleVerticalRot_mintime",
                                                          "WorkingMiddleHorizontalRot_mintime",
                                                          "WorkingPosteriorStrain_mintime",
                                                          "BalancingPosteriorStrain_mintime",
                                                          "WorkingAnteriorStrain_mintime",
                                                          "BalancingAnteriorStrain_mintime",
                                                          "mandiblePitch_mintime"))
  out_plot <- ggplot(plotting_df) +
    geom_boxplot(aes(x= value, y = name, fill=name))+
    xlab("ScaledTime")+
    ylab("Variable")+
    scale_y_discrete(labels = c("TongueAngle_mintime" = "Tongue Angle", "WorkingPosteriorVerticalRot_mintime" = "Posterior Vertical Rotation", 
                                "WorkingPosteriorHorizontalRot_mintime" = "Posterior Horizontal Rotation", "WorkingMiddleVerticalRot_mintime" = "Middle Vertical Rotation",
                                'WorkingMiddleHorizontalRot_mintime' = "Middle Horizontal Rotation", "WorkingPosteriorStrain_mintime" = "Working Posterior Strain", 
                                "BalancingPosteriorStrain_mintime" = "Balancing Posterior Strain", "WorkingAnteriorStrain_mintime" = "Working Anterior Strain",
                                "BalancingAnteriorStrain_mintime" = "Balancing Anterior Strain", "mandiblePitch_mintime" = "Mandible Pitch"))+
    scale_fill_manual(values = c("gray","red","blue","red","blue","#9C755F", "#FF9DA7", "#9C755F","#FF9DA7","#B07AA1"))+
    theme_classic() + 
    theme(legend.position = "none")
  out_plot
}

plot_cycles_of_variable <- function(analysis_object, variable, side){
  if (side == "right"){
  data_df <- analysis_object$cranium_CRS$cleaned_cycles %>%
    filter(CycleSide_fromMinGape == "right"& CycleType_fromMinGape == "Chew")
  } else{
    data_df <- analysis_object$cranium_CRS$cleaned_cycles %>%
      filter(CycleSide_fromMinGape == "left"& CycleType_fromMinGape == "Chew")
  }
  
  ggplot(data_df) + 
    geom_point(aes(x = ScaledTime_fromMinGape, y = .data[[variable]], color = as.factor(Cycle_fromMinGape), shape = Event))
}


relabel_sides <- function(cranium_CRS_object, working_side){
  if(working_side == "left"){
  cranium_CRS_object$interpolated_data <- cranium_CRS_object$interpolated_data %>%
    mutate(WorkingAnteriorStrain = LeftMiddleToAnterior,
           BalancingAnteriorStrain = RightMiddleToAnterior,
           WorkingPosteriorStrain = LeftPosteriorToMiddle,
           BalancingPosteriorStrain = RightPosteriorToMiddle,
           WorkingMiddleHorizontalRot = -MiddleHorizontalRot,
           WorkingMiddleVerticalRot = -MiddleVerticalRot,
           WorkingPosteriorHorizontalRot = -PostHorizontalRot,
           WorkingPosteriorVerticalRot = -PostVerticalRot)
  
  } else { cranium_CRS_object$interpolated_data <- cranium_CRS_object$interpolated_data %>%
    mutate(WorkingAnteriorStrain = RightMiddleToAnterior,
           BalancingAnteriorStrain = LeftMiddleToAnterior,
           WorkingPosteriorStrain = RightPosteriorToMiddle,
           BalancingPosteriorStrain = LeftPosteriorToMiddle,
           WorkingMiddleHorizontalRot = MiddleHorizontalRot,
           WorkingMiddleVerticalRot = MiddleVerticalRot,
           WorkingPosteriorHorizontalRot = PostHorizontalRot,
           WorkingPosteriorVerticalRot = PostVerticalRot)
  }
  cranium_CRS_object
}

rescale_min_times <- function(minima_df){
  rescale_time <- function(minima_col){
      ScaledToAngle <- minima_col * 360
  }
  
  ids <- minima_df[,c("Cycle_fromMinGape", "Event")]
  minima <- dplyr::select(minima_df, (names(minima_df)[which(!(names(minima_df) %in% c("Cycle_fromMinGape", "Event")))]))
  apply(minima, 2, rescale_time)
}


plot_circular_histogram<- function(colname, listofdfs){
  out_data <- list()
  
  for (i in c(1:length(listofdfs))){
    col_data <- listofdfs[[i]]$cranium_CRS$times_to_minima_cyclic[,colname]
    freqs <- data.frame(table(cut(col_data, breaks = c(seq(0,360,15)))))
    names(freqs)[2] <- names(listofdfs)[i]
    out_data[[i]]<- freqs
  }
  
  out_df <- out_data %>% reduce(left_join, by = "Var1")
  out_df <- out_df %>% 
    pivot_longer(names(listofdfs))
  out_df$IntervalStart <- as.numeric(str_match(out_df$Var1, "\\((.*?)\\,")[,2])
  
  ggplot(out_df, aes(x = IntervalStart, y= value, fill = name)) +
    geom_bar(stat = "identity") +
    coord_polar()+
    ylab("Count")+
    xlab("")+
    ggtitle(str_extract(colname,pattern = "^[:alpha:]+"))+
    scale_x_continuous(limits = c(0,360), breaks = c(0,90,180,270), labels = c("0%","25%", "50%", "75%")) + 
    scale_fill_viridis_d()+
    theme_minimal() + 
    theme(legend.position = "none", plot.margin = unit(c(0,0,0,0), "cm"), plot.title = element_text(size = 12, face = "bold"))
}

# 
# circular_summary_stats <- function(analysis_object){
#   df <- analysis_object$cranium_CRS$times_to_minima_cyclic
#   list_of_circular_data <- vector(mode = "list", length = ncol(df))
#   list_of_circular_stats <- vector(mode = "list", length = ncol(df))
#   
#   for (i in c(1:ncol(df))){
#     list_of_circular_data[[i]] <- circular(df[,i], type = "angles", units = "degrees")
#     names(list_of_circular_data)[i] <- names(df)[i]
#   
#     mean <- circular::mean.circular(list_of_circular_data[[i]], na.rm = TRUE)
#     median <- circular::median.circular(list_of_circular_data[[i]], na.rm = TRUE)
#     quantiles <- circular::quantile.circular(list_of_circular_data[[i]], probs = c(0.25, 0.75), na.rm = TRUE)
#     minimum <- 
#   }
# }

find_transition_times <- function(cranium_CRS_object, animal_info){
  cycles <- pull(cranium_CRS_object$cleaned_cycles, Cycle_fromMinGape)
  events <- pull(cranium_CRS_object$cleaned_cycles, Event)
  event_cycles <- unique(str_c(events, cycles, sep = "_"))
  event_cycles
  
  all_cycles<- bind_rows(cranium_CRS_object$list_of_events)
  all_cycles
  
  summary_data <- all_cycles %>% 
    filter(Event_Cycle %in% event_cycles) %>%
    filter(GapeCyclePhase %in% c("FC-SC", "SO-FO")) %>%
    pivot_wider(names_from = GapeCyclePhase,values_from = ScaledTime_fromMinGape) %>%
    filter(CycleSide_fromMinGape == animal_info$working_side, CycleType_fromMinGape == "Chew" ) %>%
    dplyr::select(c("FC-SC", "SO-FO")) %>%
    summarize_all(list(mean = function(x){mean(x, na.rm =TRUE)}, sd = function(x){sd(x, na.rm = TRUE)}, se = function(x){sd(x, na.rm= TRUE)/sqrt(n())}))
    
    summary_data
}
