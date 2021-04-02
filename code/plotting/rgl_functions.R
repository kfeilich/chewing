#' @param new.device a logical value. If TRUE, creates a new device
#' @param bg the background color of the device
#' @param width the width of the device
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}


# x, y, z : numeric vectors corresponding to
#  the coordinates of points
# axis.col : axis colors
# xlab, ylab, zlab: axis labels
# show.plane : add axis planes
# show.bbox : add the bounding box decoration
# bbox.col: the bounding box colors. The first color is the
# the background color; the second color is the color of tick marks
rgl_add_axes <- function(x_max, x_min, y_max, y_min, z_max, z_min,
                         xlab = "", ylab="", zlab="", show.plane = FALSE, 
                         show.bbox = FALSE, bbox.col = c("#333377","black"))
{ 
  
  lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
  x_extent <- max(c(abs(x_max), abs(x_min)))
  xlim <- c(-x_extent, x_extent) * 1.1
  
  y_extent <- max(c(abs(y_max), abs(y_min)))
  ylim <- c(-y_extent, y_extent) * 1.1
  
  z_extent <- max(c(abs(z_max), abs(z_min)))
  zlim <- c(-z_extent, z_extent) * 1.1
  
  # Add axes
  rgl.lines(xlim, c(0, 0), c(0, 0), color = "red")
  rgl.lines(c(0, 0), ylim, c(0, 0), color = "green")
  rgl.lines(c(0, 0), c(0, 0), zlim, color = "blue")
  
  # Add a point at the end of each axes to specify the direction
  axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0), 
                c(0, 0, zlim[2]))
  rgl.points(axes, color = c("red", "green", "blue"), size = 3)
  
  # Add axis labels
  rgl.texts(axes, text = c(xlab, ylab, zlab), color =  c("red", "green", "blue"),
            adj = c(0.5, -0.8), size = 2)
  
  # Add plane
  if(show.plane) {
    xlim <- xlim/1.1; zlim <- zlim /1.1
  rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
             z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
  }
  
  # Add bounding box decoration
  if(show.bbox){
    rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5, 
             emission=bbox.col[1], specular=bbox.col[1], shininess=5,           
             xlen = 3, ylen = 3, zlen = 3) 
  }
}

extract_xyz_df <- function(event_df, tongue_column_names){
  tongue_column_names <- tongue_column_names[str_detect(tongue_column_names, "_[A-Z]$")]
  
  tongue_X_cols <- c(tongue_column_names[str_detect(tongue_column_names, "_X$")])
  tongue_Y_cols <- c(tongue_column_names[str_detect(tongue_column_names, "_Y$")])
  tongue_Z_cols <- c(tongue_column_names[str_detect(tongue_column_names, "_Z$")])
  
  x_coordinates <- event_df %>%
    select(all_of(c(tongue_X_cols, "Time"))) %>%
    pivot_longer(cols = all_of(tongue_X_cols), names_to = "Marker", values_to = "X") %>%
    mutate(Marker = str_sub(Marker,end = -8))
  
  y_coordinates <- event_df %>%
    select(all_of(c(tongue_Y_cols, "Time"))) %>%
    pivot_longer(cols = all_of(tongue_Y_cols), names_to = "Marker", values_to = "Y") %>%
    mutate(Marker = str_sub(Marker,end = -8))
  
  z_coordinates <- event_df %>%
    select(all_of(c(tongue_Z_cols, "Time"))) %>%
    pivot_longer(cols = all_of(tongue_Z_cols), names_to = "Marker", values_to = "Z") %>%
    mutate(Marker = str_sub(Marker,end = -8))
  
  out <- x_coordinates %>%
    left_join(y_coordinates, by = c("Time", "Marker")) %>%
    left_join(z_coordinates, by = c("Time", "Marker"))
  
  x_max <- max(out$X, na.rm = TRUE)
  x_min <- min(out$X, na.rm = TRUE)
  y_max <- max(out$Y, na.rm = TRUE)
  y_min <- min(out$Y, na.rm = TRUE)
  z_max <- max(out$Z, na.rm = TRUE)
  z_min <- min(out$Z, na.rm = TRUE)
  
  
  out <- out %>%
    mutate(Time = as.factor(Time)) %>%
    group_by(Time) %>%
    group_split()
  
  list("coordinates" = out, "x_min" = x_min, "x_max" = x_max, 
       "y_min" = y_min, "y_max" = y_max,
       "z_min" = z_min, "z_max" = z_max)
}


plot_tongue_points <- function(frame, coordinate_list){
  rgl_init()
  view3d(theta= 85, phi = 15)
  rgl_add_axes(coordinate_list$x_max, coordinate_list$x_min, coordinate_list$y_max,
               coordinate_list$y_min, coordinate_list$z_max, coordinate_list$z_min)
  spheres3d(coordinate_list$coordinates[[frame]]$X,
            coordinate_list$coordinates[[frame]]$Y,
            coordinate_list$coordinates[[frame]]$Z, radius = 0.1, col = "black")
  view3d(theta= 285, phi = -15)
  rgl.snapshot(paste0(frame, ".png"),fmt = "png")
}


plot_tongue_and_hyoid_points <- function(frame, tongue_coordinate_list, hyoid_coordinate_list, symphysis_coordinate_list, user_matrix){
  rgl_init()
  view3d(userMatrix = user_matrix)
  
  x_max <- max(tongue_coordinate_list$x_max, hyoid_coordinate_list$x_max, symphysis_coordinate_list$x_max)
  x_min <- min(tongue_coordinate_list$x_min, hyoid_coordinate_list$x_min, symphysis_coordinate_list$x_min)
  y_max <- max(tongue_coordinate_list$y_max, hyoid_coordinate_list$y_max, symphysis_coordinate_list$y_max)
  y_min <- min(tongue_coordinate_list$y_min, hyoid_coordinate_list$y_min, symphysis_coordinate_list$y_min)
  z_max <- max(tongue_coordinate_list$z_max, hyoid_coordinate_list$z_max, symphysis_coordinate_list$z_max)
  z_min <- min(tongue_coordinate_list$z_min, hyoid_coordinate_list$z_min, symphysis_coordinate_list$z_min)
  
    rgl_add_axes(x_max, x_min,y_max, y_min, z_max, z_min)
  spheres3d(tongue_coordinate_list$coordinates[[frame]]$X,
            tongue_coordinate_list$coordinates[[frame]]$Y,
            tongue_coordinate_list$coordinates[[frame]]$Z, radius = 0.1, col = "black")
  spheres3d(hyoid_coordinate_list$coordinates[[frame]]$X,
            hyoid_coordinate_list$coordinates[[frame]]$Y,
            hyoid_coordinate_list$coordinates[[frame]]$Z, radius = 0.1, col = "red")
  spheres3d(symphysis_coordinate_list$coordinates[[frame]]$X,
            symphysis_coordinate_list$coordinates[[frame]]$Y,
            symphysis_coordinate_list$coordinates[[frame]]$Z, radius = 0.1, col = "blue")
  rgl.snapshot(paste0(frame, ".png"),fmt = "png")
}

Chewbacca_event1_chew <- filter(Chewbacca_cranium_crs_20170509$list_of_events_labeled[[1]], CycleType == "Chew")
Chewbacca_event1_chew_tongue_coords <- extract_xyz_df(Chewbacca_event1_chew, tongue_columns_Chewbacca)
Chewbacca_event1_chew_hyoid_coords <- extract_xyz_df(Chewbacca_event1_chew, hyoid_columns_Chewbacca)
Chewbacca_event1_chew_symphysis_coords <- extract_xyz_df(Chewbacca_event1_chew, symphysis_columns_Chewbacca)

map(c(1:length(Chewbacca_event1_chew_tongue_coords$coordinates)), plot_tongue_and_hyoid_points, Chewbacca_event1_chew_tongue_coords, Chewbacca_event1_chew_hyoid_coords, Chewbacca_event1_chew_symphysis_coords, sagittal)

