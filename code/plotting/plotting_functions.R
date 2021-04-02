################################################################################
# Plotting functions for chewing data from Orsbon et al ____.                  #
# Author: Kara L. Feilich                                                      #
# Date created:                                                                #
# Date last edited:                                                            #
# Purpose: To facilitate plotting of gape cycles, gape phases, and EMG data    #
################################################################################

# plot_gape_cycle() #############################################

#' Plots the gape cycle
#' 
#' This plots the gape cycle and highlights the chews as grey rectangles. Note
#' the typical input y_var is "Symphysis_XHz_Y"
#' 
#' @param list_of_events (list) A list of event data as output by process_data_file()
#' @param event (integer) The event of interest. Acceptable values are 1:length(list_of_events)
#' @param y_var (string) The column name of the y variable to plot. Typically "Symphysis_XHz_Y"
#' @param y_label (string) What to label the plot y-axis
#' @param rects (Bool) TRUE = draw rectangle behind chews
#' @return A ggplot object
#' @seealso plot_variable_on_time()
plot_gape_cycles <-function(event, file_output, y1_var, y_label, rects = TRUE){
  
  #  Set up to optionally plot rectangles around chewing bouts
  if(rects == TRUE){
    # Needed for x bounds on rectangles
    chew_times <- find_chew_timepts(event, file_output$list_of_events)
    # Calculate y-bounds of rectangles
    max_y <- max(list_of_events[[event]][y1_var]) 
    min_y <- min(list_of_events[[event]][y1_var])
  }
  
  plot_out <- ggplot()  # Initiate the plot
  
  # If selected, plot chew rectangles beneath data
  if(rects == TRUE){
    plot_out <- plot_out + 
      geom_rect(data = chew_times, aes(xmin = as.numeric(StartTime), 
                                       xmax = as.numeric(EndTime),
                                       ymin = -Inf, ymax = Inf), 
                fill = "gray89")  # Arbitrary fill color
  }
  
  # Plot the selected y variable, with the line colored by cycle phase
  plot_out <- plot_out +
    geom_line(data = file_output$list_of_events_labeled[[event]],
              aes_string(x = "Time", y = y1_var, col = "CyclePhaseType", group = 1), lwd = 1.5) +
   xlab("Time (s)")+
    ylab(y_label)+
    theme_classic()+
    theme(legend.position = "top")
  
  plot_out
}

# plot_variable_on_time() #######################################
#' Plots a variable against time
#' 
#' @param list_of_events (list) A list of event data as output by process_data_file()
#' @param event (integer) The event of interest. Acceptable values are 1:length(list_of_events)
#' @param y_var (string) The column name of the y variable to plot
#' @param y_label (string) What to label the plot y-axis
#' @param xlabels (Bool) Whether to include x axis ticks and label
#' @return A ggplot object
#' @seealso plot_gape_cycle(), plot_variables_on_time()
plot_variable_on_time <- function(event, file_output, y_var, y_label, xlabels = TRUE){
  
  # Plot y variable as a black line
  plot_out <- ggplot(file_output$list_of_events[[event]],
                      aes_string(x = "Time", y = y_var))+
    geom_line(lwd = 1.5) + 
    xlab("Time (s)")+
    ylab(y_label)+
    theme_classic()
  
  # Optionally remove x axis ticks and label
  if(xlabels == FALSE){
    plot_out <- plot_out  + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  }
  
  plot_out
}

# plot_variables_on_time() #######################################
#' Plots variables of same scale against time
#' 
#' @param list_of_events (list) A list of event data as output by process_data_file()
#' @param event (integer) The event of interest. Acceptable values are 1:length(list_of_events)
#' @param y_vars (character) The column names of the y variables to plot
#' @param y_label (string) What to label the plot y-axis
#' @param legend (Bool) Whether to add a legend on the right hand side of the plot
#' @param xlabels (Bool) Whether to include x-axis ticks and labels (Useful for combining plots)
#' @return A ggplot object
#' @seealso plot_variable_on_time()
plot_variables_on_time <- function(event, file_output, y_vars, y_label, 
                                   legend = TRUE, xlabels = TRUE) {
  # Slim down the data to only the variables of interest
  data_input <- file_output$list_of_events[[event]] %>%  # For the selected event
    select(Time, all_of(y_vars)) %>%  # Choose only the desired variables
    pivot_longer(cols = all_of(y_vars)) %>%  # Make the variable name a factor
    mutate(name = factor(name, levels = y_vars)) 
  
  # Build the plot
  plot_out <- ggplot(data_input, aes(x = Time, y = value, color = name)) + 
    geom_line(lwd = 1.5) + 
    ylab(y_label) +
    theme_classic() # Gets rid of ugly ggplot default formatting
  
  # Optionally include legend
  if (legend == TRUE){
    plot_out <- plot_out + theme(legend.position = "right")
  } else {
    plot_out <- plot_out + theme(legend.position = "none")
  }
  
  # Optionally exclude x axis labels
  if(xlabels == FALSE){
    plot_out <- plot_out  + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  }
  
  plot_out
}
  

# crop_x_axis() #############################################
#' Crop the x axis of a ggplot object
#' 
#' This function allows you to highlight a specific domain on the x-axis, to 
#' better see certain timepoints. Specifically, it crops to a user-input chewing
#' series. 
#' @param input_plot (a ggplot) typically from plot_variable(s)_on_time()
#' @param timepoints_df (DataFrame) A chew_timepoints dataframe put out by process_data_file()
#' @param row_input (integer) Which row of the chew_timepoints dataframe to focus on
#' @return A ggplot object
crop_x_axis <- function(input_plot, timepoints_df, row_input){
  input_plot +
    coord_cartesian(xlim = as.numeric(c(timepoints_df$StartTime[row_input], timepoints_df$EndTime[row_input])))
}

# plot_second_deriv() #######################################
#' Plot the second derivative of a variable on top of that variable
#' 
#' This function will plot the variable of interest, colored by cycle phase, as
#' well as the second derivative of that variable. 
#' @param column (numeric) Vector of the y variable to differentiate
#' @param time_column (numeric) vector of the time column
#' @param phase_column (character) labels for phase of gape cycle
#' @param column_name (string) what to label the y axis
#' @param framerate (integer) Usually 200 Hz
#' @return a ggplot object
#'
plot_second_deriv <- function(column, time_column, phase_column, column_name, framerate){
  # Calculate the first and second derivative
  derivative_df <- calculate_derivative(column, time_column, phase_column,
                                        column_name, framerate)
  
  # Center the data so it lines up with the derivative data
  derivative_df$centered <- derivative_df[,column_name] - mean(derivative_df[,column_name])
  
  # Calculate a scale factor to use ggplot's infuriating second axis feature
  # which requires a 1:1 transform between y scales
  scale_factor = max(derivative_df$centered)/max(derivative_df[,paste0(column_name, "_d2")])
  derivative_df$d2_scaled <- derivative_df[,paste0(column_name, "_d2")] * scale_factor
  
  #Build the plot
  deriv_plot <- ggplot(derivative_df, aes(x = Time)) + 
     geom_line(aes(y = centered, col = CyclePhaseType, group = 1), lwd = 2)+
     geom_line(aes(y = d2_scaled), col = "red", lwd = 2) + 
     scale_y_continuous(name = column_name, sec.axis = sec_axis(~./scale_factor, name = "second deriv.")) +
     theme_classic() + 
     theme(
       axis.title.y.left=element_text(color="black"),
       axis.text.y.left=element_text(color="black"),
       axis.title.y.right=element_text(color="red"),
       axis.text.y.right=element_text(color="red")
     )
   
   deriv_plot
   
}


# plot_gape_cycles_baseR() #######################################
#' This function plots gape cycles as plot_gape_cycle, but using baseR graphics
#' 
#' @param event (integer) The number of the event to plot
#' @param file_output (list) The output of process_data_file() and process_event_data()
#' @param y1_var (character) The name of the column to color by phase
#' @param y2_var (character) The name of a second column to plot on top of y1
#' @param xlims (numeric) A vector (x1, x2) containing the xlimits of the plot
plot_gape_cycles_baseR <- function(event, file_output, y1_var, y2_var, xlims, rects = FALSE){
  
  # Build the color vector to "paint" the y1 variable
  phases <- c("NA", "MinGape", "SO", "SO-FO", "FO", "MaxGape", "FC", "FC-SC", "SC" )
  phase_colors <- c("black", "black", "#440154FF", "black", "#31688EFF", "black", "#35B779FF", "black", "#FDE725FF")
  names(phase_colors) <- phases
  
  # Make a dataframe of just the variables of interest and the color vector
  df <- file_output$list_of_events_labeled[[event]]
  x <- pull(df, Time)
  y1 <- pull(df, y1_var)
  y2 <- pull(df, y2_var)
  col_vector <- phase_colors[pull(df, CyclePhaseType)]

  # Build an empty plot
  plot(x, y1, xlab = "Time (s)",xlim = xlims, ylab = "Symphysis Y Position", type = "n")
  
  if(rects == TRUE){
    chews <- file_output$timepoints[[event]][which(file_output$timepoints[[event]]$CycleType == "Chew"),]
    if (nrow(chews) > 0){
     xlefts <- chews$StartTime
     xrights <- chews$EndTime
     ybottoms <- min(y1)
     ytops <- max(y1)
     
     rect(xleft = xlefts, ybottom = ybottoms, xright = xrights, ytop = ytops, 
          col = "lightgrey", lwd = 0)
    }
  }
  
  # Some fancy magic to plot the phase colors
  segments(x[-length(x)] , y1[-length(y1)], x[-1L], y1[-1L], col= col_vector, lwd = 3)
  # Plot the second axis
  par(new = TRUE)
  plot(x, y2, type = "l",xlim = xlims, lwd = 3, axes = FALSE, xlab = "", ylab = "")
  axis(side = 4, at = pretty(range(y2)))
  mtext("Symphysis Z Position", side = 4, line = 3)
  abline(h = 0, col = "grey")
  
}

# plot_emg_baseR() #######################################
#' This function plots a single EMG channel using baseR graphics
#' 
#' @param event (integer) The number of the event to plot
#' @param file_output (list) The output of process_data_file() and process_event_data()
#' @param y_var (character) The name of the column to plot
#' @param xlims (numeric) A vector (x1, x2) containing the xlimits of the plot
plot_emg_baseR <- function(event, file_output, y_var, xlims){
  df <- file_output$list_of_events_labeled[[event]]
  x <- pull(df, Time)
  y <- pull(df, y_var)
  
  plot(x, y, type = "l", xlab = "Time(s)", ylab = y_var, xlim = xlims, lwd = 3)
}

# plot_LR_emg_baseR() #######################################
#' This function plots gape cycles as plot_gape_cycle, but using baseR graphics
#' 
#' @param event (integer) The number of the event to plot
#' @param file_output (list) The output of process_data_file() and process_event_data()
#' @param left_var (character) The name of the column of the LEFT muscle's EMG
#' @param right_var (character) The name of the column of the RIGHT muscle's EMG
#' @param xlims (numeric) A vector (x1, x2) containing the xlimits of the plot
plot_LR_emg_baseR <- function(event, file_output, left_var, right_var, xlims){
  
  df <- file_output$list_of_events_labeled[[event]]
  x <- pull(df, Time)
  left <- pull(df, left_var)
  right <- pull(df, right_var)

  
  plot(x, left, xlab = "Time (s)",xlim = xlims, ylab = '', type = "l", col = "blue")
  mtext(left_var, side = 2, line = 3, col = "blue")
  lines(x, right, xlim = xlims, col = "red")
  mtext(right_var, side = 2, line = 2, col = "red")
  abline(h = 0, col = "grey")
  
  
}

# plot_variables_baseR() #######################################
#' This function plots two variables on the same x axis, but using baseR graphics
#' 
#' @param event (integer) The number of the event to plot
#' @param file_output (list) The output of process_data_file() and process_event_data()
#' @param yvar1 (character) The name of the column to color by phase
#' @param yvar2 (character) The name of a second column to plot on top of y1
#' @param xlims (numeric) A vector (x1, x2) containing the xlimits of the plot
plot_variables_baseR <- function(event, file_output, yvar1, yvar2, xlims){
  df <- file_output$list_of_events_labeled[[event]]
  x <- pull(df, Time)
  y1 <- pull(df, yvar1)
  y2 <- pull(df, yvar2)
 
  plot(x, y1, type = 'l', xlab = "Time (s)", xlim = xlims, ylab= '', col = 'black', lwd = 2)
  mtext(yvar1, side = 2, line = 2, col = "black")
  par(new = TRUE)
  plot(x, y2, type = 'l', axes = FALSE, col = "grey", xlab ='', ylab = '', lwd  = 2)
  axis(side = 4, at = pretty(range(y2)))      # Add second axis
  mtext(yvar2, side = 4, line = 3, col = "grey")

}
