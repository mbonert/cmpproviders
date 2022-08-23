# -----------------------------------------------------------------------------------------------------------
# R function to create a funnel plot to compare (healthcare) providers
#
# Michael Bonert, BASc, MASC, MD, FRCPC ( michael@librepathology.org )
# Copyright (c) 2022
#
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General 
# Public License as published by the Free Software Foundation, either version 3 of the License, or (at your 
# option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even 
# the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public 
# License for more details.
#
# You should have received a copy of the GNU General Public License along with this program. If not, 
# see https://www.gnu.org/licenses/
# -----------------------------------------------------------------------------------------------------------

#' Creates a funnel plot with an arbitrary number of funnels
#' 
#' @description
#' Funnel plots to compare (healthcare) providers or institutions
#'
#' @details
#' A function to compare provider rate data.  The implementation is based on Bonert et al. 
#' (2017) https://doi.org/10.4103/jpi.jpi_50_17
#'
#' @param x_var                 x values on funnel plot, e.g. number of specimens
#' @param y_var                 y values on funnel plot, e.g. diagnostic rate (0-1)
#' @param labels                labels that describe the (x,y) data points, e.g. name of institution or provider identification; default: the data points are numbered (if undefined)
#' @param limits                limits (confidence intervals); default: 95\%, 99.9\%
#' @param funnel_centre_line    defines the funnel centre line; default: median of y_var
#' @param addlabels             whether to display labels on the funnel plot; accepted values are:
#' \itemize{
#' \item{\code{"none"}}{ : do not add labels (default)}
#' \item{\code{"all"}}{ : add labels to all points}
#' \item{\code{"outliers"}}{ : label only outliers outside of the inner funnels}
#' \item{\code{"outliers2"}}{ : label only outliers outside of second innermost funnel}
#' \item{\code{"only_oaf"}}{ : label only outliers outside of all funnels}
#' \item{\code{"list"}}{ : display the list of labels given by argument \code{labels_to_display}}
#' }
#' @param labels_to_display     selected labels to display (see argument \emph{addlabels})
#' @param x_label               x axis label
#' @param y_label               y axis label
#' @param plot_title            title of the (funnel) plot
#' @param x_lower_limit         lower x limit (min x value on plot)
#' @param x_upper_limit         upper x limit (max x value on plot)
#' @param y_lower_limit         lower y limit (min y value on plot)
#' @param y_upper_limit         upper y limit (max y value on plot)
#' @param x_fpad_upper          upper padding as fraction of x axis range
#' @param x_fpad_lower          lower padding as fraction of x axis range
#' @param y_fsd                 padding as fraction of standard deviation of y values 
#' @param funnel_curve_calculation_parameter    method used to calculate the funnel curves / determine the x values used to calculate the funnel curves; 1: uses geom_function to calculate/plot funnel curves, 2 = uses geom_line to plot funnel curves, 3: uses geom_line to plot funnel curves, calculates ideal x using \emph{get_ideal_x.r} (default)
#' @param fc_min_dx             funnel curve parameter, minimum delta x
#' @param fc_max_y_err          funnel curve parameter, maximum y error
#' @param x2plot                vector defines extra x values used to calculate the funnel curves
#' @param show_legend           TRUE shows legend at bottom of plot
#' @param y_percent             y axis in percent (multiplies y_var by 100)
#'
#' @returns funnel plot object
#'
#' @examples
#' library(COUNT)
#' library(dplyr)
#' data(medpar)
#'
#' print("Number of cases by provider ...")
#' provider_count=as.data.frame(table(medpar$provnum))
#' colnames(provider_count) = c('PROVIDER', 'COUNT')
#' provider_count
#' 
#' print("Purging providers with less than 20 cases ...")
#' requiredcases=20
#' medpar_trunc <- medpar[medpar$provnum %in% provider_count[provider_count$COUNT>=requiredcases,]$PROVIDER,]
#' 
#' print("Number of cases by provider in purged data set ...")
#' provider_count_trunc=as.data.frame(table(medpar_trunc$provnum))
#' colnames(provider_count_trunc) = c('PROVIDER', 'COUNT')
#' provider_count_trunc
#' 
#' medpar_trunc_died <- medpar_trunc[medpar_trunc$died==1,]
#' 
#' print("Number of died by provider in purged data set ...")
#' provider_count_trunc_died=as.data.frame(table(medpar_trunc_died$provnum))
#' colnames(provider_count_trunc_died) = c('PROVIDER', 'COUNT')
#' provider_count_trunc_died
#' 
#' print("Death rate by provider ...")
#' y_var = provider_count_trunc_died[,2]/ provider_count_trunc[,2]
#' y_var
#' 
#' x_var = provider_count_trunc[,2]
#' provider_labels = provider_count_trunc[,1]
#' 
#' print("Creating funnel plots ...")
#' fp1=funnel2cmpproviders(x_var, y_var, limits=c(95,99.9,99.9999), addlabels = 1, 
#'   x_label = "Provider \crVolume (Patients Cared for by Provider)", y_label = "Death Rate (Deaths by 
#'   Provider/Patients Cared for by Provider)", plot_title = "Death Rate by Provider and Volume") 
#' fp2=funnel2cmpproviders(x_var, y_var, limits=c(95,99.9,99.9999), labels=provider_labels, 
#'   addlabels = "OUTLIERS", x_label = "Provider Volume (Patients Cared for by Provider)", 
#'   y_label = "Provider Death Rate (Deaths by Provider/Patients Cared for by Provider)", 
#'   plot_title = "Death Rate by Provider and Volume with Outliers Labelled")
#'
#' @import ggplot2 
#' @import grid gridExtra
#' @importFrom ggrepel geom_label_repel
#'
#' @export funnel2cmpproviders

funnel2cmpproviders <- function(x_var, y_var, labels = NULL, limits = c(95,99.9), funnel_centre_line = NULL, addlabels = 'NONE', labels_to_display = NULL, x_label = NULL, y_label = NULL, plot_title = NULL, x_lower_limit = NULL, x_upper_limit = NULL, y_lower_limit = NULL,  y_upper_limit = NULL, x_fpad_upper = 0.1, x_fpad_lower = 0.03, y_fsd = 0.5, funnel_curve_calculation_parameter = 3, fc_min_dx = 0.5, fc_max_y_err = 0.003, x2plot = NULL, show_legend=TRUE, y_percent = FALSE ) {
  # Control
  debug=0
  showoutlier_arr=0 # output the outliers to screen
  
  # Check input data   
  xlen=length(x_var)
  ylen=length(y_var)
  if (xlen != ylen) {
    stop("ERROR 1: the input variables 'x_var' and 'y_var' are not equal in length!")
  }
  if (!is.null(labels)) {
    labelslen=length(labels)
  
    # Convert 'labels' into strings - to avoid type coercion when padded with NAs
    labels=paste(labels, sep="")

    if (labelslen != xlen) {
      stop("ERROR 2: the input variables 'x_var' and 'labels' are not equal in length!")      
    }
  } else {
    labels <- 1:xlen
  }
  if (debug==1) {
    print("labels:")
    print(labels) # testing
  }
  
  # Calculate funnel centre line
  if ( is.null(funnel_centre_line) ) {
    funnel_centre_line = median(y_var)  
  }  
  if (debug==1) {
    print("funnel_centre_line:")
    print(funnel_centre_line) # testing
  }

  # Create outliers array
  num_of_funnels=length(limits) # number of limits
  outliers_arr=matrix(0,ylen,num_of_funnels)
  colnames(outliers_arr) = limits
  rownames(outliers_arr) = labels    

  # Calculate the funnel limits  
  limits_arr=matrix(0,ylen,2*num_of_funnels)
  for (x_ctr in 1:length(x_var)) {
    for (ci_ctr in 1:length(limits)) {
      ci_kk=calc_fpc(limits[ci_ctr], funnel_centre_line, x_var[x_ctr])
      
      limits_arr[x_ctr,(2*(ci_ctr-1)+1)]=ci_kk[1]
      limits_arr[x_ctr,(2*(ci_ctr-1)+2)]=ci_kk[2]
      
      # check whether outside funnel (below)
      if (!is.na(ci_kk[1])) {
        if (y_var[x_ctr]<ci_kk[1]) {
          outliers_arr[x_ctr,ci_ctr]=-1
        }
      }  
      # check whether outside funnel (above)
      if (!is.na(ci_kk[2])) {
        if (y_var[x_ctr]>ci_kk[2]) {
         outliers_arr[x_ctr,ci_ctr]=1      
        }
      }
    }
  }
  len_outlierarr=ylen
  
  if (debug==1 || showoutlier_arr==1) {
    print("outliers_arr:")
    print(outliers_arr) # testing
  }    

  # Calculate plot limits
  if ( is.null(y_upper_limit) || is.null(y_lower_limit) ) {
    y_upper_limit=max(y_var)+sd(y_var)*y_fsd
    y_lower_limit=min(y_var)-sd(y_var)*y_fsd
  
    if (y_upper_limit>1) {
      y_upper_limit=1
    }
    if (y_lower_limit<0) {
      y_lower_limit=0
    }
  }
  if ( is.null(x_upper_limit) || is.null(x_lower_limit) ) {
    range_x=max(x_var)-min(x_var)
    x_upper_limit=max(x_var)+range_x*x_fpad_upper
    x_lower_limit=min(x_var)-range_x*x_fpad_lower
  }  
  
  if (debug==1) {
    print("plot limits")
    print("x_lower_limit:")
    print(x_lower_limit) 
    print("x_upper_limit:")
    print(x_upper_limit) 
    print("y_lower_limit:")
    print(y_lower_limit) 
    print("y_upper_limit:")
    print(y_upper_limit) 
  }    
  
  # Calculate the ideal x vector to get smooth funnel curves
  if ( funnel_curve_calculation_parameter>2 ) {
    # Add points to make funnels extend to the plot limits
  
    ideal_x = get_ideal_x(x_lower_limit, x_upper_limit, y_lower_limit, y_upper_limit, limits, funnel_centre_line, fc_min_dx, fc_max_y_err)
    
    extended_x_var=c(x_var, ideal_x)

    len_idealx=dim(data.frame(ideal_x))[1]
    someNAs2=matrix(NA,len_idealx)    
    extended_y_var=c(y_var, someNAs2)    
    
    x_var = extended_x_var
    y_var = extended_y_var
    xlen = dim(data.frame(x_var))[1]
    ylen = dim(data.frame(y_var))[1]
    labels=c(labels, someNAs2)
  }
  
  # Add points specified in 'x2plot'
  if ( !is.null(x2plot) ) {
    if (debug==1) {
      print("x2plot is NOT empty") # testing
    }
    
    extended_x_var=c(x_var, x2plot)
    len_x2plot=dim(data.frame(x2plot))[1]

    someNAs=matrix(NA,len_x2plot)
    extended_y_var=c(y_var,someNAs)
    
    x_var = extended_x_var
    y_var = extended_y_var
    xlen = dim(data.frame(x_var))[1]
    ylen = dim(data.frame(y_var))[1]
    labels=c(labels,someNAs)
  }

  if (debug==1) {
    print("labels (updated):")
    print(labels) # testing
  }  
  
  # Recalculate the funnel limits (if necessary)
  if ( !is.null(x2plot) || funnel_curve_calculation_parameter>2 ) {    
    limits_arr=matrix(0,ylen,2*num_of_funnels)
    
    for (x_ctr in 1:length(x_var)) {
      for (ci_ctr in 1:length(limits)) {
        ci_kk=calc_fpc(limits[ci_ctr], funnel_centre_line, x_var[x_ctr])
      
        limits_arr[x_ctr,(2*(ci_ctr-1)+1)]=ci_kk[1]
        limits_arr[x_ctr,(2*(ci_ctr-1)+2)]=ci_kk[2]
      }
    }
  }
  
  df_limits=data.frame(limits_arr)
  if (debug==1) {
    #print("df_limits:")
    print(df_limits) # testing
  }
  
  # Create 'outliers' variable to put into dataframe
  outliers=labels
  if (addlabels==2 || addlabels=='OUTLIERS' || addlabels=='outliers') {
    outliers[1:len_outlierarr]=outliers_arr[,1] # use innermost funnel
  } else if ( (addlabels=='OUTLIERS2' || addlabels=='outliers2') && num_of_funnels>1 ) {
    outliers[1:len_outlierarr]=outliers_arr[,2] # use second innermost funnel  
  } else {
    outliers[1:len_outlierarr]=outliers_arr[,num_of_funnels] # use outermost funnel   
  }
  
  # Create 'labels2display' variable to put into dataframe
  labels2display=matrix(NA, xlen) # initialize variable to put into dataframe
  if ( addlabels=='LIST' || addlabels=='list') {
    # modify 'labels2display'
    for (labels_ctr in 1:length(labels_to_display)) {
      i_index=which(labels==labels_to_display[labels_ctr])
      labels2display[i_index]=1
    }
  }
  
  # Create data frame
  if ( y_percent==TRUE && funnel_curve_calculation_parameter !=1 ) {
    y_var=y_var*100
    y_upper_limit=y_upper_limit*100
    y_lower_limit=y_lower_limit*100
    funnel_centre_line=funnel_centre_line*100
    df = data.frame(x_var,y_var,df_limits*100,labels,outliers,labels2display)  
  } else {
    df = data.frame(x_var,y_var,df_limits,labels,outliers,labels2display)
  }  
  
  if (debug==1) {
    print(df) # testing
  }

  # Create legend_labels
  legend_labels=matrix(0,num_of_funnels+2)
  colour4labels=matrix(0,num_of_funnels+2)
  size4labels=matrix(0,num_of_funnels+2)
  linetype4labels=matrix(0,num_of_funnels+2)
  for (ci_ctr in 1:length(limits)) {   
    if (limits[ci_ctr]>1) {
      legend_labels[ci_ctr]=paste(as.character(limits[ci_ctr]), "% CI")
    } else {
      legend_labels[ci_ctr]=paste("p =",as.character(limits[ci_ctr]))
    }
    
    if (ci_ctr == 1) {
      colour4labels[1]="darkorchid3"
      size4labels[1]=1.1
      linetype4labels[1]=5
    } else if (ci_ctr == 2) { 
      colour4labels[2]="green3"
      size4labels[2]=1.3
      linetype4labels[2]=1
      } else if (ci_ctr == 3) {
      colour4labels[3]="blue"
      size4labels[3]=0.8      
      linetype4labels[3]=5      
    } else {
      colour4labels[ci_ctr]="black"
      size4labels[4]=0.8    
      linetype4labels[4]=5     
    }
  }
  legend_labels[num_of_funnels+2]="Providers"
  colour4labels[num_of_funnels+2]="blue"
  legend_labels[num_of_funnels+1]="Funnel Centre Line"
  colour4labels[num_of_funnels+1]="black"
  
  if (debug==1) {
    print("legend_labels")
    print(legend_labels) # testing
    print("colour4labels")
    print(colour4labels) # testing
  }  

  # Plot the data points
  #   theme_bw() --> background black and white
  fp = ggplot(df,aes(x=x_var, y=y_var)) + geom_point(size=4, na.rm = TRUE, colour = "blue", alpha = 0.75) + theme_bw()    
  
  # Add centre line, axis labels and plot title
  fp = fp + geom_hline(aes(yintercept = funnel_centre_line), linetype = 2, colour = "black") + xlab(x_label) + ylab(y_label) + ggtitle(plot_title) + theme(axis.title=element_text(face="bold"), plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) 

  # Add funnel plot curves (limits)  
  if (funnel_curve_calculation_parameter==1) {
    # Adding funnel curves with 'geom_function'
    for (ci_ctr in 1:length(limits)) {    
      fp = fp + geom_function(fun = calc_fpc, args = list(ci_or_alpha = limits[ci_ctr], probability = funnel_centre_line, upper_or_lower = 1, trunc_val = 0), colour = colour4labels[ci_ctr], size = size4labels[ci_ctr], na.rm = TRUE, linetype = linetype4labels[ci_ctr]) + geom_function(fun = calc_fpc, args = list(ci_or_alpha = limits[ci_ctr], probability = funnel_centre_line, upper_or_lower = 2, trunc_val = 0), colour = colour4labels[ci_ctr], size = size4labels[ci_ctr], na.rm = TRUE, linetype = linetype4labels[ci_ctr]) 
    }
  } else if (funnel_curve_calculation_parameter>1) {
    # Adding funnel curves with 'geom_line'
    for (ci_ctr in 1:length(limits)) {
      lowerlimit=names(df)[(2*(ci_ctr-1)+3)]
      upperlimit=names(df)[(2*(ci_ctr-1)+4)]

      fp = fp + geom_line(aes_string(x = x_var, y = lowerlimit), colour = colour4labels[ci_ctr], size = size4labels[ci_ctr], na.rm = TRUE, linetype = linetype4labels[ci_ctr]) + geom_line(aes_string(x = x_var, y = upperlimit), colour = colour4labels[ci_ctr], size = size4labels[ci_ctr], na.rm = TRUE, linetype = linetype4labels[ci_ctr]) 
    }  
  }
  
  # Add labels to the plot
  if (addlabels==1 || addlabels=='ALL' || addlabels=='all') {
    if (debug==1) {
      print("Adding labels to all ...") # testing
    }  
    fp = fp + geom_label_repel(aes(label = labels), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', min.segment.length = 0, na.rm = TRUE)
  }

  if (addlabels==2 || addlabels=='OUTLIERS' || addlabels=='outliers' ||  addlabels==3 || addlabels=='OUTLIERS2' || addlabels=='outliers2' || addlabels=='ONLY_OAF' || addlabels=='only_oaf') {
    if (debug==1) {
      print("Adding labels for outliers only ...") # testing
    } 
    fp = fp + geom_label_repel(data = subset(df, df$outliers!=0 ), aes(label = labels), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', min.segment.length = 0, na.rm = TRUE)
  }

  if (addlabels=='LIST' ) {
    if (debug==1) {
      print("Adding labels from list ...") # testing
    } 
    fp = fp + geom_label_repel(data = subset(df, df$labels2display==1 ), aes(label = labels), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', min.segment.length = 0, na.rm = TRUE)
  }  
  
  # Set plot limits
  fp = fp + xlim(x_lower_limit,x_upper_limit) + ylim(y_lower_limit,y_upper_limit) 
  
  # Generate legend on another plot   
  if ( show_legend == TRUE ) {  
    # Create data set for legend generation
    legend_arr=matrix(0,ylen,num_of_funnels)
    if (funnel_centre_line>0.5) {
      # Use upper
      ii_start=1
    } else {
      # Use lower
      ii_start=2
    }
    legend_arr=matrix(0,ylen,num_of_funnels)
    i_legend=ii_start
    for (ci_ctr in 1:length(limits)) {   
      legend_arr[,ci_ctr]=limits_arr[,i_legend]
      i_legend=i_legend+2
    }

    if (debug==1) {
      print("legend_arr:")
      print(legend_arr) # testing
    } 
    
    legend_arr2=matrix(0,ylen*num_of_funnels,6)
    row_j=1
    for (ci_ctr in 1:length(limits)) {
      legend_arr2[row_j:(row_j+ylen-1),1]=matrix(x_var,ncol=1,nrow=ylen)
      legend_arr2[row_j:(row_j+ylen-1),2]=legend_arr[,ci_ctr]
      legend_arr2[row_j:(row_j+ylen-1),3]=matrix(legend_labels[ci_ctr],ncol=1,nrow=ylen)
      legend_arr2[row_j:(row_j+ylen-1),4]=matrix(colour4labels[ci_ctr],ncol=1,nrow=ylen)
      legend_arr2[row_j:(row_j+ylen-1),5]=matrix(size4labels[ci_ctr],ncol=1,nrow=ylen)
      legend_arr2[row_j:(row_j+ylen-1),6]=matrix(linetype4labels[ci_ctr],ncol=1,nrow=ylen)      
      row_j=row_j+ylen
    }

    if (debug==1) {
      print("legend_arr2:")
      print(legend_arr2) # testing
      print("Colour labels ...")
      print(colour4labels[1:num_of_funnels])
      print("Legend labels ...")
      print(legend_labels[1:num_of_funnels])
    }  
    
    legend_df = data.frame(x=legend_arr2[,1],y=legend_arr2[,2],Control_Limits=legend_arr2[,3],color_code=legend_arr2[,4],size_code=legend_arr2[,5],linetype_code=legend_arr2[,6])
    
    # Create plot from which the legend will be extracted
    plot_w_legend=ggplot(legend_df,aes(x = as.numeric(x), y = as.numeric(y), color = color_code)) +
      geom_line() + scale_color_identity(guide = "legend", labels = legend_labels[1:num_of_funnels], breaks = colour4labels[1:num_of_funnels]) + labs(color = "Funnel Curves") + theme(legend.position="bottom")
      
    if (debug==1) {  
      print(plot_w_legend) # testing
      print("Extracting legend...")
    }
     
    # Extract legend 
    pdf(NULL)
    output1 = invisible(  ggplot_gtable(ggplot_build(plot_w_legend))  )
    output2 = which(sapply(output1$grobs, function(x) x$name) == "guide-box") 
    stripped_legend = output1$grobs[[output2]] 
    dev.off()  
    
    # Add extracted legend to plot initially generated
    fp = grid.arrange(arrangeGrob(fp, ncol = 1),
         stripped_legend, nrow = 2, heights = c(20, 1))  
  }
  
  return(fp)
}


