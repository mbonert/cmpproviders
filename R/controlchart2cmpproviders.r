# -----------------------------------------------------------------------------------------------------------
# R function to create a control chart to compare (healthcare) providers
#
# Michael Bonert, BASc, MASC, MD, FRCPC ( michael@librepathology.org )
# Copyright (c) 2023
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

#' Creates a control chart with an arbitrary number of control lines
#' 
#' @description
#' Control charts to compare (healthcare) providers or institutions
#'
#' @details
#' A function to compare provider rate data.  The implementation is based on Bonert et al. 
#' (2023) https://doi.org/10.1038/s41598-022-26962-w 
#'
#' @param x_var                         x values on funnel plot, e.g. number of specimens
#' @param y_var                         y values on funnel plot, e.g. diagnostic rate (0-1)
#' @param labels                        labels that describe the (x,y) data points, e.g. name of institution or provider identification; default: the data points are numbered (if undefined)
#' @param limits                        limits (confidence intervals); default: 95\%, 99.9\%
#' @param control_chart_centre_line     defines the control chart centre line; default: median of y_var
#' @param addlabels                     whether to display labels on the funnel plot; accepted values are:
#' \itemize{
#' \item{\code{"none"}}{ : do not add labels (default)}
#' \item{\code{"all"}}{ : add labels to all points}
#' \item{\code{"outliers"}}{ : label only outliers outside of the inner funnels}
#' \item{\code{"outliers2"}}{ : label only outliers outside of second innermost funnel}
#' \item{\code{"only_oaf"}}{ : label only outliers outside of all funnels}
#' \item{\code{"list"}}{ : display the list of labels given by argument \code{labels_to_display}}
#' }
#' @param labels_to_display             selected labels to display (see argument \emph{addlabels})
#' @param x_label                       x axis label
#' @param y_label                       y axis label
#' @param plot_title                    title of the (funnel) plot
#' @param show_legend                   TRUE shows legend at bottom of plot
#' @param y_percent                     y axis in percent (multiplies y_var_normed by 100)
#' @param sort_by_y_norm                sort by the normed y value
#' @param showoutlier_arr               show the outlier array
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
#' print("Creating control charts ...")
#' cc1=controlchart2cmpproviders(x_var, y_var, limits=c(95,99.9,99.9999), addlabels = 1, 
#'   x_label = "Provider", y_label = "Normalized Death Rate by Provider", 
#'   plot_title = "Normalized Death Rate by Provider") 
#' cc2=controlchart2cmpproviders(x_var, y_var, limits=c(95,99.9,99.9999), labels=provider_labels, 
#'   addlabels = "OUTLIERS", x_label = "Provider", y_label = "Normalized Provider Death Rate", 
#'   plot_title = "Normalized Death Rate by Provider with Outliers Labelled", do_not_sort_by_y_norm=1)
#' cc3=controlchart2cmpproviders(x_var, y_var, limits=c(95,99.9,99.9999), labels=provider_labels, 
#'   addlabels = "OUTLIERS", x_label = "Provider", y_label = "Normalized Provider Death Rate", 
#'   plot_title = "Normalized Death Rate by Provider with Outliers Labelled")
#'
#' @import ggplot2 
#' @import grid gridExtra
#' @importFrom ggrepel geom_label_repel
#'
#' @export controlchart2cmpproviders


controlchart2cmpproviders <- function(x_var, y_var, labels = NULL, limits = c(95,99.9), control_chart_centre_line = NULL, addlabels = 'NONE', labels_to_display = NULL, x_label = NULL, y_label = NULL, plot_title = NULL, show_legend=TRUE, y_percent = FALSE, sort_by_y_norm=TRUE, showoutlier_arr=FALSE ) {
  # Control
  debug=0
  
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
  if ( is.null(control_chart_centre_line) ) {
    control_chart_centre_line = median(y_var)  
  }  
  if (debug==1) {
    print("control_chart_centre_line:")
    print(control_chart_centre_line) # testing
  }  
  
  # Create outliers array
  num_of_controlcharts=length(limits) # number of limits
  outliers_arr=matrix(0,ylen,num_of_controlcharts)
  colnames(outliers_arr) = limits
  rownames(outliers_arr) = labels   
  
  # Get n_normed
  nnormed_idx=which.max(x_var)
  n_normed=x_var[nnormed_idx]
  
  if (debug==1) {
    print("n_normed:")
    print(n_normed)
  }
  
  # Calculate y_var_normed
  y_var_normed=matrix(0,ylen,1)
  for (x_ctr in 1:length(x_var)) {
    y_var_normed[x_ctr] = get_r_normed( y_var[x_ctr], control_chart_centre_line, n_normed, x_var[x_ctr])
  }
  
  if (debug==1) {
    print("y_var_normed:")
    print(y_var_normed)
  }

  # Sort the input by y_var_normed 
  if (sort_by_y_norm==TRUE) {
    df2sort=data.frame(x_var,y_var_normed,labels)  
    sortorder=order(df2sort$y_var_normed,decreasing = TRUE)

    if (debug==1) {
      print("sortorder:")
      print(sortorder) # testing
      #print(df2sort[order(sortorder), ])
      print(df2sort[order(df2sort$y_var_normed, decreasing = FALSE), ])
    }
    y_var_normed=df2sort[order(df2sort$y_var_normed, decreasing = FALSE), ]$y_var_normed
    x_var=df2sort[order(df2sort$y_var_normed, decreasing = FALSE), ]$x_var
    labels=df2sort[order(df2sort$y_var_normed, decreasing = FALSE), ]$labels  
  }

  # Create outliers array
  num_of_controlcharts=length(limits) # number of limits
  outliers_arr=matrix(0,ylen,num_of_controlcharts)
  colnames(outliers_arr) = limits
  rownames(outliers_arr) = labels   

  len_outlierarr=ylen  
  
  # Calculate the control limits  
  limits_arr=matrix(0,ylen,2*num_of_controlcharts)
  for (x_ctr in 1:length(x_var)) {
    for (ci_ctr in 1:length(limits)) {
      #ci_kk=calc_fpc(limits[ci_ctr], control_chart_centre_line, x_var[x_ctr])
      ci_kk=calc_fpc(limits[ci_ctr], control_chart_centre_line, n_normed)

      limits_arr[x_ctr,(2*(ci_ctr-1)+1)]=ci_kk[1]
      limits_arr[x_ctr,(2*(ci_ctr-1)+2)]=ci_kk[2]  
      
      # check whether outside funnel (below)
      if (!is.na(ci_kk[1])) {
        if (y_var_normed[x_ctr]<ci_kk[1]) {
          outliers_arr[x_ctr,ci_ctr]=-1
        }
      }  
      # check whether outside funnel (above)
      if (!is.na(ci_kk[2])) {
        if (y_var_normed[x_ctr]>ci_kk[2]) {
         outliers_arr[x_ctr,ci_ctr]=1      
        }
      }      
    }
  }

  if (debug==1) {
    print("limits_arr:")
    print(limits_arr)
  }  
  
  df_limits=data.frame(limits_arr)
  if (debug==1) {
    print("df_limits:")
    print(df_limits) # testing
  }
  
  if (debug==1 || showoutlier_arr==TRUE) {
    print("outliers_arr:")
    print(outliers_arr) # testing
  }    

  # Create 'outliers' variable to put into dataframe
  outliers=labels
  if (addlabels==2 || addlabels=='OUTLIERS' || addlabels=='outliers') {
    outliers[1:len_outlierarr]=outliers_arr[,1] # use innermost funnel
  } else if ( (addlabels=='OUTLIERS2' || addlabels=='outliers2') && num_of_controlcharts>1 ) {
    outliers[1:len_outlierarr]=outliers_arr[,2] # use second innermost funnel  
  } else {
    outliers[1:len_outlierarr]=outliers_arr[,num_of_controlcharts] # use outermost funnel   
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
  
  x_var_count=c(1:xlen)
 
  # Create data frame
  if ( y_percent==TRUE ) {
    y_var_normed=y_var_normed*100
    control_chart_centre_line=control_chart_centre_line*100
    df = data.frame(x_var_count,y_var_normed,df_limits*100,labels,outliers,labels2display) 
  } else {
    df = data.frame(x_var_count,y_var_normed,df_limits,labels,outliers,labels2display)
  }  
  
  if (debug==1) {
    print("df:")
    print(df) # testing
  } 
   
  # Create legend_labels
  legend_labels=matrix(0,num_of_controlcharts+2)
  colour4labels=matrix(0,num_of_controlcharts+2)
  size4labels=matrix(0,num_of_controlcharts+2)
  linetype4labels=matrix(0,num_of_controlcharts+2)
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
  legend_labels[num_of_controlcharts+2]="Providers"
  colour4labels[num_of_controlcharts+2]="blue"
  legend_labels[num_of_controlcharts+1]="Funnel Centre Line"
  colour4labels[num_of_controlcharts+1]="black"
  
  if (debug==1) {
    print("legend_labels")
    print(legend_labels) # testing
    print("colour4labels")
    print(colour4labels) # testing
  }   
  
  # Plot the data points
  #   theme_bw() --> background black and white  
  cc = ggplot(df,aes(x=x_var_count,y=y_var_normed)) + geom_point(size=4, na.rm = TRUE, colour = "blue", alpha = 0.75) + theme_bw()    

  # Add centre line, axis labels and plot title
  cc = cc + geom_hline(aes(yintercept = control_chart_centre_line), linetype = 2, colour = "black") + xlab(x_label) + ylab(y_label) + ggtitle(plot_title) + theme(axis.title=element_text(face="bold"), plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) 

  # Adding control lines with 'geom_line'
  for (ci_ctr in 1:length(limits)) {
    lowerlimit=names(df)[(2*(ci_ctr-1)+3)]
    upperlimit=names(df)[(2*(ci_ctr-1)+4)]

    cc = cc + geom_line(aes_string(x = c(1:xlen), y = lowerlimit), colour = colour4labels[ci_ctr], size = size4labels[ci_ctr], na.rm = TRUE, linetype = linetype4labels[ci_ctr]) + geom_line(aes_string(x = c(1:xlen), y = upperlimit), colour = colour4labels[ci_ctr], size = size4labels[ci_ctr], na.rm = TRUE, linetype = linetype4labels[ci_ctr]) 
  }   
  
  # Add labels to the plot
  if (addlabels==1 || addlabels=='ALL') {
    if (debug==1) {
      print("Adding labels to all ...") # testing
    }  
    cc = cc + geom_label_repel(aes(label = labels), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', min.segment.length = 0, na.rm = TRUE)
  }

  if (addlabels==2 || addlabels=='OUTLIERS'  ||  addlabels==3 || addlabels=='OUTLIERS2'  ||  addlabels=='ONLY_OAF') {
    if (debug==1) {
      print("Adding labels for outliers only ...") # testing
    } 
    cc = cc + geom_label_repel(data = subset(df, df$outliers!=0 ), aes(label = labels), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', min.segment.length = 0, na.rm = TRUE)
  }

  if (addlabels=='LIST' ) {
    if (debug==1) {
      print("Adding labels from list ...") # testing
    } 
    cc = cc + geom_label_repel(data = subset(df, df$labels2display==1 ), aes(label = labels), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', min.segment.length = 0, na.rm = TRUE)
  }  
  
  
  # Generate legend on another plot   
  if ( show_legend == TRUE ) {  
    # Create data set for legend generation
    legend_arr=matrix(0,ylen,num_of_controlcharts)
    if (control_chart_centre_line>0.5) {
      # Use upper
      ii_start=1
    } else {
      # Use lower
      ii_start=2
    }
    legend_arr=matrix(0,ylen,num_of_controlcharts)
    i_legend=ii_start
    for (ci_ctr in 1:length(limits)) {   
      legend_arr[,ci_ctr]=limits_arr[,i_legend]
      i_legend=i_legend+2
    }

    if (debug==1) {
      print("legend_arr:")
      print(legend_arr) # testing
    } 
    
    legend_arr2=matrix(0,ylen*num_of_controlcharts,6)
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
      print(colour4labels[1:num_of_controlcharts])
      print("Legend labels ...")
      print(legend_labels[1:num_of_controlcharts])
    }  
    
    legend_df = data.frame(x=legend_arr2[,1],y=legend_arr2[,2],Control_Limits=legend_arr2[,3],color_code=legend_arr2[,4],size_code=legend_arr2[,5],linetype_code=legend_arr2[,6])
    
    # Create plot from which the legend will be extracted
    plot_w_legend=ggplot(legend_df,aes(x = as.numeric(x), y = as.numeric(y), color = color_code)) +
      geom_line() + scale_color_identity(guide = "legend", labels = legend_labels[1:num_of_controlcharts], breaks = colour4labels[1:num_of_controlcharts]) + labs(color = "Control Lines") + theme(legend.position="bottom")
      
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
    
    # Add legend to plot initially generated
    suppressPackageStartupMessages(library(gridExtra))
    library(grid)
    library(lattice)
    cc = grid.arrange(arrangeGrob(cc, ncol = 1),
         stripped_legend, nrow = 2, heights = c(20, 1))  
  }
  
  return(cc)
}
