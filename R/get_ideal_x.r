# -----------------------------------------------------------------------------------------------------------
# R function to get ideal x values for funnel curve calculation
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

#' Calculates ideal x values to obtain a smooth funnel curve 
#'
#' @description
#' Returns the x values for the funnel curve 
#' 
#' @details
#' This function calculates the x values to obtain a smooth funnel curve. Inputs are the plot limits, confidence intervals, funnel centre line and two control paramters ("fc_min_dx" and "fc_max_y_err"). The function approximates a y value between two x values by linear interpolation and then calculates the error in y using the function "calc_fpc". If the error in y exceeds "fc_max_y_err" and distance between the x values is greater than "fc_min_dx": an x value is added. 
#'
#' @param x_lower_limit         minimum x value for plot (0-1)
#' @param x_upper_limit         maximum x value for plot (0-1)
#' @param y_lower_limit         minimum y value for plot (0-1)
#' @param y_upper_limit         maximum y value for plot (0-1)
#' @param limits                funnel plot limits, e.g. 95 or 99.9
#' @param fc_min_dx             minimum delta x 
#' @param fc_max_y_err          maximal y error
#'
#' @return a vector that represent the ideal x values to obtain a smooth funnel curve  
#'
#' @examples
#' get_ideal_x(20, 100, .1, .9, c(95,99), 0.45, 0.5, 0.003)
#'
#' @keywords internal
#'
#' @export calc_fpc

get_ideal_x <- function(x_lower_limit, x_upper_limit, y_lower_limit, y_upper_limit, limits, funnel_centre_line, fc_min_dx, fc_max_y_err) {
  # ---------------------------------------------------------------------------------------------------------
  num_of_funnels=length(limits) # number of limits
  debug=0
  
  if (num_of_funnels<1) {
    stop("ERROR 1: 'limits' variable not valid!")
  }
  if (funnel_centre_line<0 || funnel_centre_line>1) {  
    stop("ERROR 2: 'funnel_centre_line' variable not valid!")  
  }
  
  if (funnel_centre_line<0.5) {
    upper_or_lower=1
  } else {
    upper_or_lower=2
  }
 
  x_range=x_upper_limit-x_lower_limit
  y_range=y_upper_limit-y_lower_limit

  # initialize 'x_var_tmp' and 'len_xvartmp'
  x_var_tmp = c(x_lower_limit, (x_lower_limit+x_upper_limit)/2, x_upper_limit)  
  len_xvartmp=dim(data.frame(x_var_tmp))[1]

  if (debug==1) {
    print("x_var_tmp:")
    print(x_var_tmp)
    print("upper_or_lower:")
    print(upper_or_lower)
    print("funnel_centre_line:")
    print(funnel_centre_line)  
    print("x_range:")
    print(x_range)
    print("y_range:")
    print(y_range)
    print(" ~~~~~ ~~~~~ ~~~~~ ~~~~~ ~~~~~ ~~~~~")    
    print(" ")
  }  
  
  for (limit_ctr in 1:length(limits)) {
    limit_ii=limits[limit_ctr]
  
    if (debug==1) {
      print(" ~~~~~ ~~~~~ ~~~~~ ~~~~~ ~~~~~ ~~~~~")
      print("limit_ii:")
      print(limit_ii)
      print(" ~~~~~ ~~~~~ ~~~~~ ~~~~~ ~~~~~ ~~~~~")
    }
  
    x_ii=1
    xii_element=c(-1,-1,-1)
    yii_element=c(-1,-1,-1)
    while (1) {
      divide_element=0
  
      # calculate y_ii, y_ii + 1
      y_ii = calc_fpc(limit_ii, funnel_centre_line, x_var_tmp[x_ii], upper_or_lower)
      y_ii_plus_1 = calc_fpc(limit_ii, funnel_centre_line, x_var_tmp[x_ii+1], upper_or_lower)
      delta_x_ii=x_var_tmp[x_ii+1]-x_var_tmp[x_ii]
      delta_y_ii=abs(y_ii_plus_1-y_ii)
          
      # calculate y_ii + 2
      y_ii_plus_2 = calc_fpc(limit_ii, funnel_centre_line, x_var_tmp[x_ii+2], upper_or_lower)
      delta_x_ii_plus_1=x_var_tmp[x_ii+2]-x_var_tmp[x_ii+1]
      delta_y_ii_plus_1=abs(y_ii_plus_2-y_ii_plus_1)
      
      # estimate y_ii + 1 with average
      y_ii_plus_1_estimate=(y_ii+y_ii_plus_2)/2
      
      # calculate error in estimation
      err_y_ii_plus_1 = abs(y_ii_plus_1 - y_ii_plus_1_estimate)
  
      if ( !is.na(err_y_ii_plus_1) ) {
        if (err_y_ii_plus_1> fc_max_y_err) {
          divide_element=1        
        }
      }
      
      xii_element[1]=x_var_tmp[x_ii]
      xii_element[2]=x_var_tmp[x_ii+1]
      xii_element[3]=x_var_tmp[x_ii+2]      
      yii_element[1]=y_ii
      yii_element[2]=y_ii_plus_1
      yii_element[3]=y_ii_plus_2    
    
      if ( is.na(yii_element[1]) && !is.na(yii_element[2]) ) {
        divide_element=1
      }
      #if ( is.na(yii_element[1]) && is.na(yii_element[2]) ) {
      #  x_ii=x_ii+1
      #}      
      
      if ( !(is.na(yii_element[1])) ) {  
        if (yii_element[1]<y_lower_limit || yii_element[1]>y_upper_limit) {
          divide_element=1
          exclude_xii=1
      
          if (yii_element[3]<y_lower_limit || yii_element[3]>y_upper_limit) {
            break
          }
        }    
      }
    
      if (debug==1) {
        print(" ----- ----- -----")
        print("x_ii:")
        print(x_ii)
        print("x_var_tmp[x_ii]:")
        print(x_var_tmp[x_ii])
        print("x_var_tmp[x_ii+1]:")
        print(x_var_tmp[x_ii+1])
        #print("delta_x_ii:")
        #print(delta_x_ii)
        #print("delta_x_ii_plus_1:")
        #print(delta_x_ii_plus_1)
        print("xii_element:")
        print(xii_element)
        print("yii_element:")
        print(yii_element)
        print("err_y_ii_plus_1:")
        print(err_y_ii_plus_1)
        print("divide_element:")
        print(divide_element)
        #plot(xii_element,yii_element) # testing
      }
    
    
      if (divide_element==1) {
        # update x_var_tmp
        new_x_var_tmp=matrix(NA,len_xvartmp+2)
        new_x_var_tmp[x_ii]=xii_element[1]
        new_x_var_tmp[x_ii+1]=(xii_element[1]+xii_element[2])/2 # new
        new_x_var_tmp[x_ii+2]=xii_element[2]
        new_x_var_tmp[x_ii+3]=(xii_element[2]+xii_element[3])/2 # new
        new_x_var_tmp[x_ii+4]=xii_element[3]        

        if ( x_ii>1 ) {
          new_x_var_tmp[1:x_ii-1]=x_var_tmp[1:x_ii-1]
        }
        
        if ( len_xvartmp >= 5 ) {
          #print("...(incomplete 3):") # testing
          #print(x_var_tmp[(x_ii+3):(len_xvartmp)]) # testing
          new_x_var_tmp[(x_ii+5):(len_xvartmp+2)]=x_var_tmp[(x_ii+3):(len_xvartmp)]
        }
        
        #if (debug==1) {
        #  print("new_x_var_tmp:")
        #  print(new_x_var_tmp)
        #}        
       
        x_var_tmp=new_x_var_tmp
        len_xvartmp=dim(data.frame(x_var_tmp))[1]
        
        if (debug==1) {
          print("updated 'x_var_tmp':")        
          print(x_var_tmp)        
          print("len_xvartmp:")
          print(len_xvartmp)
          #plot(new_x_var_tmp, new_y_var_tmp)
        }
        
        if ( (new_x_var_tmp[x_ii+1] - new_x_var_tmp[x_ii]) < fc_min_dx ) {
          x_ii=x_ii+1
        }
        
      } else {
        x_ii=x_ii+1
      }

      if (x_ii+1>=len_xvartmp) {
        break
      }
    }
    if ( debug == 1 ) {
      print("FINISHED limit_ii:")
      print(limit_ii)
      
      y_var_tmp=x_var_tmp
      for (x_ctr in 1:length(x_var_tmp)) {
         y_var_tmp[x_ctr]=calc_fpc(limit_ii, funnel_centre_line, x_var_tmp[x_ctr], upper_or_lower)
      }
      title_str=as.character(limit_ii)
      plot(x_var_tmp, y_var_tmp) 
      title(main=title_str)
    }
    if (limit_ctr==1) {
      output_x_var=x_var_tmp
    } else {
      output_x_var=c(output_x_var, x_var_tmp)
    }
    
    # reset 'x_var_tmp' and 'len_xvartmp'
    x_var_tmp = c(x_lower_limit, (x_lower_limit+x_upper_limit)/2, x_upper_limit) 
    len_xvartmp=dim(data.frame(x_var_tmp))[1]
    
  }
  #if ( debug == 1 ) {  
  #  print("output_x_var:")
  #  print(output_x_var)
  #}

  output_x_var=sort(output_x_var)
  #if ( debug == 1 ) {  
  #  print("output_x_var (sorted):")
  #  print(output_x_var)
  #}    
  
  output_x_var=unique(output_x_var)
  if ( debug == 1 ) {  
    print("output_x_var (unique):")
    print(output_x_var)
  }  
  
  return(output_x_var)
}
