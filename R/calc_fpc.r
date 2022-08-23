# -----------------------------------------------------------------------------------------------------------
# R function to calculate funnel plot curve
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

#' Calculates point(s) of funnel curve(s)
#'
#' @description
#' Returns the 'y' values(s) of the funnel curve(s) for one 'x' value
#' 
#' @details
#' This function calculates the 'y' value(s) of (a) funnel curve(s) for a given confidence interval (or alpha value), 'x' value (sample size), and funnel centre (probability). Options include: output only the upper or lower value, and truncate the results, if they are not realistic. 
#'
#' @param ci_or_alpha           confidence interval (if input >1), alpha value (if input <1)
#' @param probability           probability of the result; this is centre of the funnel plot (y value)
#' @param sample_size           size of the sample (x value)
#' @param upper_or_lower        determines the output; 0: returns upper and lower, 1: returns lower only, 2: returns upper only
#' @param trunc_val             truncate nonsensical values; default is: return only values between 0-1 and NA for values outside 0-1
#'
#' @return output value(s) between 0-1 (unless trunc_val is not 1) 
#'
#' @examples
#' calc_fpc(95, 0.269, 100)
#' calc_fpc(95, 0.269, 100, 2)
#' calc_fpc(99, 0.05, 100)
#'
#' @keywords internal
#'
#' @export calc_fpc

calc_fpc <- function(ci_or_alpha, probability, sample_size, upper_or_lower = 0, trunc_val = 1) {
  # ---------------------------------------------------------------------------------------------------------
  # for debugging
  #print("In 'calc_fpc' function")
  #print(ci_or_alpha)
  #print(probability)
  #print(sample_size)
  #print(upper_or_lower)
  #print(trunc_val)
  
  # Input integrity checking
  #if ( length(sample_size) > 1) {
  #  stop("ERROR: 'sample_size' argument is invalid")
  #}
  if ( is.na(sample_size) ) {
    return(NA)
  }
  
  # Use_normal distribution approximation
  # Get standard error
  se = sqrt(probability*(1-probability)/sample_size)
  
  if (ci_or_alpha>1) {
    alpha = (100 - ci_or_alpha)/100
  } else {
    alpha = ci_or_alpha
  }
  
  if (alpha==0.05) {
    magic_multiplier = 1.95996398454005
  } else if (alpha==0.001) {
    magic_multiplier = 3.29052673149194
  } else {
    magic_multiplier = qnorm(1-alpha/2) # inverse normal distribution
  }
  
  if ( upper_or_lower==1 ) {
    lowerrate = probability - magic_multiplier * se  
  
    if (trunc_val == 1) {
      if ( lowerrate<0 ) {
        lowerrate = NA
      }
    }  
    return (lowerrate)
  } else if ( upper_or_lower==2 ) {
    upperrate = probability + magic_multiplier * se
  
    if (trunc_val == 1) {
      if ( upperrate>1 ) {
        upperrate = NA
      }
    }
    return (upperrate)
  } else if ( upper_or_lower==0 ) {
    # Get the upperrate & lower rate
    upperrate = probability + magic_multiplier * se
    lowerrate = probability - magic_multiplier * se
  
    # Remove values that do not make sense
    if (trunc_val == 1) {
      if ( lowerrate<0 ) {
        lowerrate = NA
      }
      if ( upperrate>1 ) {
        upperrate = NA
      }
    }  
  
    output = c(lowerrate, upperrate)
    return(output)
  } else {
    stop("ERROR: 'upper_or_lower' argument is invalid")
  }
  
}
