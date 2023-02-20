# -----------------------------------------------------------------------------------------------------------
# R function to normalize a rate value
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

#' Calculates the normalized rate value
#'
#' @description
#' Returns normalized rate value 
#' 
#' @details
#' This function calculates the normed r. Inputs are the measured rate, ideal rate, normalized count and the measured count.  
#' The implementation is based on Bonert et al. (2023) https://doi.org/10.1038/s41598-022-26962-w 
#'
#' @param r_measured    measured rate (0-1)
#' @param r_ideal       ideal measure (0-1)
#' @param n_normed      normalized n
#' @param n_measured    measured n
#'
#' @return the normalized rate
#'
#' @examples
#' get_r_normed(0.14, 0.10, 198, 82)
#' get_r_normed(0.0, 0.10, 198, 82)
#'
#' @keywords internal
#' 
#' @export get_r_normed

get_r_normed <- function(r_measured, r_ideal, n_normed, n_measured) {
  # ---------------------------------------------------------------------------------------------------------
  r_normed=(r_measured-r_ideal)*(sqrt(r_ideal*(1-r_ideal)/n_normed)/sqrt(r_ideal*(1-r_ideal)/n_measured))+r_ideal

  return(r_normed)
}  
