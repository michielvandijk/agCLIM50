#'=================================================================================================
#' Project:  General
#' Subject:  Get data path 
#' Author:   Tom Morley
#' Contact:  tomas.morley@wur.nl
#' Output:   Correct datapath for user
#'=================================================================================================

# Use this file to set your path to the data
# check your computer username using
# Sys.info()["user"] and use this in the if
# statement. Then add your dataPath within 
# the {} brackets

# Michiel WEcR
if(Sys.info()["user"] == "dijk158") {
  dataPath <- "D:\\R\\agCLIM50"}

# Michiel IIASA
if(Sys.info()["user"] == "vandijkm") {
  dataPath <- "C:/Users/vandijkm/Dropbox/AgClim50 scenario results"}

# Anybody else:
if(Sys.info()["user"] == "") {
  dataPath <- ""}
