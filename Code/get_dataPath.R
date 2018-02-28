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
  dataPath <- "C:\\Users\\dijk158\\Dropbox\\AgClim50 scenario results"}

if(Sys.info()["user"] == "dijk158") {
  GAMSPath <- "C:\\Program Files\\GAMS\\win64\\24.6"}



# Michiel IIASA
if(Sys.info()["user"] == "vandijkm") {
  dataPath <- "C:/Users/vandijkm/Dropbox/AgClim50 scenario results"}

if(Sys.info()["user"] == "vandijkm") {
  GAMSPath <- "C:/GAMS/win64/24.9"}
