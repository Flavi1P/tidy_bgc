#' Date of profile
#'
#' @description A function to extract the date of the profile
#'
#' @param nc is a nc object returned by the nc_open of ncdf4 package
#'
#' @return tge function return a date with YYYY/MM/DD format
#' @import ncdf4
#' @importFrom lubridate date
#' @export


get_date <- function(nc){
  juld <- ncvar_get(nc, "JULD")
  juldqc <- ncvar_get(nc, "JULD_QC")
  origin <- NA
  origin <- as.POSIXct("1950-01-01 00:00:00", order="ymdhms") #convert juld->time
  time <- NA
  time <- origin + juld*3600*24
  time <- lubridate::date(time)
  jd_qc <- NA
  jd_qc <- substr(ncvar_get(nc,"JULD_QC"),1,1)
  if(jd_qc == 1){
    return(time)
  }
  else{
    print('JULD QC is different of 1')
  }


}
