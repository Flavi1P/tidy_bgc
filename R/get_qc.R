#' Extract QC of variables
#'
#' @description Create a data frame with the QC of selected variable along the depth of the profile
#'
#' @import stringr
#' @importFROM tidyr pivot_wider
#'
#' @param vars is a vector of variable
#' @param nc_path is the path of the ncdf file
#'
#' @return a tbale with QC value of each variable along the depth
#' @export

get_qc <- function(vars, nc_path){
  nc <- nc_open(nc_path)
  qc_table <- data.frame('vars' = character(),
                         'qc'= numeric())

  for(i in vars){
    t <- ncvar_get(nc, paste(i, 'QC', sep = '_'))
    if(grepl('/SD[0-9]', nc_path)){
      qc_vec <- unlist(str_split(t, pattern = ''))
    }
    else if(grepl('/BD[0-9]', nc_path)){
      qc_vec <- unlist(str_split(t[[3]], pattern = ''))
    }
    qc_vec <- as.numeric(gsub(' ', NA, qc_vec))
    depth <- seq(1:length(qc_vec))
    qc_var <- data.frame('vars' = paste(i, '_qc'), 'qc' = qc_vec, 'depth' = depth)
    qc_table <- bind_rows(qc_table, qc_var)
  }

  qc_table <- pivot_wider(qc_table, names_from = 'vars', values_from = 'qc')
  nc_close(nc)
  return(qc_table)
}
