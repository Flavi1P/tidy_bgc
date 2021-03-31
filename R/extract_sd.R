#' Extract sd
#'
#'@description Create a dataframe with bgc variables values, depth, date, lon and lat from a SD profile.
#'
#' @param nc_path the path to your file
#' @param vars a vector containing the BGC variables you want in the output table
#'
#' @return a table with your BGC variables, depth, date, lon and lat
#' @import dplyr
#' @importFrom  tidyr pivot_wider
#' @import magrittr
#' @export

extract_sd <- function(nc_path, vars){
  nc <- nc_open(nc_path)
  long_df <- data.frame('depth' = numeric(), 'variable' = character(), value = numeric())
  for(i in vars){
    var <- ncvar_get(nc, i)
    depth <- seq(1, length(var))
    table <- data.frame('depth' = depth, 'variable' = i, 'value' = var)
    long_df <- bind_rows(long_df, table)
  }
  lon <- ncvar_get(nc, 'LONGITUDE')
  lat <- ncvar_get(nc, 'LATITUDE')
  date <- get_date(nc)
  final_df <- long_df %>% pivot_wider(names_from = 'variable', values_from = 'value') %>%
    mutate('date' = date,
           'lon' = lon,
           'lat' = lat)
  return(final_df)
}
