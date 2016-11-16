#' Extract the hrus table required for topHRU()
#'
#' @param fle_path character string providing the absolute file path. The
#'   file to read must be either the project.mdb data base file or a .csv
#'   file to where the hrus table from the project.mdb data base was saved.
#'
#' @importFrom RODBC odbcConnectAccess sqlQuery
#'
#' @return Returns the hru_table as a data.frame
#' @export

extract_HRU <- function(file_path) {
  if(grepl(".mdb$",file_path)){
    mdb_con <- odbcConnectAccess(file_path)
    hru_table <- sqlQuery( mdb_con , paste("select * from hrus"))
  } else if(grepl(".csv$",file_path)){
    hru_table <- read.csv(file = file_path)
  } else{
    stop("File type must be either '.csv' or '.mdb'")
  }
}
