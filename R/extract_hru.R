#' Extract the hrus table required for topHRU()
#'
#' @param file_path character string providing the absolute file path. The
#'   file to read must be either the project.mdb data base file or a .csv
#'   file to where the hrus table from the project.mdb data base was saved.
#'
#' @return Returns the hru_table as a data.frame
#' @export

extract_hru <- function(file_path) {
  if(grepl("\\.mdb$",file_path)){
    if (requireNamespace("RODBC", quietly = TRUE)) {
      mdb_con <- RODBC::odbcConnectAccess(file_path)
      hru_table <- RODBC::sqlQuery( mdb_con , paste("select * from hrus"))
    } else {
      stop("RODBC required if hrus table is loaded from 'project.mdb'",
           call. = FALSE)
    }
  } else if(grepl("\\.csv$",file_path)){
    hru_table <- read.csv(file = file_path)
  } else{
    stop("File type must be either '.csv' or '.mdb'")
  }
}
