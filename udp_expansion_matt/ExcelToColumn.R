ExcelColToNumber <- function(col) {
  alphabet <- strsplit(intToUtf8(c(65:90)),"")[[1]]
  col <- toupper(col)  # Convert the column name to uppercase
  if (nchar(col) == 1) {
    which(alphabet == col)
  } 
  if (nchar(col) == 2) {
    val_1 <- substr(x = col, start = 1, stop = 1)
    val_2 <- substr(x = col, start = 2, stop = 2)
    val_1_num <- which(alphabet == val_1)
    val_2_num <- which(alphabet == val_2)
    val_1_num * 26 + val_2_num
    } else {
    stop("Cannot handle more than two characters")
  }
}

ExcelColToNumber("iw")
