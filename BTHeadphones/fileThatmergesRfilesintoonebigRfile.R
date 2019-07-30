setwd("C:/Users/Corona-Velez/Documents/GitHub/mercado-libre/BTHeadphones/")
filedir <- c("BTHeadphones_Arg.R", "BTHeadphones_Bra.R", "BTHeadphones_Chi.R", "BTHeadphones_Col.R", "BTHeadphones_Mex.R", "BTHeadphones_Per.R", "BTHeadphones_Uru.R", "BTHeadphones.R")

sink("mynewRfile.R")

for(i in c(1:8)) {
  current_file = readLines(paste("C:/Users/Corona-Velez/Documents/GitHub/mercado-libre/BTHeadphones/", filedir[i], sep = ""))
  cat("\n\n#### Current file:",filedir[i],"\n\n")
  cat("tryCatch({", sep = "")
  cat(current_file, sep ="\n")
  cat("}, warning = function(warning_condition) {
  print(warning_condition)
}, error = function(error_condition) {
  print(error_condition)
})", sep = "")
}

sink()
