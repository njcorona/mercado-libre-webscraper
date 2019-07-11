library(magrittr)

files <- c("iPhone8PlusCases", "iPhone8Cases", "ElectricRazors", "BluetoothHeadphones")

updateFiles <- function(x) {
  filename <- x
  top <- "C:/Users/Corona-Velez/Documents/GitHub/mercado-libre/" %>% paste(filename, sep = "") %>% paste("/", sep = "") %>% paste(filename, sep = "") %>% paste("_", sep = "")

  vec <- c("Arg", "Bra", "Chi", "Col", "Mex", "Per", "Uru")

  sapply(vec, function(x) {
  
    input <- top %>% paste(x, sep = "") %>% paste(".Rmd", sep = "")
    output <- top %>% paste(x, sep = "") %>% paste(".R", sep = "")
    print(input)
    knitr::purl(input, output, documentation = 2)
  })

  input <- "C:/Users/Corona-Velez/Documents/GitHub/mercado-libre/" %>% paste(filename, sep = "") %>% paste("/", sep = "") %>% paste(filename, sep = "") %>% paste(".Rmd", sep = "")
  output <- "C:/Users/Corona-Velez/Documents/GitHub/mercado-libre/" %>% paste(filename, sep = "") %>% paste("/", sep = "") %>% paste(filename, sep = "") %>% paste(".R", sep = "")
  print(input)
  knitr::purl(input, output, documentation = 2)
}

sapply(files, updateFiles)
