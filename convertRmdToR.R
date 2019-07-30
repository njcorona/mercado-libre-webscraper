library(magrittr)

files <- c("iPhone8PlusCases", "iPhone8Cases", "ElectricRazors", "BTHeadphones", "BluetoothSpeakers", "MotorcycleHelmets")

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

# Set working directory
files <- c("iPhone8PlusCases", "iPhone8Cases", "ElectricRazors", "BTHeadphones", "BluetoothSpeakers", "MotorcycleHelmets")

updateFiles <- function(x) {
  filename <- x
  top <- "C:/Users/njcor/Documents/GitHub/mercado-libre/" %>% paste(filename, sep = "") %>% paste("/", sep = "") %>% paste(filename, sep = "") %>% paste("_", sep = "")
  
  vec <- c("Arg", "Bra", "Chi", "Col", "Mex", "Per", "Uru")
  
  sapply(vec, function(x) {
    Rfile <- top %>% paste(x, sep = "") %>% paste(".R", sep = "")
    print(Rfile)
    text <- readLines(Rfile)
    text[17] <- str_replace(text[17], "Corona-Velez", "njcor")
    writeLines(text, Rfile)
  })
  
}

sapply(files, updateFiles)
