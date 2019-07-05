library(magrittr)

filename <- "ElectricRazors"

top <- "~/GitHub/mercado-libre/" %>% paste(filename, sep = "") %>% paste("/", sep = "") %>% paste(filename, sep = "") %>% paste("_", sep = "")

vec <- c("Arg", "Bra", "Chi", "Col", "Mex", "Per", "Uru")

sapply(vec, function(x) {
  
  input <- top %>% paste(x, sep = "") %>% paste(".Rmd", sep = "")
  output <- top %>% paste(x, sep = "") %>% paste(".R", sep = "")
  
  knitr::purl(input, output, documentation = 2)
})

input <- "~/GitHub/mercado-libre/" %>% paste(filename, sep = "") %>% paste("/", sep = "") %>% paste(filename, sep = "") %>% paste(".Rmd", sep = "")
output <- "~/GitHub/mercado-libre/" %>% paste(filename, sep = "") %>% paste("/", sep = "") %>% paste(filename, sep = "") %>% paste(".R", sep = "")
knitr::purl(input, output, documentation = 2)
