#' ---
#' title: "MotorcycleHelmets"
#' author: "NicolasCorona"
#' date: "6/16/2019"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(httr)))
suppressWarnings(suppressMessages(library(stringr)))
suppressWarnings(suppressMessages(library(rvest)))
suppressWarnings(suppressMessages(library(magrittr)))
suppressWarnings(suppressMessages(library(future)))
setwd("C:/Users/Corona-Velez/Documents/GitHub/mercado-libre/MotorcycleHelmets")

#' 
## ------------------------------------------------------------------------
print(Sys.time())
date <- paste(str_sub(Sys.time(), 1, 10), "_", sep = "")
arg_p <- read_csv(date %>% paste("MotorcycleHelmets_Prod_Arg.csv", sep = ""), col_names = TRUE)
bra_p <- read_csv(date %>% paste("MotorcycleHelmets_Prod_Bra.csv", sep = ""), col_names = TRUE)
chi_p <- read_csv(date %>% paste("MotorcycleHelmets_Prod_Chi.csv", sep = ""), col_names = TRUE)
col_p <- read_csv(date %>% paste("MotorcycleHelmets_Prod_Col.csv", sep = ""), col_names = TRUE)
mex_p <- read_csv(date %>% paste("MotorcycleHelmets_Prod_Mex.csv", sep = ""), col_names = TRUE)
per_p <- read_csv(date %>% paste("MotorcycleHelmets_Prod_Per.csv", sep = ""), col_names = TRUE)
uru_p <- read_csv(date %>% paste("MotorcycleHelmets_Prod_Uru.csv", sep = ""), col_names = TRUE)

arg_s <- read_csv(date %>% paste("MotorcycleHelmets_Sell_Arg.csv", sep = ""), col_names = TRUE)
bra_s <- read_csv(date %>% paste("MotorcycleHelmets_Sell_Bra.csv", sep = ""), col_names = TRUE)
chi_s <- read_csv(date %>% paste("MotorcycleHelmets_Sell_Chi.csv", sep = ""), col_names = TRUE)
col_s <- read_csv(date %>% paste("MotorcycleHelmets_Sell_Col.csv", sep = ""), col_names = TRUE)
mex_s <- read_csv(date %>% paste("MotorcycleHelmets_Sell_Mex.csv", sep = ""), col_names = TRUE)
per_s <- read_csv(date %>% paste("MotorcycleHelmets_Sell_Per.csv", sep = ""), col_names = TRUE)
uru_s <- read_csv(date %>% paste("MotorcycleHelmets_Sell_Uru.csv", sep = ""), col_names = TRUE)

arg_p <- cbind(country = "ARG", arg_p)
bra_p <- cbind(country = "BRA", bra_p)
chi_p <- cbind(country = "CHI", chi_p)
col_p <- cbind(country = "COL", col_p)
mex_p <- cbind(country = "MEX", mex_p)
per_p <- cbind(country = "PER", per_p)
uru_p <- cbind(country = "URU", uru_p)

arg_s <- cbind(country = "ARG", arg_s)
bra_s <- cbind(country = "BRA", bra_s)
chi_s <- cbind(country = "CHI", chi_s)
col_s <- cbind(country = "COL", col_s)
mex_s <- cbind(country = "MEX", mex_s)
per_s <- cbind(country = "PER", per_s)
uru_s <- cbind(country = "URU", uru_s)

arg_p$country <- as.character(arg_p$country)
bra_p$country <- as.character(bra_p$country)
chi_p$country <- as.character(chi_p$country)
col_p$country <- as.character(col_p$country)
mex_p$country <- as.character(mex_p$country)
per_p$country <- as.character(per_p$country)
uru_p$country <- as.character(uru_p$country)

arg_s$country <- as.character(arg_s$country)
bra_s$country <- as.character(bra_s$country)
chi_s$country <- as.character(chi_s$country)
col_s$country <- as.character(col_s$country)
mex_s$country <- as.character(mex_s$country)
per_s$country <- as.character(per_s$country)
uru_s$country <- as.character(uru_s$country)

arg_p$num_installments <- as.character(arg_p$num_installments)
bra_p$num_installments <- as.character(bra_p$num_installments)
chi_p$num_installments <- as.character(chi_p$num_installments)
col_p$num_installments <- as.character(col_p$num_installments)
mex_p$num_installments <- as.character(mex_p$num_installments)
per_p$num_installments <- as.character(per_p$num_installments)
uru_p$num_installments <- as.character(uru_p$num_installments)

arg_p$amt_installments <- as.character(arg_p$amt_installments)
bra_p$amt_installments <- as.character(bra_p$amt_installments)
chi_p$amt_installments <- as.character(chi_p$amt_installments)
col_p$amt_installments <- as.character(col_p$amt_installments)
mex_p$amt_installments <- as.character(mex_p$amt_installments)
per_p$amt_installments <- as.character(per_p$amt_installments)
uru_p$amt_installments <- as.character(uru_p$amt_installments)

prod <- bind_rows(arg_p, bra_p, chi_p, col_p, mex_p, per_p, uru_p)
sell <- bind_rows(arg_s, bra_s, chi_s, col_s, mex_s, per_s, uru_s)

# Names are reversed.  Here they are unreversed.
stored_units_timeframe_of_amt_sold <- sell$units_timeframe_of_amt_sold
sell$units_timeframe_of_amt_sold <- sell$timeframe_of_amt_sold
sell$timeframe_of_amt_sold <- stored_units_timeframe_of_amt_sold

# Combining plural/singuler/Spanish/Portuguese words.
sell$units_of_time_operating[which(sell$units_of_time_operating == "ano")] <- "años"
sell$units_of_time_operating[which(sell$units_of_time_operating == "anos")] <- "años"
sell$units_of_time_operating[which(sell$units_of_time_operating == "año")] <- "años"

sell$units_of_time_operating[which(sell$units_of_time_operating == "mês")] <- "meses"
sell$units_of_time_operating[which(sell$units_of_time_operating == "mes")] <- "meses"

sell$units_of_time_operating[which(sell$units_of_time_operating == "dias")] <- "días" 

sell$units_timeframe_of_amt_sold[which(sell$units_timeframe_of_amt_sold == "anos")] <- "años"
sell$units_timeframe_of_amt_sold[which(sell$units_timeframe_of_amt_sold == "mes")] <- "meses"
sell$units_timeframe_of_amt_sold[which(sell$units_timeframe_of_amt_sold == "dias")] <- "días"

prod$shipping[which(prod$shipping == "Envio")] <- "Envío"
prod$shipping[which(prod$shipping == "Envio para todo o país")] <- "Envío a todo el país"
prod$shipping[which(prod$shipping == "Envío a nivel nacional")] <- "Envío a todo el país"
prod$shipping[which(prod$shipping == "Entrega a combinar com o vendedor")] <- "Entrega a acordar con el vendedor"

prod$shipping[which(prod$shipping == "Frete grátis")] <- "Envío gratis"
prod$shipping[which(prod$shipping == "Chegará hoje")] <- "Llega hoy"
prod$shipping[which(prod$shipping == "Envío gratis a nivel nacional")] <- "Envío gratis a todo el país"

prod$free_return[which(prod$free_return == "Devolução grátis")] <- "Devolución gratis"

prod$free_return_info[which(prod$free_return_info == "Você tem 30 dias a partir do recebimento")] <- "Tenés 30 días desde que lo recibís"
prod$free_return_info[which(prod$free_return_info == "Você tem 10 dias a partir do recebimento")] <- "Tenés 10 días desde que lo recibís"
prod$free_return_info[which(prod$free_return_info == "Tienes 10 días desde que lo recibes")] <- "Tenés 10 días desde que lo recibís"
prod$free_return_info[which(prod$free_return_info == "Tienes 30 días desde que lo recibes")] <- "Tenés 30 días desde que lo recibís" 

# Set seller amount sold timeframe values for Peru and Uruguay.
sell$units_timeframe_of_amt_sold[which(sell$country == "PER")] <- sell$units_of_time_operating[which(sell$country == "PER")]

sell$units_timeframe_of_amt_sold[which(sell$country == "URU")] <- sell$units_of_time_operating[which(sell$country == "URU")]

sell$timeframe_of_amt_sold[which(sell$country == "PER")] <- sell$time_operating[which(sell$country == "PER")]
sell$timeframe_of_amt_sold[which(sell$country == "URU")] <- sell$time_operating[which(sell$country == "URU")]

# Converting to integers.
units_of_time_operating_values <- sell$units_of_time_operating[!duplicated(sell$units_of_time_operating)]
units_of_time_operating_values <- units_of_time_operating_values[!is.na(units_of_time_operating_values)]

sell$units_of_time_operating <- sapply(sell$units_of_time_operating, function(x) { return ( if (is.na(x)) { NA } else { which(x == units_of_time_operating_values) }) })

# These were commented out.  Why?
# units_timeframe_of_amt_sold_values <- sell$units_timeframe_of_amt_sold[!duplicated(sell$units_timeframe_of_amt_sold)]
# units_timeframe_of_amt_sold_values <- units_timeframe_of_amt_sold_values[!is.na(units_timeframe_of_amt_sold_values)]

sell$units_timeframe_of_amt_sold <- sapply(sell$units_timeframe_of_amt_sold, function(x) { return ( if (is.na(x)) { NA } else { which(x == units_of_time_operating_values) }) })

shipping_values <- prod$shipping[!duplicated(prod$shipping)]
shipping_values <- shipping_values[!is.na(shipping_values)]

prod$shipping <- sapply(prod$shipping, function(x) { return ( if (is.na(x)) { NA } else { which(x == shipping_values) }) })

free_return_values <- prod$free_return[!duplicated(prod$free_return)]
free_return_values <- free_return_values[!is.na(free_return_values)]

prod$free_return <- sapply(prod$free_return, function(x) { return ( if (is.na(x)) { NA } else { which(x == free_return_values) }) })

free_return_info_values <- prod$free_return_info[!duplicated(prod$free_return_info)]
free_return_info_values <- free_return_info_values[!is.na(free_return_info_values)]

prod$free_return_info <- sapply(prod$free_return_info, function(x) { return ( if (is.na(x)) { NA } else { which(x == free_return_info_values) }) })

# Combining characteristics with different names.
prod$`Línea`[which(prod$country == "BRA")] <- prod$Linha[which(prod$country == "BRA")]
prod$`Tipo de casco`[which(prod$country == "BRA")] <- prod$`Tipo de capacete`[which(prod$country == "BRA")]
prod$`Materiales del exterior`[which(prod$country == "BRA")] <- prod$`Materiais do exterior`[which(prod$country == "BRA")]
prod$`Materiales del interior`[which(prod$country == "BRA")] <- prod$`Materiais do interior`[which(prod$country == "BRA")]
prod$Edad[which(prod$country == "BRA")] <- prod$Idade[which(prod$country == "BRA")]
prod$`Tipo de casco`[which(prod$country == "CHI")] <- prod$Tipo[which(prod$country == "CHI")]

t <- prod$`Tipo de casco`[which(prod$country == "URU")] 
t[which(is.na(t))] <- prod$Tipo[which(prod$country == "URU")][which(is.na(t))]
prod$`Tipo de casco`[which(prod$country == "URU")] <- t

t <- prod$`Tipo de casco`[which(prod$country == "COL")] 
t[which(is.na(t))] <- prod$Tipo[which(prod$country == "COL")][which(is.na(t))]
prod$`Tipo de casco`[which(prod$country == "COL")] <- t

prod$Linha <- NULL
prod$`Tipo de capacete` <- NULL
prod$`Materiais do exterior` <- NULL
prod$`Materiais do interior` <- NULL
prod$Idade <- NULL
prod$Tipo <- NULL

# Remove arrival_time column. #14
prod$arrival_time <- NULL

write_csv(prod, date %>% paste("MotorcycleHelmets_Prod.csv", sep = ""))
write_csv(sell, date %>% paste("MotorcycleHelmets_Sell.csv", sep = ""))

# I may want to remove this particular bit of cleaning until I bring all of the countries together.
list <- list(units_of_time_operating_values, shipping_values, free_return_values, free_return_info_values)
max <- max( sapply(list, function(x) { length(x) }) )
list <- lapply(list, function(x) {
  if (length(x) < max) {
    orig_l <- length(x)
    x <- rep(x, length.out = max)
    x[c((orig_l + 1):max)] <- "" # As opposed to " ".
    return (x)
  }
  return (x)
  
})

data_legend <- tibble(value = c(1:max), units_of_time_operating = list[[1]], units_timeframe_of_amt_sold = list[[1]], shipping = list[[2]], free_return = list[[3]], free_return_info = list[[4]])
write_csv(data_legend, date %>% paste("MotorcycleHelmets_Legend.csv", sep = ""))
print(Sys.time())


#' 
