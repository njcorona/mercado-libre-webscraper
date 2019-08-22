#' ---
#' title: "BluetoothSpeakers_Arg"
#' author: "NicolasCorona"
#' date: "5/27/2019"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
# This code loads in the libraries of code I am using when I write this code.  For example, rvest and httr are the two libraries I use to identify parts of a website and scrape data from the website. 
knitr::opts_chunk$set(echo = TRUE)
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(httr)))
suppressWarnings(suppressMessages(library(stringr)))
suppressWarnings(suppressMessages(library(rvest)))
suppressWarnings(suppressMessages(library(magrittr)))
suppressWarnings(suppressMessages(library(future)))
setwd("C:/Users/Corona-Velez/Documents/GitHub/mercado-libre/BluetoothSpeakers")

#' 
#' ### Links for bluetooth speakers, Argentina, including the first 50
#' 
## ----scrape_links--------------------------------------------------------
date <- paste(str_sub(Sys.time(), 1, 10), "_", sep = "")
links <- data.frame()

# The first URL when you search the products is different from the URLs for all products after the fiftieth.  That is the URL stored here.
url <- "https://electronica.mercadolibre.com.ar/reproductores-portatiles-accesorios-parlantes/bluetooth-parlante_ItemTypeID_N"

# read_html() is a function that takes in a URL and extracts the html code that is associated with that URL.
# html_nodes() is a function that takes in a block of html code and the name of a part of that code and returns that part of the html code, if it exists.
# html_text() takes in a part of an html code and returns any text data (words) associated with it. 
# html_attr() is similar to html_text() but it extracts any URL data associated with a node (like a hyperlink in an email).

num_products_to_scrape <- read_html(url) %>% html_nodes(".quantity-results") %>% html_text() # Gets node value of the quantity of search results.
num_products_to_scrape <- as.numeric(paste(str_extract_all(num_products_to_scrape, "[0-9]+")[[1]], collapse = '')) # Extracts integer.
num_products_to_scrape <- if (num_products_to_scrape >= 2001) { 40 } else { floor(num_products_to_scrape / 50) } # Sets to correct value.  We assume there will be greater than 50 search results.  If there are greater than 2000sx search results, this is set to 2001.  Otherwise, it's set to the greatest possible number of links we can scrape given the number of search results provided.

curr_url <- read_html(url) %>% html_nodes("a.item__info-title") %>% html_attr("href")
  
curr_item <- read_html(url) %>% html_nodes(".main-title") %>% html_text()
  
if (length(curr_url) < length(curr_item)) {
  curr_item <- curr_item[1:length(curr_url)]
} else if (length(curr_url) > length(curr_item)) {
  curr_url <- curr_url[1:length(curr_item)]
}

df <- data.frame(curr_url[1:length(curr_url)], curr_item[1:length(curr_url)])
df$index <- c((1):(1 + length(curr_url) - 1))
parent <- c(1:length(curr_url))
parent[1:length(curr_url)] <- url
df$parent <- parent
links <- rbind(links, df)

# This for-loop gets the URLs for products in all of search result pages (page 1, page 2, ...), filtered for new items by "_ItemTypeID_N".
counter = nrow(links) + 1
print("Scraping from pages with product links.")
for (i in 1:num_products_to_scrape) {
  print(i)
  url <- paste(paste("https://electronica.mercadolibre.com.ar/reproductores-portatiles-accesorios-parlantes/bluetooth-parlante_Desde_", (i * 50) + 1, sep = ""), "_ItemTypeID_N", sep = "")
  
  curr_url <- read_html(url) %>% html_nodes("a.item__info-title") %>% html_attr("href")
  
  curr_item <- read_html(paste(paste("https://electronica.mercadolibre.com.ar/reproductores-portatiles-accesorios-parlantes/bluetooth-parlante_Desde_", (i * 50) + 1, sep = ""), "_ItemTypeID_N", sep = "")) %>% html_nodes(".main-title") %>% html_text()
  
  if (length(curr_url) < length(curr_item)) {
    curr_item <- curr_item[1:length(curr_url)]
  } else if (length(curr_url) > length(curr_item)) {
    curr_url <- curr_url[1:length(curr_item)]
  }
  
  # Store the product's url, name, and appearance in the search results in a dataframe called df.
  df <- data.frame(curr_url[1:length(curr_url)], curr_item[1:length(curr_url)])
  df$index <- c((counter):(counter + length(curr_url) - 1))
  parent <- c(1:length(curr_url))
  parent[1:length(curr_url)] <- url
  df$parent <- parent
  
  # Bind that dataframe to a larger dataframe called links with all of the products' links.
  links <- rbind(links, df)
  counter <- counter + length(curr_url)
}

#' 
## ------------------------------------------------------------------------
# This function takes in a part of a website and returns the text at that location.
get_html_text <- function(read_html, node_html) {
  read_html %>% html_nodes(node_html) %>% html_text()
}

# This function takes in a part of a website and returns the URL attached to that part.
get_html_attr_href <- function(read_html, node_html) {
  read_html %>% html_nodes(node_html) %>% html_attr("href")
}

# Any information that the scraper cannot grab is returned as an empty list.  The csv file doesn't know how to represent an empty list, so this function converts empty lists into NA values.
turn_to_na <- function(x) {
    if (length(x) == 0) {
      return (NA)
    } else {
      return (x)
    }
}

# This function takes the list of characteristic names and characteristic values and populates the appropriate locations in the dataframe for each item.
populateCharacteristics <- function(data, rowIndex) {
  itemInfo <- data[rowIndex, ]
  categories <- unlist(itemInfo$`list(char_categories)`)
  values <- unlist(itemInfo$`list(char_values)`)
  for (i in 1:length(categories)) {
    curr_cat <- categories[i]
    index_of_category_in_df <- ncol(data) - length(list_of_characteristics) + which(list_of_characteristics == curr_cat)
    curr_val <- values[i]
    data[rowIndex, index_of_category_in_df] <- curr_val
  }
  return(data)
}

# This function takes in a URL, a search position, and the name of a product and scrapes everything we're looking for from that URL.  Then it returns a row value for that product and attaches all of the data we've scraped plus the search position and name to that row.
scrapeNodes <- function(test, search_position, name) {
  read_html <- read_html(as.character(test))
  char_categories <- get_html_text(read_html, ".specs-item-primary strong")
  char_values <- get_html_text(read_html, ".specs-item-primary span")

  curr_price <- get_html_text(read_html, "#productInfo .price-tag-fraction")
  if (length(curr_price) == 2) {
    orig_price <- curr_price[1]
    curr_price <- curr_price[2]
  } else {
    orig_price <- NA
  }

   # Arg has periods to designate thousands.  Default interpretation is mistakenly that they're decimals, crops out trailing zeroes.
  curr_price <- str_replace(curr_price, "[\\.]", "")
  orig_price <- str_replace(orig_price, "[\\.]", "")
  
  num_sold <- get_html_text(read_html, ".item-conditions")
  
  in_stock <- get_html_text(read_html, ".dropdown-quantity-available")
  if (length(in_stock) == 0) {
    in_stock <- get_html_text(read_html, ".stock-string-last-item")
    if (length(in_stock) != 0) {
      if (gsub("[\t\n$]", "", in_stock) == "Único disponible!") {
        in_stock <- "1"
      }
      if (gsub("[\t\n$]", "", in_stock) == "Último disponible!") {
        in_stock <- "1"
      }
    }
  }

  num_installments <- get_html_text(read_html, ".highlight-info strong")
  
  # Usually NA for Argentina.
  amt_installments <- NA
  
  arrival_time <- get_html_text(read_html, ".black")
  arrival_time <- turn_to_na(arrival_time)
  
  shipping <- get_html_text(read_html, ".green")
  shipping <- shipping[which(shipping != "")]
  shipping <- turn_to_na(shipping)

  if (is.na(shipping)) {
    shipping <- arrival_time
    arrival_time <- NA
  }

  free_return <- get_html_text(read_html, ".benefits-row__title--returns")
  free_return_info <- get_html_text(read_html, ".benefits-row__subtitle")

  review_avg <- get_html_text(read_html, ".review-summary-average")

  num_reviews <- get_html_text(read_html, ".review-summary-average-legend span:nth-child(2)")

  # I use this seller_link in the future to extract the seller information associated with this product.
  seller_link <- get_html_attr_href(read_html, ".card-block-link")
  
  if (length(seller_link) == 0) {
    seller_link <- get_html_attr_href(read_html, ".vip-section-seller-info .ui-view-more__link")
    seller_link <- paste("https://articulo.mercadolibre.com.pe", seller_link, sep = "")
    seller_link <- get_html_attr_href(read_html(as.character(seller_link)), ".feedback-profile-link a")
  }
  
  item_id <- get_html_text(read_html, ".item-info__id-number")
  
  
  full_reviews_link <- get_html_attr_href(read_html, "#reviewsCard .main-section__view-more")
  full_reviews_link <- turn_to_na(full_reviews_link)
  if (!is.na(full_reviews_link)) { full_reviews_link <- paste("https://articulo.mercadolibre.com.ar", full_reviews_link, sep = "") }
  
  five_star <- NA
  four_star <- NA
  three_star <- NA
  two_star <- NA
  one_star <- NA
  
  if (!is.na(full_reviews_link)) {
    reviews_html <- read_html(as.character(full_reviews_link))
    five_star <-  get_html_text(reviews_html, "span#ratingTotal5") %>% as.numeric()
    four_star <-  get_html_text(reviews_html, "span#ratingTotal4") %>% as.numeric()
    three_star <-  get_html_text(reviews_html, "span#ratingTotal3") %>% as.numeric()
    two_star <-  get_html_text(reviews_html, "span#ratingTotal2") %>% as.numeric()
    one_star <-  get_html_text(reviews_html, "span#ratingTotal1") %>% as.numeric()
    five_star <-  turn_to_na(five_star)
    four_star <-  turn_to_na(four_star)
    three_star <-  turn_to_na(three_star)
    two_star <-  turn_to_na(two_star)
    one_star <-  turn_to_na(one_star)
  }
  
  if (length(num_reviews) != 0) {
    if (as.numeric(num_reviews) == 0) {
      five_star <- 0
      four_star <- 0
      three_star <- 0
      two_star <- 0
      one_star <- 0
    }
  }
  
  # This ensure that any information not available for this particular item is stored as NA in our table.
  curr_price <- turn_to_na(curr_price)
  orig_price <- turn_to_na(orig_price)
  num_sold <- turn_to_na(num_sold)
  in_stock <- turn_to_na(in_stock)
  num_installments <- turn_to_na(num_installments)
  shipping <- turn_to_na(shipping)
  free_return <- turn_to_na(free_return)
  free_return_info <- turn_to_na(free_return_info)
  review_avg <- turn_to_na(review_avg)
  num_reviews <- turn_to_na(num_reviews)
  seller_link <- turn_to_na(seller_link)
  name <- turn_to_na(name)
  item_id <- turn_to_na(item_id)
  amt_installments <- turn_to_na(amt_installments)

  df1 <- tibble(search_position, name, list(char_categories), list(char_values), curr_price, orig_price, num_sold, in_stock, num_installments, amt_installments, arrival_time, shipping, free_return, free_return_info, review_avg, num_reviews, seller_link, product_link = test, five_star, four_star, three_star, two_star, one_star, item_id)
  return(df1)
}

#' 
#' 
#' ### For each of the links, scrape info.
#' 
## ----scrape_from_links---------------------------------------------------
df <- NULL

# This loops over the first X links we've collected, one by one, and strips the desired information from them.
print("Scraping from product links.")
list_of_dfs <- vector("list", 3000)
seq <- seq(1, length(links$curr_item.1.length.curr_url..) - 20, 20) # PUT BACK IN TO COMPLETE
# seq <- seq(1, 201, 20) # REMOVE TO COMPLETE
max <- seq[length(seq)] + 20  # PUT BACK IN TO COMPLETE

# This for-loop requests data for 20 links at a time to make the scraping process move faster.
for (i in seq) {
       print(i)
       test0 <- links$curr_url.1.length.curr_url..[i + 0]
       test1 <- links$curr_url.1.length.curr_url..[i + 1]
       test2 <- links$curr_url.1.length.curr_url..[i + 2]
       test3 <- links$curr_url.1.length.curr_url..[i + 3]
       test4 <- links$curr_url.1.length.curr_url..[i + 4]
       test5 <- links$curr_url.1.length.curr_url..[i + 5]
       test6 <- links$curr_url.1.length.curr_url..[i + 6]
       test7 <- links$curr_url.1.length.curr_url..[i + 7]
       test8 <- links$curr_url.1.length.curr_url..[i + 8]
       test9 <- links$curr_url.1.length.curr_url..[i + 9]
       test10 <- links$curr_url.1.length.curr_url..[i + 10]
       test11 <- links$curr_url.1.length.curr_url..[i + 11]
       test12 <- links$curr_url.1.length.curr_url..[i + 12]
       test13 <- links$curr_url.1.length.curr_url..[i + 13]
       test14 <- links$curr_url.1.length.curr_url..[i + 14]
       test15 <- links$curr_url.1.length.curr_url..[i + 15]
       test16 <- links$curr_url.1.length.curr_url..[i + 16]
       test17 <- links$curr_url.1.length.curr_url..[i + 17]
       test18 <- links$curr_url.1.length.curr_url..[i + 18]
       test19 <- links$curr_url.1.length.curr_url..[i + 19]

       # "future" means that the computer will go on to complete the next request before this one is finished, allowing me to make multiple requests to the MercadoLibre server at the same time.  This saves a lot of time!
        future0 <- future ({ scrapeNodes(test0, links$index[i + 0], links$curr_item.1.length.curr_url..[i + 0]) }) %plan% multiprocess
       future1 <- future ({ scrapeNodes(test1, links$index[i + 1], links$curr_item.1.length.curr_url..[i + 1]) }) %plan% multiprocess
       future2 <- future ({ scrapeNodes(test2, links$index[i + 2], links$curr_item.1.length.curr_url..[i + 2]) }) %plan% multiprocess
       future3 <- future ({ scrapeNodes(test3, links$index[i + 3], links$curr_item.1.length.curr_url..[i + 3]) }) %plan% multiprocess
       future4 <- future ({ scrapeNodes(test4, links$index[i + 4], links$curr_item.1.length.curr_url..[i + 4]) }) %plan% multiprocess
       future5 <- future ({ scrapeNodes(test5, links$index[i + 5], links$curr_item.1.length.curr_url..[i + 5]) }) %plan% multiprocess
       future6 <- future ({ scrapeNodes(test6, links$index[i + 6], links$curr_item.1.length.curr_url..[i + 6]) }) %plan% multiprocess
       future7 <- future ({ scrapeNodes(test7, links$index[i + 7], links$curr_item.1.length.curr_url..[i + 7]) }) %plan% multiprocess
       future8 <- future ({ scrapeNodes(test8, links$index[i + 8], links$curr_item.1.length.curr_url..[i + 8]) }) %plan% multiprocess
       future9 <- future ({ scrapeNodes(test9, links$index[i + 9], links$curr_item.1.length.curr_url..[i + 9]) }) %plan% multiprocess
       future10 <- future ({ scrapeNodes(test10, links$index[i + 10], links$curr_item.1.length.curr_url..[i + 10]) }) %plan% multiprocess
       future11 <- future ({ scrapeNodes(test11, links$index[i + 11], links$curr_item.1.length.curr_url..[i + 11]) }) %plan% multiprocess
       future12 <- future ({ scrapeNodes(test12, links$index[i + 12], links$curr_item.1.length.curr_url..[i + 12]) }) %plan% multiprocess
       future13 <- future ({ scrapeNodes(test13, links$index[i + 13], links$curr_item.1.length.curr_url..[i + 13]) }) %plan% multiprocess
       future14 <- future ({ scrapeNodes(test14, links$index[i + 14], links$curr_item.1.length.curr_url..[i + 14]) }) %plan% multiprocess
       future15 <- future ({ scrapeNodes(test15, links$index[i + 15], links$curr_item.1.length.curr_url..[i + 15]) }) %plan% multiprocess
       future16 <- future ({ scrapeNodes(test16, links$index[i + 16], links$curr_item.1.length.curr_url..[i + 16]) }) %plan% multiprocess
       future17 <- future ({ scrapeNodes(test17, links$index[i + 17], links$curr_item.1.length.curr_url..[i + 17]) }) %plan% multiprocess
       future18 <- future ({ scrapeNodes(test18, links$index[i + 18], links$curr_item.1.length.curr_url..[i + 18]) }) %plan% multiprocess
       future19 <- future ({ scrapeNodes(test19, links$index[i + 19], links$curr_item.1.length.curr_url..[i + 19]) }) %plan% multiprocess
       
       list_of_dfs[[i + 0]] <- value(future0)
       list_of_dfs[[i + 1]] <- value(future1)
       list_of_dfs[[i + 2]] <- value(future2)
       list_of_dfs[[i + 3]] <- value(future3)
       list_of_dfs[[i + 4]] <- value(future4)
       list_of_dfs[[i + 5]] <- value(future5)
       list_of_dfs[[i + 6]] <- value(future6)
       list_of_dfs[[i + 7]] <- value(future7)
       list_of_dfs[[i + 8]] <- value(future8)
       list_of_dfs[[i + 9]] <- value(future9)
       list_of_dfs[[i + 10]] <- value(future10)
       list_of_dfs[[i + 11]] <- value(future11)
       list_of_dfs[[i + 12]] <- value(future12)
       list_of_dfs[[i + 13]] <- value(future13)
       list_of_dfs[[i + 14]] <- value(future14)
       list_of_dfs[[i + 15]] <- value(future15)
       list_of_dfs[[i + 16]] <- value(future16)
       list_of_dfs[[i + 17]] <- value(future17)
       list_of_dfs[[i + 18]] <- value(future18)
       list_of_dfs[[i + 19]] <- value(future19)
}

for (i in max:length(links$curr_item.1.length.curr_url..)) {
   test <- links$curr_url.1.length.curr_url..[i]
   list_of_dfs[[i]] <- scrapeNodes(test, links$index[i], links$curr_item.1.length.curr_url..[i])
}  # PUT BACK IN TO COMPLETE

# This takes all of our individual rows and combines them into one large dataframe.
df <- bind_rows(list_of_dfs)

# This transforms our lists of characteristics for each individual product into one master list.
list_of_characteristics <- df$`list(char_categories)`
list_of_characteristics <- unlist(list_of_characteristics, recursive = FALSE)
list_of_characteristics <- list_of_characteristics[!duplicated(list_of_characteristics)]
df[, list_of_characteristics] <- NA

# Then this for-loop populates the characteristics columns with values.
# Note:  I should be able to write this more concisely with sapply later.
for (i in 1:length(links$curr_item.1.length.curr_url..)) {
  df <- populateCharacteristics(df, i)
}

df$`list(char_categories)` <- NULL
df$`list(char_values)` <- NULL

# Remove sparsely populated columns (less than 10%).
df2 <- apply(df, 2, function(x) { 
      if (length(which(is.na(x) == FALSE)) < 0.1 * length(x)) {
        FALSE 
      } else {
        TRUE
      } 
  })
df2[1:21] <- TRUE
df <- df[, which(df2 == TRUE)]

backup_df <- df

write_csv(df, date %>% paste("BluetoothSpeakers_Prod_Arg_Raw.csv", sep = ""))

#' 
#' ### Collect seller info.
## ------------------------------------------------------------------------
# Some products have sellers in common.  This condenses all of the sellers into a list of unique, non-repeated URLs to their seller profiles.
list_of_seller_links <- df$seller_link
list_of_seller_links <- list_of_seller_links[!duplicated(list_of_seller_links)]
list_of_seller_links <- list_of_seller_links[!is.na(list_of_seller_links)]
list_of_dfs <- vector("list", 3000)
seller_df <- NULL
file_connection <- file(paste(date, "Arg_output.txt", sep = ""), open = "wt")
writeLines(c("List of URLs that encountered errors and their errors:"), file_connection)

# Iterates over all of the unique seller links to get their info.
for (i in 1:length(list_of_seller_links)) {
  print(i)
  link <- list_of_seller_links[i]
  
  jumptonext <- TRUE
  tryCatch(
    {
      read_html <- read_html(as.character(link))
      jumptonext <- FALSE
    },
    error=function(cond) {
      errorURLs <- c("-----------------", paste("URL:", link))
      errorURLs <- c(errorURLs, paste("The above link failed once with error: ", cond, sep = ""))
      writeLines(errorURLs, file_connection)
      jumptonext <- TRUE
    }
  )
  
  if (jumptonext) {
    tryCatch(
      {
        errorURLs <- c("Running it again...")
        read_html <- read_html(as.character(link))
        jumptonext <- FALSE
        errorURLs <- c(errorURLs, "It worked.")
        writeLines(errorURLs, file_connection)
      },
      error=function(cond) {
        errorURLs <- c(paste("Running it again...", "The above link failed twice.  Second error: ", cond, sep = ""))
        writeLines(errorURLs, file_connection)
        jumptonext <- TRUE
      }
    )
  }
  
  if (jumptonext) {
    errorURLs <- c("Failed, so skipping to next iteration.")
    writeLines(errorURLs, file_connection)
    next()
  }
  
  seller_name <- get_html_text(read_html, "#store-info__name")
  if (length(seller_name) == 0) {
    seller_name <- get_html_text(read_html, "#brand")
  }
  time_operating <- get_html_text(read_html, ".experience span span")
  units_of_time_operating <- get_html_text(read_html, ".experience > span")
  amt_sold <- get_html_text(read_html, ".seller-info__subtitle-sales span span:nth-child(1)")
  timeframe_of_amt_sold <- get_html_text(read_html, ".seller-info__subtitle-sales > span")
  leader_status <- get_html_text(read_html, ".leader-status__icon")
  top_descriptors <- get_html_text(read_html, "h2")
  top_descriptor1 <- NA
  top_descriptor2 <- NA
  if (length(top_descriptors) == 1) {
      top_descriptor1 <- top_descriptors[1]
  } else if (length(top_descriptors) == 2) {
      top_descriptor1 <- top_descriptors[1]
      top_descriptor2 <- top_descriptors[2]
  }
  num_reviews <- get_html_text(read_html, ".total")
  num_neg_reviews <- get_html_text(read_html, ".buyers-feedback-bar--negative #feedback_good")
  num_neutral_reviews <- get_html_text(read_html, ".buyers-feedback-bar--neutral #feedback_good")
  num_pos_reviews <- get_html_text(read_html, ".buyers-feedback-bar--positive #feedback_good")
  location <- get_html_text(read_html, ".location-subtitle")
  
  seller_name <- turn_to_na(seller_name)
  link <- turn_to_na(link)
  time_operating <- turn_to_na(time_operating)
  amt_sold <- turn_to_na(amt_sold)
  leader_status <- turn_to_na(leader_status)
  top_descriptor1 <- turn_to_na(top_descriptor1)
  top_descriptor2 <- turn_to_na(top_descriptor2)
  num_neg_reviews <- turn_to_na(num_neg_reviews)
  num_reviews <- turn_to_na(num_reviews)
  num_neutral_reviews <- turn_to_na(num_neutral_reviews)
  num_pos_reviews <- turn_to_na(num_pos_reviews)
  location <- turn_to_na(location)
  units_of_time_operating <- turn_to_na(units_of_time_operating)
  timeframe_of_amt_sold <- turn_to_na(timeframe_of_amt_sold)

  seller_df1 <- tibble(seller_name, link, time_operating, units_of_time_operating, amt_sold, leader_status, top_descriptor1, top_descriptor2, num_reviews, num_neg_reviews, num_neutral_reviews, num_pos_reviews, location, timeframe_of_amt_sold)
  list_of_dfs[[i]] <- seller_df1
}
close(file_connection, type = "wt")
seller_df <- bind_rows(list_of_dfs)
backup_seller_df <- seller_df
write_csv(seller_df, date %>% paste("BluetoothSpeakers_Sell_Arg_Raw.csv", sep = ""))

#' 
#' ### Cleaning the data.
## ------------------------------------------------------------------------
seller_df <- backup_seller_df
df <- backup_df

# Extracts number from "(### disponibles)"
df$in_stock <- sapply(df$in_stock, function(x) { as.numeric(str_extract_all(x, "[0-9]+")[[1]][1]) })

# Extracts units in time_operating (either months or years).
# Note: this may run into an error if anything in the vector is NA.
seller_df$units_of_time_operating <- sapply(seller_df$units_of_time_operating, function(x) { strsplit(x, " ")[[1]][2] })

#units_of_time_operating_values <- seller_df$units_of_time_operating[!duplicated(seller_df$units_of_time_operating)]
#seller_df$units_of_time_operating <- sapply(seller_df$units_of_time_operating, function(x) { return ( if (is.na(x)) { which(is.na(units_of_time_operating_values)) } else { which(x == units_of_time_operating_values) }) })

# Extracts units in time_operating (either months or years).
# Note: this may run into an error if anything in the vector is NA.
seller_df$units_timeframe_of_amt_sold <- sapply(seller_df$timeframe_of_amt_sold, function(x) { strsplit(x, " ")[[1]][6] })
#seller_df$amt_sold <- sapply(seller_df$timeframe_of_amt_sold, function(x) { strsplit(x, " ")[[1]][1] })
seller_df$timeframe_of_amt_sold <- sapply(seller_df$timeframe_of_amt_sold, function(x) { strsplit(x, " ")[[1]][7] })

if (nrow(seller_df[which(seller_df$units_timeframe_of_amt_sold == "año"),]) > 0) {
    seller_df[which(seller_df$units_timeframe_of_amt_sold == "año"),]$timeframe_of_amt_sold <- "años"
    seller_df[which(seller_df$units_timeframe_of_amt_sold == "año"),]$units_timeframe_of_amt_sold <- 1
}

if (nrow(seller_df[which(seller_df$units_timeframe_of_amt_sold == "mes"),]) > 0) {
    seller_df[which(seller_df$units_timeframe_of_amt_sold == "mes"),]$timeframe_of_amt_sold <- "meses"
    seller_df[which(seller_df$units_timeframe_of_amt_sold == "mes"),]$units_timeframe_of_amt_sold <- 1
}

#timeframe_of_amt_sold_values <- seller_df$timeframe_of_amt_sold[!duplicated(seller_df$timeframe_of_amt_sold)]

#seller_df$timeframe_of_amt_sold <- sapply(seller_df$timeframe_of_amt_sold, function(x) { return ( if (is.na(x)) { which(is.na(timeframe_of_amt_sold_values)) } else { which(x == timeframe_of_amt_sold_values) }) })

# Extracts integers from num_sold data.
anon <- function(x) {
   t <- substr(gsub("[[:space:]]", "", x),1,1000)
   if (!is.na(t)) {
     if (t == "Nuevo") {
       return(0)
     }
   }
   return(as.numeric(str_extract_all(t, "[0-9]+")[[1]][1]))
}
df$num_sold <- sapply(df$num_sold, anon)
  
# Split around "?" to isolate MercadoLider status for each seller.
seller_df$leader_status <- sapply(seller_df$leader_status, function(x) { strsplit(x, "Â¡")[[1]][1] })

# Extracts number from Bad, Good, and Neutral review counts for sellers.
seller_df$num_neg_reviews <- sapply(seller_df$num_neg_reviews, function(x) { as.numeric(str_extract_all(x, "[0-9]+")[[1]]) })
seller_df$num_pos_reviews <- sapply(seller_df$num_pos_reviews, function(x) { as.numeric(str_extract_all(x, "[0-9]+")[[1]]) })
seller_df$num_neutral_reviews <- sapply(seller_df$num_neutral_reviews, function(x) { as.numeric(str_extract_all(x, "[0-9]+")[[1]]) })

# Converts string fields to integer identifiers.  For example, all products with "llega manana" are assigned a 1 instead of "llega manana".
#arrival_time_values <- df$arrival_time[!duplicated(df$arrival_time)]
#df$arrival_time <- sapply(df$arrival_time, function(x) { return ( if (is.na(x)) { which(is.na(arrival_time_values)) } else { which(x == arrival_time_values) }) })

#shipping_values <- df$shipping[!duplicated(df$shipping)]
#df$shipping <- sapply(df$shipping, function(x) { return ( if (is.na(x)) { which(is.na(shipping_values)) } else { which(x == shipping_values) }) })

#free_return_values <- df$free_return[!duplicated(df$free_return)]
#df$free_return <- sapply(df$free_return, function(x) { return ( if (is.na(x)) { which(is.na(free_return_values)) } else { which(x == free_return_values) }) })

#free_return_info_values <- df$free_return_info[!duplicated(df$free_return_info)]
#df$free_return_info <- sapply(df$free_return_info, function(x) { return ( if (is.na(x)) { which(is.na(free_return_info_values)) } else { which(x == free_return_info_values) }) })

df$review_avg <- as.numeric(df$review_avg)
df$num_reviews <- as.numeric(df$num_reviews)

# x[17] is the five_star
# x[18] is the four_star
# x[19] is the three_star
# x[20] is the two_star
# x[21] is the one_star

# First, set all star values to zero if num_reviews == 1 or == 2.
df[which(df$num_reviews == 1 | df$num_reviews == 2), c(17:21)] <- 0

# Then, adjust individually as necessary.
df[which(df$num_reviews == 1 & df$review_avg == 1), 21] <- 1
df[which(df$num_reviews == 1 & df$review_avg == 2), 20] <- 1
df[which(df$num_reviews == 1 & df$review_avg == 3), 19] <- 1
df[which(df$num_reviews == 1 & df$review_avg == 4), 18] <- 1
df[which(df$num_reviews == 1 & df$review_avg == 5), 17] <- 1

df[which(df$num_reviews == 2 & df$review_avg == 1), 21] <- 2

df[which(df$num_reviews == 2 & df$review_avg == 1.5), 21] <- 1
df[which(df$num_reviews == 2 & df$review_avg == 1.5), 20] <- 1

df[which(df$num_reviews == 2 & df$review_avg == 2), 20] <- 2

df[which(df$num_reviews == 2 & df$review_avg == 2.5), 20] <- 1
df[which(df$num_reviews == 2 & df$review_avg == 2.5), 19] <- 1

df[which(df$num_reviews == 2 & df$review_avg == 3), 19] <- 2

df[which(df$num_reviews == 2 & df$review_avg == 3.5), 19] <- 1
df[which(df$num_reviews == 2 & df$review_avg == 3.5), 18] <- 1

df[which(df$num_reviews == 2 & df$review_avg == 4), 18] <- 2

df[which(df$num_reviews == 2 & df$review_avg == 4.5), 18] <- 1
df[which(df$num_reviews == 2 & df$review_avg == 4.5), 17] <- 1

df[which(df$num_reviews == 2 & df$review_avg == 5), 17] <- 2

df$amt_installments <- NA  # The scraper for these is dragging up other nodes that do not signify the amount paid per installment.

write_csv(df, date %>% paste("BluetoothSpeakers_Prod_Arg.csv", sep = ""))
write_csv(seller_df, date %>% paste("BluetoothSpeakers_Sell_Arg.csv", sep = ""))

#' 
