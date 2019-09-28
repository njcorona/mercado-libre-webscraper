library(httr)
library(jsonlite)
library(lubridate)

url  <- "https://api.mercadolibre.com"

# Only gives 50 results at a time, so have to make API call in offset increments of 50.
path <- "/sites/MCO/search?q=bluetooth-audifonos&access_token=$ACCESS_TOKEN&offset=50"

raw.result <- GET(url = url, path = path)

this.raw.content <- rawToChar(raw.result$content)

this.content <- fromJSON(this.raw.content)

this.content$results[[3]]