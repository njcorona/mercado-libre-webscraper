# Add tryCatch lines.
filepath <- "C:/Users/njcor/Documents/GitHub/mercado-libre/BluetoothSpeakers/BluetoothSpeakers_Arg.R"
text <- readLines(filepath)

text[which(text == "seller_df <- backup_seller_df")] <- paste("tryCatch({", text[which(text == "seller_df <- backup_seller_df")], sep = "\n")
text[which(text == ".csv\", sep = \"\"))
print(Sys.time())")] <- paste(text[which(text == ".csv\", sep = \"\"))
print(Sys.time())")], "}, warning = function(w) {
    print(\"Got a warning!\")
}, error = function(e) {
    print(\"Got an error: %s\",e)
}, finally = {
    print(\"Finished cleaning.\")
})", sep = "\n")




writeLines(text, filepath)
