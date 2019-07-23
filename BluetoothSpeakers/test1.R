print("test1")
print(paste(Sys.time(), " is when test1 starts hanging", sep = ""))
Sys.sleep(6)
stop("ERROR HERE")