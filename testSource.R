tryCatch({
  print(Sys.time())
  Sys.sleep(120)
  print(Sys.time())
  Sys.sleep(120)
  print(Sys.time())
  Sys.sleep(120)
  print(Sys.time())
  Sys.sleep(120)
  print(Sys.time())
  Sys.sleep(120)
  print(Sys.time())
  Sys.sleep(120)
})

# This never timed out.  Even when being sourced.  I do not understand why I was running into timeout errors
# on my other PC.