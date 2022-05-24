# function extracting parish codes and names from users' home coordinates,
# using the Dataforsyningen API (works for Danish parishes only)
# 
# Required package:
# library(jsonlite)


get_parishes <- function(df){
  owner <- c()
  parish_codes <- c()
  parish_names <- c()
  for (i in 1:nrow(df)){
    user <- df[i, "owner"]
    x <- df[i, "userloc_lon"]
    y <- df[i, "userloc_lat"]
    url <- paste0("https://api.dataforsyningen.dk/sogne/reverse?x=", x, "&y=", y)
    
    parish_code <- tryCatch({
      fromJSON(url)$kode
    }, 
    error = function(e){
      return(NA)
    })
    
    parish_name <- tryCatch({
      fromJSON(url)$navn
    }, 
    error = function(e){
      return(NA)
    })
    
    owner <- c(owner, user)
    parish_codes <- c(parish_codes, parish_code)
    parish_names <- c(parish_names, parish_name)
    results <- data.frame(owner, parish_codes, parish_names)
  }
  return(results)
}
