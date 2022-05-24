# function that extracts country names (out of a vector of world countries),
# municipalities (out of a given vector of municipalities), and parishes
# (out of a given vector of parishes)
# 
# appends identified information to the given input df
# 
# Packages required:
# library(flickRgeotag)
# library(stringr)
# library(countrycode)


get_profile_location <- function(df, municip_vector, parish_vector){
  loc_profile <- c()
  for (user in df$owner){
    info <- flickr.people.getInfo(user)
    loc_profile <- c(loc_profile, info$location)
  }
  loc_profile <- cbind(loc_profile)
  
  loc_profile_municipality <- c()
  loc_profile_country <- c()
  loc_profile_parish <- c()
  
  for (row in 1:nrow(loc_profile)){
    
    # check for "Copenhagen" or a municipality from the list
    # (to be adjusted if applied in a different context)
    municipality <- NA
    if (grepl("Copenhagen", loc_profile[row], fixed=TRUE)){
      municipality <- "København"
    } 
    if (grepl(paste(municip_vector,collapse="|"), loc_profile[row])){
      municipality <- str_extract(loc_profile[row], paste(municip_vector,collapse="|"))
    }
    
    # check for a country from the list of world countries, also catch "Danmark" and "USA" 
    # (to be adjusted if applied in a different context)
    country <- str_extract(loc_profile[row], paste(unique(countryname_dict$country.name.en),collapse="|"))
    if (grepl("Danmark", loc_profile[row], fixed=TRUE)){
      country <- "Denmark"
    }
    if (grepl("USA", loc_profile[row], fixed=TRUE)){
      country <- "United States"
    }
    
    # check for a parish from the list of Danish parishes
    parish <- str_extract(loc_profile[row], paste(parish_vector,collapse="|"))
    
    loc_profile_municipality <- c(loc_profile_municipality, municipality)
    loc_profile_country <- c(loc_profile_country, country)
    loc_profile_parish <- c(loc_profile_parish, parish)
  }
  df$loc_profile <- loc_profile
  df$loc_profile_country <- cbind(loc_profile_country)
  df$loc_profile_municipality <- cbind(loc_profile_municipality)
  df$loc_profile_parish <- cbind(loc_profile_parish)
  return(df)
}
