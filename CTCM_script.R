#####################################
####### INTRO #######################

# This script includes the full CTCM analysis undertaken for the site of Jaegersborg Dyrehave
# in Denmark, from data extraction until final model estimation.

library(photosearcher)
# api_key <- "..."     # get an API key at https://www.flickr.com/services/apps/create/apply/  
# and copy paste it between the quotation marks
library(flickRgeotag)
library(dplyr)
library(stringr)
library(sf)
library(pracma)
library(rnaturalearth)
library(rnaturalearthdata)
library(osrm)
library(countrycode)
library(jsonlite)
library(MASS)
library(VGAM)
library(glmmADMB)
library(glmmTMB)
library(rvest)
library(xml2)
library(ggplot2)
library(lubridate)
library(zoo)
library(lmtest)
library(stargazer)
library(patchwork)

source("debugged_functions.R")
source("get_profile_location.R")
source("get_home_location.R")
source("get_parishes.R")

#-------------------------------------------------------------------------------------------------------------------------------------------------
#####################################
####### DATA EXTRACTION #############

tags <- c("jaegersborg", "j%C3%A6gersborg", "dyrehave", "dyrehaven", "jaegersborgdyrehave", "j%C3%A6gersborgdyrehave", "deer park", "deer")

data <- c()
count <- 1
while (count < 6){
  photo_data <- photo_search(mindate_taken = "2004-01-01",
                             maxdate_taken = "2021-12-31",
                             tags = tags)
  data <- rbind(data, photo_data)
  rm(photo_data)
  count <- count + 1
}

# reduce to unique photos
data <- data[!duplicated(data[ , "id"]), ] 

# delete columns that are not needed
data <- data[,c("id", "owner", "title", "datetaken", "tags", "latitude", "longitude", "accuracy")]

# delete pictures tagged outside a rectangular box around the study site
data_geo <- data[data$longitude > 12.53 & data$longitude < 12.6 & data$latitude > 55.765 & data$latitude < 55.84,]

rm(data, count)

# construct date and year columns for PUD and weights
data_geo$datetaken_notime <- substr(data_geo$datetaken, 0, 10)
data_geo$year <- substr(data_geo$datetaken, 0, 4)
data_geo$month <- substr(data_geo$datetaken, 6, 7)

# reduce to PUD
PUD_data_geo <- data_geo[!duplicated(data_geo[ , c("owner", "datetaken_notime")]), ] 
rm(data_geo)

#-------------------------------------------------------------------------------------------------------------------------------------------------
#####################################
####### NO AND DATES OF VISITS ######

# reduce to users
user_data_geo <- PUD_data_geo[!duplicated(PUD_data_geo[ , "owner"]), ] 

# number of photos taken at study site
no_of_photos <- data.frame(table(data_geo$owner))
colnames(no_of_photos) <- c("owner", "no_of_photos")
user_data_geo <- merge(user_data_geo, no_of_photos, by = "owner")

no_of_PUD <- as.data.frame(table(PUD_data_geo$owner))
colnames(no_of_PUD) <- c("owner", "no_of_PUD")

PUD <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("owner", "PUD"))
PUD_list <- c()
for (owner in user_data_geo$owner){
  photos <- PUD_data_geo[PUD_data_geo$owner == owner,]
  PUD_user <- list(sort(c(photos$datetaken_notime)))
  PUD_list <- c(PUD_list, PUD_user)
  PUD[nrow(PUD) + 1,] = c(owner, NA)
}
PUD$PUD <- PUD_list

user_data_geo <- merge(user_data_geo, no_of_PUD, by = "owner")
user_data_geo <- merge(user_data_geo, PUD, by = "owner")
rm(no_of_PUD, photos, PUD_list, PUD_user, PUD, owner)

# some statistics on the number of PUDs
range(user_data_geo$no_of_PUD)
median(user_data_geo$no_of_PUD)
sort(table(user_data_geo$no_of_PUD), decreasing = T)

# check for consecutive day trips and remove consecutive PUD
user_data_geo$min_diff_days <- NA
for (i in 1:nrow(user_data_geo)){
  user_data_geo$no_of_PUD_no_consecutives[i] <- user_data_geo$no_of_PUD[i]
  if (user_data_geo$no_of_PUD[[i]] > 1){
    diff_list <- c()
    for (j in 1:(length(user_data_geo$PUD[[i]])-1)){
      diff_list <- c(diff_list, as.numeric(difftime(as.Date(user_data_geo$PUD[[i]][j+1]), as.Date(user_data_geo$PUD[[i]][j]), units = "days")))
    }
    indices <- c(which(diff_list == 1), which(diff_list == 1)+1)
    if (length(indices)>0){
      indices <- unique(indices)
      user_data_geo$PUD[[i]] <- user_data_geo$PUD[[i]][-indices]
      user_data_geo$no_of_PUD_no_consecutives[i] <- length(user_data_geo$PUD[[i]])
    }
    user_data_geo$min_diff_days[i] <- min(diff_list)
  }
}

# drop users who do not have any PUDs left after dropping consecutives
user_data_geo <- user_data_geo[!user_data_geo$no_of_PUD_no_consecutives == 0,]


# transform PUD column to be able to export df as csv
for (i in 1:nrow(user_data_geo)){
  user_data_geo$PUD[i] <-  paste(unlist(user_data_geo$PUD[i]), collapse = ", ")
}
user_data_geo$PUD <- as.character(user_data_geo$PUD)

# check for most popular days
sort(table(PUD_data_geo$datetaken_notime), descending = T)


#-------------------------------------------------------------------------------------------------------------------------------------------------
#####################################
####### HOME LOCATIONS ##############

# import list and shapefiles of municipalities in DNK, SWE and DEU
DNK <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_DNK_2_sf.rds"))[,c("NAME_0", "NAME_1", "NAME_2", "geometry")]
SWE <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_SWE_2_sf.rds"))[,c("NAME_0", "NAME_1", "NAME_2", "geometry")]
DEU <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_DEU_2_sf.rds"))[,c("NAME_0", "NAME_1", "NAME_2", "geometry")]
municip_polys <- rbind(DNK, DEU, SWE)
rm(DNK, DEU, SWE)
municip_polys <- st_set_precision(st_transform(municip_polys, 4326), 1e8)
colnames(municip_polys) <- c("country", "region", "municip", "geometry")


# create list of parishes 
parishes_list <- cbind(fromJSON("https://api.dataforsyningen.dk/sogne")$navn)

# apply the get_profile_location function on all users to get (country and municipality) location from profile info, append to df
user_data_geo <- get_profile_location(user_data_geo, municip_polys$municip, parishes_list)

# some statistics on the profile location info
nrow(user_data_geo[user_data_geo$loc_profile != "",])
nrow(user_data_geo[!is.na(user_data_geo$loc_profile_country),])
sort(table(user_data_geo$loc_profile_country), decreasing = TRUE)
nrow(user_data_geo[!is.na(user_data_geo$loc_profile_municipality),])
sort(table(user_data_geo$loc_profile_municipality), decreasing = TRUE)
nrow(user_data_geo[!is.na(user_data_geo$loc_profile_parish),])

# apply the get_home_location function on all users to obtain home country and municipality
home_locations <- lapply(user_data_geo$owner, get_home_location, municip_polys = municip_polys)
home_locations <- bind_rows(home_locations)

# manually move locations of three users that are a few meters out in the water and can therefore not be assigned to a parish
home_locations$userloc_lat[home_locations$owner == "7818803@N04"] <- 55.776700
home_locations$userloc_lon[home_locations$owner == "7818803@N04"] <- 12.591975
home_locations$userloc_lat[home_locations$owner == "78597264@N00"] <- 55.676495
home_locations$userloc_lon[home_locations$owner == "78597264@N00"] <- 12.576596
home_locations$userloc_lat[home_locations$owner == "99830892@N04"] <- 55.692427
home_locations$userloc_lon[home_locations$owner == "99830892@N04"] <- 12.609012

# get parishes by sending coordinates to the dataforsyningen API
parishes <- get_parishes(home_locations)

home_locations <- merge(home_locations, parishes, by = "owner")

# add all info to user dataframe
users_with_loc <- merge(user_data_geo, home_locations, by = "owner")
write.csv(users_with_loc, "Files/users_with_loc.csv")

# drop users who only have photos or PUDs in relation to the study site
users_with_loc <- users_with_loc[!(users_with_loc$no_of_photos == users_with_loc$total_no_photos | users_with_loc$no_of_PUD == users_with_loc$total_no_PUD),]

# some statistics on the predicted home locations
sort(table(users_with_loc$home_country), decreasing = T)
sort(table(users_with_loc$loc_profile_country[users_with_loc$home_country == "Denmark",]), decreasing = T)
users_with_loc_DK_SE_DE <- users_with_loc[users_with_loc$home_country == "Denmark" | users_with_loc$home_country == "Sweden" | users_with_loc$home_country == "Germany",]
sort(table(users_with_loc_DK_SE_DE$municip), decreasing = T)
sort(table(users_with_loc$loc_profile_municipality[users_with_loc$municip == "København",]), decreasing = T)
sort(table(users_with_loc$municip[which(users_with_loc$loc_profile_municipality == "København")]), decreasing = T)

# filter out Danish users
users_with_loc_DK <- users_with_loc[users_with_loc$home_country == "Denmark",]

sort(table(users_with_loc_DK$parish_names), decreasing = T)
length(table(users_with_loc_DK$parish_names))

rm(home_locations, municip_polys, parishes, user_data_geo, PUD_data_geo, users_with_loc, parishes_list)

#-------------------------------------------------------------------------------------------------------------------------------------------------
#####################################
####### CALCULATE DISTANCES #########

# create destination location df
destination <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("location", "lon", "lat"))
destination$location <- "destination"
destination$lon <- "12.589213"
destination$lat <- "55.795084"

# add substitute sites
sub1 <- c("sub1", "12.575466", "55.602043")   # vestamager
sub2 <- c("sub2", "12.051907", "56.023984")   # tisvilde
sub3 <- c("sub3", "12.282529", "55.984908")   # gribskov

destination <- rbind(destination, sub1, sub2, sub3)


# get travel times from user home locations to destination and substitutes, add to users_with_loc df
travel_info <- osrmTable(src = users_with_loc_DK[, c("owner", "userloc_lon", "userloc_lat")],
                         dst = destination,
                         measure =  c("duration", "distance"), 
                         osrm.profile = "car")
traveltime <- data.frame(row.names(travel_info$durations), travel_info$durations[,"destination"], travel_info$durations[,"sub1"], travel_info$durations[,"sub2"], travel_info$durations[,"sub3"])
traveldist <- data.frame(row.names(travel_info$distances), travel_info$distances[,"destination"], travel_info$distances[,"sub1"], travel_info$distances[,"sub2"], travel_info$distances[,"sub3"])
colnames(traveltime) <- c("owner", "traveltime_dest", "traveltime_sub1", "traveltime_sub2", "traveltime_sub3")
colnames(traveldist) <- c("owner", "traveldist_dest", "traveldist_sub1", "traveldist_sub2", "traveldist_sub3")

# only keep distance and time for closest substitute site
travel_info <- merge(traveltime, traveldist, by = "owner")

travel_info$traveldist_sub <- pmin(travel_info$traveldist_sub1, travel_info$traveldist_sub2, travel_info$traveldist_sub3)
travel_info$sub_used <- colnames(travel_info[,c("traveldist_sub1", "traveldist_sub2", "traveldist_sub3")])[apply(travel_info[,c("traveldist_sub1", "traveldist_sub2", "traveldist_sub3")],1,which.min)]
travel_info$sub_used <- paste0("traveltime_", substr(travel_info$sub_used, 12, 15))


for (i in 1:nrow(travel_info)){
  travel_info$traveltime_sub[i] <- travel_info[i,travel_info$sub_used[i]]
}
travel_info$traveltime_sub <- as.numeric(travel_info$traveltime_sub)

travel_info <- travel_info[, c("owner", "traveltime_dest", "traveldist_dest", "traveltime_sub", "traveldist_sub", "sub_used")]

sort(table(travel_info$sub_used), decreasing = T)


# setting up assumptions and values

driving_cost <- 1 # weighted average of marginal driving costs per km per person in DKK, assuming 1.6 people/car
cycling_speed <- 14.4 # in km/h
walking_speed <- 5.3 # in km/h
time_cost <- 88.63 # weighted average of the value of time in 2020 DKK per hour

# calculate travelcost based on assumptions and values
travelcost1 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("owner", "travelcost_dest1", "travelcost_sub1"))
for (user in travel_info$owner){
  user <- user
  distance_dest <- travel_info[which(travel_info$owner==user),"traveldist_dest"]/1000
  drive_time_dest <- travel_info[which(travel_info$owner==user),"traveltime_dest"]/60
  distance_sub <- travel_info[which(travel_info$owner==user),"traveldist_sub"]/1000
  drive_time_sub <- travel_info[which(travel_info$owner==user),"traveltime_sub"]/60
  
  car_cost_dest <- driving_cost*distance_dest + time_cost*drive_time_dest
  bike_cost_dest <- (distance_dest/cycling_speed)*time_cost
  foot_cost_dest <- (distance_dest/walking_speed)*time_cost
  
  car_cost_sub <- driving_cost*distance_sub + time_cost*drive_time_sub
  bike_cost_sub <- (distance_sub/cycling_speed)*time_cost
  foot_cost_sub <- (distance_sub/walking_speed)*time_cost
  
  if (distance_dest < 1){
    user_travelcost_dest <- 2*(0.14*car_cost_dest + 0.14*bike_cost_dest + 0.72*foot_cost_dest)
  } else if (distance_dest < 3){
    user_travelcost_dest <- 2*(0.27*car_cost_dest + 0.24*bike_cost_dest + 0.49*foot_cost_dest)
  } else if (distance_dest < 5){
    user_travelcost_dest <- 2*(0.51*car_cost_dest + 0.28*bike_cost_dest + 0.21*foot_cost_dest)
  } else if (distance_dest < 10){
    user_travelcost_dest <- 2*(0.76*car_cost_dest + 0.24*bike_cost_dest)
  } else {
    user_travelcost_dest <- 2*car_cost_dest
  }
  
  # same for sub site
  if (distance_sub < 1){
    user_travelcost_sub <- 2*(0.14*car_cost_sub + 0.14*bike_cost_sub + 0.72*foot_cost_sub)
  } else if (distance_sub < 3){
    user_travelcost_sub <- 2*(0.27*car_cost_sub + 0.24*bike_cost_sub + 0.49*foot_cost_sub)
  } else if (distance_sub < 5){
    user_travelcost_sub <- 2*(0.51*car_cost_sub + 0.28*bike_cost_sub + 0.21*foot_cost_sub)
  } else if (distance_sub < 10){
    user_travelcost_sub <- 2*(0.76*car_cost_sub + 0.24*bike_cost_sub)
  } else {
    user_travelcost_sub <- 2*car_cost_sub
  }
  travelcost1[nrow(travelcost1)+1,] <- c(user, user_travelcost_dest, user_travelcost_sub)
}

# calculate alternative travelcost assuming 100% car
travelcost2 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("owner", "travelcost_dest2", "travelcost_sub2"))
for (user in travel_info$owner){
  user <- user
  distance_dest <- travel_info[which(travel_info$owner==user),"traveldist_dest"]/1000
  drive_time_dest <- travel_info[which(travel_info$owner==user),"traveltime_dest"]/60
  distance_sub <- travel_info[which(travel_info$owner==user),"traveldist_sub"]/1000
  drive_time_sub <- travel_info[which(travel_info$owner==user),"traveltime_sub"]/60
  
  user_travelcost_dest <- 2*(driving_cost*distance_dest + time_cost*drive_time_dest)
  user_travelcost_sub <- 2*(driving_cost*distance_sub + time_cost*drive_time_sub)
  
  travelcost2[nrow(travelcost2)+1,] <- c(user, user_travelcost_dest, user_travelcost_sub)
}

travelcost <- merge(travelcost1, travelcost2, by = "owner")
travelcost$travelcost_dest1 <- as.numeric(travelcost$travelcost_dest1)
travelcost$travelcost_dest2 <- as.numeric(travelcost$travelcost_dest2)
travelcost$travelcost_sub1 <- as.numeric(travelcost$travelcost_sub1)
travelcost$travelcost_sub2 <- as.numeric(travelcost$travelcost_sub2)

travel_info <- merge(travel_info, travelcost, by = "owner")

users_with_travel_info <- merge(users_with_loc_DK, travel_info, by = "owner")

rm(travel_info, traveltime, traveldist, destination, travelcost, travelcost1, travelcost2, sub1, sub2, sub3, users_with_loc_DK)

#-------------------------------------------------------------------------------------------------------------------------------------------------
#####################################
####### CONTROL VARIABLES ###########

# income data (download from https://www.ae.dk/analyse/2021-04-i-det-rigeste-lokalomraade-tjener-de-fem-gange-mere-end-bunden)
income_data <- read.csv("Control_Variables/income_data.csv", sep = ";", encoding = "UTF-8")

for (i in 1:nrow(users_with_travel_info)){
  if (!is.na(users_with_travel_info$parish_codes[i]) && users_with_travel_info$parish_codes[i] != 9228 && users_with_travel_info$parish_codes[i] != 9323){
    # rescale by dividing by 1000
    users_with_travel_info$income[i] <- income_data$INDKOMST.GNS.ALLE[income_data$SOGNEKODE == users_with_travel_info$parish_codes[i]]/1000
  } else{
    users_with_travel_info$income[i] <- NA
  }
}

# data for hulsig sogn is missing
# manually add data for foellenslev sogn which has a slightly different name in the income dataset
users_with_travel_info$income[users_with_travel_info$owner == "33623505@N03"] <- income_data$INDKOMST.ALLE[income_data$SOGNEKODE == 7247]/1000

# age data (downloaded from https://www.statistikbanken.dk/statbank5a/default.asp?w=1280)
raw_age_sogn_data1 <- read.csv("Control_Variables/average_age_sogn_data0709.csv", sep = ",", header = FALSE) %>% slice(4:n()) %>% `colnames<-`(c("sogn", 2007:2009))
raw_age_sogn_data2 <- read.csv("Control_Variables/average_age_sogn_data1013.csv", sep = ",", header = FALSE) %>% slice(4:n()) %>% `colnames<-`(c("sogn", 2010:2013))
raw_age_sogn_data3 <- read.csv("Control_Variables/average_age_sogn_data1417.csv", sep = ",", header = FALSE) %>% slice(4:n()) %>% `colnames<-`(c("sogn", 2014:2017))
raw_age_sogn_data4 <- read.csv("Control_Variables/average_age_sogn_data1821.csv", sep = ",", header = FALSE) %>% slice(4:n()) %>% `colnames<-`(c("sogn", 2018:2021))

raw_age_sogn_data <- cbind(raw_age_sogn_data1, raw_age_sogn_data2, raw_age_sogn_data3, raw_age_sogn_data4)
age_sogn_data <- raw_age_sogn_data[, !duplicated(colnames(raw_age_sogn_data))]

raw_age_municip_data <- read.csv("Control_Variables/average_age_municip_data.csv", sep = ",", header = FALSE)
age_municip_data <- raw_age_municip_data[4:nrow(raw_age_municip_data),2:ncol(raw_age_municip_data)]
colnames(age_municip_data) <- c("municipality", 2005:2021)

# fill zeros in sogn data with municip averages
for (i in 1:(nrow(age_sogn_data)-2)){
  for (j in 1:ncol(age_sogn_data)){
    if (age_sogn_data[i,j] == 0){
      age_sogn_data[i,j] <- age_municip_data[age_municip_data$municipality == substr(str_extract(age_sogn_data[i,1], regex("\\(([^ ]*)")), 2, 1000), j]
    }
  }
}
rm(raw_age_sogn_data1, raw_age_sogn_data2, raw_age_sogn_data3, raw_age_sogn_data4, raw_age_sogn_data, raw_age_municip_data)

# calculate weighted average age for each sogn
weights_years0721 <- as.vector(unname(table(PUD_data_geo$year))[4:18]/sum(unname(table(PUD_data_geo$year))[4:18]))
age_sogn_data <- age_sogn_data %>% mutate(weighted = as.matrix(age_sogn_data[,2:ncol(age_sogn_data)]) %*% weights_years0721)
age_sogn_data$code <- substr(age_sogn_data$sogn, 0, 4)

for (i in 1:nrow(users_with_travel_info)){
  if (!is.na(users_with_travel_info$parish_codes[i])){
    users_with_travel_info$age[i] <- age_sogn_data[which(age_sogn_data$code == users_with_travel_info$parish_codes[i]), "weighted"]
  } else {
    users_with_travel_info$age[i] <- NA
  }
}


# education
education_html <- read_html("https://flo.uri.sh/visualisation/7716644/embed?auto=1")
html_doc <- html_nodes(education_html, xpath = '/html/body')
stats <- toString(html_text(html_doc))

parishes <- str_extract_all(gsub('"', "", stats), "(?<=\\],name:)[^,]*")[[1]]
parishes <- substr(parishes, 0, nchar(parishes)-5)
higher_edu_share <- str_extract_all(gsub('"', "", stats), "(?<=,value:\\[)[^\\]]*")[[1]]

education_data <- data.frame(parishes, higher_edu_share)
municipalities <- str_extract_all(gsub('"', "", stats), "(?<=plads i )[^\\\\]*")[[1]]
education_data <- data.frame(education_data[3:nrow(education_data),], municipalities)

rm(education_html, html_doc, stats, parishes, higher_edu_share)

# rename a few parishes to match with edu data
users_with_travel_info$parish_names[which(users_with_travel_info$parish_names == "Hundige-Kildebrønde")] <- "Kildebrønde"
users_with_travel_info$parish_names[which(users_with_travel_info$parish_names == "Betlehems")] <- "Bethlehem"
users_with_travel_info$parish_names[which(users_with_travel_info$parish_names == "Godthåb")] <- "Godthaabs"
users_with_travel_info$parish_names[which(users_with_travel_info$parish_names == "Fredens og Nazaret")] <- "Fredens-Nazaret"

# match edu data with user parishes
for (i in 1:nrow(users_with_travel_info)){
  if (!is.na(users_with_travel_info$parish_names[i])){
    edu <- education_data[which(education_data$parishes == users_with_travel_info$parish_names[i] & education_data$municipalities == users_with_travel_info$municip[i]), "higher_edu_share"]
  } else {
    edu <- NA
  }
  if (length(edu) == 0){
    users_with_travel_info$edu[i] <- NA
  } else {
    users_with_travel_info$edu[i] <- edu
  }
}

# manually add data for users in koebenhavn/frederiksberg where problems arise due to parishes in different municipalities
users_with_travel_info$edu[which(users_with_travel_info$parish_names == "Sankt Markus" & users_with_travel_info$municip == "København")] <- education_data[which(education_data$parishes == "Sankt Markus" & education_data$municipalities == "Frederiksberg"), "higher_edu_share"]
users_with_travel_info$edu[which(users_with_travel_info$parish_names == "Mariendals" & users_with_travel_info$municip == "København")] <- education_data[which(education_data$parishes == "Mariendals" & education_data$municipalities == "Frederiksberg"), "higher_edu_share"]
users_with_travel_info$edu[which(users_with_travel_info$parish_names == "Føllenslev-Særslev")] <- education_data[which(education_data$parishes == "Bregninge-Bjergsted-Alleshave"), "higher_edu_share"]

users_with_travel_info$edu <- as.numeric(users_with_travel_info$edu)


# filter out users with distances e.g. > 150km
users_with_travel_info_limited <- users_with_travel_info[users_with_travel_info$traveldist_dest < 150000,]


# final data
final_data <- users_with_travel_info_limited[, c("owner", "no_of_PUD", "travelcost_dest1", "travelcost_sub1", "income", "age", "edu", "userloc_lon", "userloc_lat", "traveltime_dest", "traveldist_dest")]
final_data <- na.omit(final_data)
colnames(final_data) <- c("owner", "PUD", "tcdest", "tcsub", "inc", "age", "edu", "userloc_lon", "userloc_lat", "ttime", "tdist")
final_data$tdist <- final_data$tdist/1000

hist(final_data$PUD)
plot(final_data$PUD, final_data$tcdest)

users_with_travel_info_limited <- users_with_travel_info_limited[!is.na(users_with_travel_info_limited$income),]
table(users_with_travel_info_limited$sub_used)


# sensitivity analysis final data with 100% car travel
final_data_sensitivity <- users_with_travel_info_limited[, c("owner", "no_of_PUD", "travelcost_dest2", "travelcost_sub2", "income", "age", "edu", "userloc_lon", "userloc_lat", "traveltime_dest", "traveldist_dest")]
final_data_sensitivity <- na.omit(final_data_sensitivity)
colnames(final_data_sensitivity) <- c("owner", "PUD", "tcdest", "tcsub", "inc", "age", "edu", "userloc_lon", "userloc_lat", "ttime", "tdist")


# sensitivity analysis only users where stated municip = predicted municip --> sample too small
final_data_sensitivity2 <- na.omit(users_with_travel_info_limited[users_with_travel_info_limited$loc_profile_municipality == users_with_travel_info_limited$municip, c("owner", "no_of_PUD", "travelcost_dest1", "travelcost_sub1", "income", "age", "edu", "userloc_lon", "userloc_lat", "traveltime_dest", "traveldist_dest")])
nrow(users_with_travel_info_limited[!is.na(users_with_travel_info_limited$loc_profile_municipality),])

rm(income_data, age_municip_data, age_sogn_data, education_data, users_with_travel_info, users_with_travel_info_limited, PUD_data_geo, final_data_sensitivity2)

# some statistics on the final dataset
stargazer(as.data.frame(final_data[c("PUD", "tcdest", "tcsub", "inc", "age", "edu", "ttime", "tdist")]), 
          title="Descriptive Statistics", 
          label = "descr_stats_table",
          summary.stat = c("min", "max", "mean", "median", 
                           "sd"), digits=2,
          covariate.labels = c("PUD", "tcdest", "tcsub", "inc", "age", "edu"),
          notes = "\\parbox[t]{12.5cm}{\\scriptsize Note: PUD is the number of photo-user-days registered for each user at the study site, tcdest is the return travel cost to the study site in DKK, including operation and time costs, tcsub is the return travel cost to the closest substitute site in DKK, inc is the mean annual income of the parish each user was assigned to in 1000 DKK, age is the average age of the parish in years, edu is the share of the parish's population that has completed a long further education (equivalent to a Master's degree), ttime is the drive time from the predicted home location to the study site in minutes (one-way), and tdist is the corresponding drive distance in km (one-way).}",
          notes.align = "l")

cor(final_data$tcdest, final_data$tcsub, method = "pearson")


#-------------------------------------------------------------------------------------------------------------------------------------------------
#####################################
####### TCM MODEL ###################

# estimate TP and TNB models
trunc_poisson <- glmmadmb(PUD ~ tcdest + tcsub + inc + age + edu, family = "truncpoiss", data = na.omit(final_data))
summary(trunc_poisson)
trunc_nb <- glmmadmb(PUD ~ tcdest + tcsub + inc + age + edu, family = "truncnbinom", data = na.omit(final_data))
summary(trunc_nb)

# test for whether TP or TNB is more appropriate
lrtest(trunc_poisson2, trunc_nb2)


# sensitivity analysis without substitute sites
trunc_poisson_sens1 <- glmmadmb(PUD ~ tcdest + inc + age + edu, family = "truncpoiss", data = na.omit(final_data))
summary(trunc_poisson_sens1)
trunc_nb_sens1 <- glmmadmb(PUD ~ tcdest + inc + age + edu, family = "truncnbinom", data = na.omit(final_data))
summary(trunc_nb_sens1)

# sensitivity analysis with 100% car travel
trunc_poisson_sens2 <- glmmadmb(PUD ~ tcdest + tcsub + inc + age + edu, family = "truncpoiss", data = na.omit(final_data_sensitivity))
summary(trunc_poisson_sens2)
trunc_nb_sens2 <- glmmadmb(PUD ~ tcdest + tcsub + inc + age + edu, family = "truncnbinom", data = na.omit(final_data_sensitivity))
summary(trunc_nb_sens2)


# calculate mean consumer surplus per individual and per visit
CS_visit <- -(1/summary(trunc_nb)$coefficients[2, 1])

CS_visit_sens1 <- -(1/summary(trunc_nb_sens1)$coefficients[2, 1])
CS_visit_sens2 <- -(1/summary(trunc_nb_sens2)$coefficients[2, 1])

#-------------------------------------------------------------------------------------------------------------------------------------------------