# this script is a collection of functions from the photosearcher and 
# pracma packages that were debugged to avoid some frequently occuring
# errors. This might limit some functionality for other purposes,
# which is not required for the CTCM application

photo_search_correct <- function (mindate_taken = NULL, maxdate_taken = NULL, mindate_uploaded = NULL, 
                                  maxdate_uploaded = NULL, user_id = NULL, text = NULL, tags = NULL, 
                                  tags_any = TRUE, bbox = NULL, woe_id = NULL, sf_layer = NULL, 
                                  has_geo = TRUE) 
{
  library(photosearcher)
  pics <- NULL
  num_calls <- 0
  date_df <- data.frame(mindate_taken = mindate_taken, maxdate_taken = maxdate_taken)
  api_key <- create_and_check_key()
  if ((!is.null(bbox) & !is.null(woe_id)) | (!is.null(sf_layer) & 
                                             !is.null(woe_id)) | (!is.null(bbox) & !is.null(sf_layer))) {
    stop("Specify search location as only one of: woe_id, bbox or sf_layer.")
  }
  if (!is.null(sf_layer)) {
    bbox <- create_bbox(sf_layer = sf_layer)
  }
  if (!is.null(woe_id)) {
    check_location(api_key = api_key)
  }
  if (isTRUE(tags_any)) {
    tags_any <- "any"
  }
  else {
    tags_any <- "all"
  }
  while (nrow(date_df) > 0) {
    mindate_taken <- as.POSIXct(date_df[1, "mindate_taken"])
    maxdate_taken <- as.POSIXct(date_df[1, "maxdate_taken"])
    mindate_taken <- mindate_taken - 28800
    mindate_unix <- as.numeric(mindate_taken)
    maxdate_unix <- as.numeric(maxdate_taken)
    if (mindate_taken > maxdate_taken) {
      date_df <- date_df[-1, ]
    }
    else {
      base_url <- get_url(mindate_taken = mindate_unix, 
                          maxdate_taken = maxdate_unix, mindate_uploaded = mindate_uploaded, 
                          maxdate_uploaded = maxdate_uploaded, user_id = user_id, 
                          api_key = api_key, page = 1, text = text, tags = tags, 
                          tag_mode = tags_any, bbox = bbox, woe_id = woe_id, 
                          has_geo = has_geo)
      photo_xml <- search_url(base_url = base_url)
      num_calls <- num_calls + 1
      find_errors(error_xml = photo_xml)
      if (!is.null(photo_xml)) {
        pages_data <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))
        pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
        total_pages <- pages_data["pages", ]
        total <- pages_data["total", ]
        if (total > 0 && total > 4000) {
          for (i in 1:16) {
            base_url <- get_url(mindate_taken = mindate_unix, 
                                maxdate_taken = maxdate_unix, mindate_uploaded = mindate_uploaded, 
                                maxdate_uploaded = maxdate_uploaded, user_id = user_id, 
                                api_key = api_key, page = i, text = text, 
                                tags = tags, tag_mode = tags_any, bbox = bbox, 
                                woe_id = woe_id, has_geo = has_geo)
            photo_xml <- search_url(base_url = base_url)
            num_calls <- num_calls + 1
            if (!is.null(photo_xml)) {
              photo_atts <- xml2::xml_find_all(photo_xml, 
                                               "//photo", ns = xml2::xml_ns(photo_xml))
              tmp_df <- dplyr::bind_rows(lapply(xml2::xml_attrs(photo_atts), 
                                                function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))
              photo_atts2 <- xml2::xml_find_all(photo_xml, 
                                                "//description", ns = xml2::xml_ns(photo_xml))
              descriptions <- NULL
              for (i in 1:length(photo_atts2)) {
                tmp_df2 <- data.frame(description = paste(photo_atts2[i]))
                descriptions <- rbind(descriptions, 
                                      tmp_df2)
                tmp_df2 <- NULL
              }
              descriptions$description <- gsub("<description>", 
                                               "", descriptions$description)
              descriptions$description <- gsub("<description/>", 
                                               "", descriptions$description)
              descriptions$description <- gsub("</description>", 
                                               "", descriptions$description)
              tmp_df <- cbind(tmp_df, descriptions)
              pics <- dplyr::bind_rows(pics, tmp_df)
              tmp_df <- NULL
            }
          }
          date_df <- rbind(date_df[-1, ], data.frame(mindate_taken = max(pics$datetaken), 
                                                     maxdate_taken = maxdate_taken))
        }
        else if (total > 0 && total < 4000) {
          for (i in 1:total_pages) {
            base_url <- get_url(mindate_taken = mindate_unix, 
                                maxdate_taken = maxdate_unix, mindate_uploaded = mindate_uploaded, 
                                maxdate_uploaded = maxdate_uploaded, user_id = user_id, 
                                api_key = api_key, page = i, text = text, 
                                tags = tags, tag_mode = tags_any, bbox = bbox, 
                                woe_id = woe_id, has_geo = has_geo)
            photo_xml <- search_url(base_url = base_url)
            num_calls <- num_calls + 1
            if (!is.null(photo_xml)) {
              photo_atts <- xml2::xml_find_all(photo_xml, 
                                               "//photo", ns = xml2::xml_ns(photo_xml))
              tmp_df <- dplyr::bind_rows(lapply(xml2::xml_attrs(photo_atts), 
                                                function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))
              photo_atts2 <- xml2::xml_find_all(photo_xml, 
                                                "//description", ns = xml2::xml_ns(photo_xml))
              descriptions <- NULL
              for (i in 1:length(photo_atts2)) {
                tmp_df2 <- data.frame(description = paste(photo_atts2[i]))
                descriptions <- rbind(descriptions, 
                                      tmp_df2)
                tmp_df2 <- NULL
              }
              descriptions$description <- gsub("<description>", 
                                               "", descriptions$description)
              descriptions$description <- gsub("<description/>", 
                                               "", descriptions$description)
              descriptions$description <- gsub("</description>", 
                                               "", descriptions$description)
              tmp_df <- cbind(tmp_df, descriptions)
              pics <- dplyr::bind_rows(pics, tmp_df)
              tmp_df <- NULL
            }
          }
          date_df <- date_df[-1, ]
        }
        else {
          date_df <- date_df[-1, ]
        }
      }
      else {
        date_df <- date_df[-1, ]
      }
    }
  }
  if (is.null(pics)) {
    stop("No photographs meeting criteria")
  }
  if (!is.null(sf_layer)) {
    with_geom <- sf::st_as_sf(pics, coords = c("longitude", 
                                               "latitude"), crs = 4326)
    pics <- cbind(with_geom, longitude = pics$longitude, 
                  latitude = pics$latitude)
    sf_layer <- sf::st_transform(sf_layer, crs = "+proj=longlat +datum=WGS84 +no_defs")
    pics$within <- sf::st_intersects(pics, sf_layer)
    pics$within <- as.character(pics$within)
    pics <- dplyr::filter(pics, pics$within != "integer(0)")
  }
  pics <- parse_pic_correct(pics = pics)
  pics <- dplyr::distinct(pics)
  return(pics)
}


parse_pic_correct <- function (pics = NULL) 
{
  pics <- data.frame(lapply(pics, as.character), stringsAsFactors = FALSE)
  pics$datetaken <- as.POSIXct(pics$datetaken)
  cols.num <- c("id", "server", "farm", "latitude", "longitude", 
                "woeid")
  pics[cols.num] <- sapply(pics[cols.num], as.numeric)
  cols.num <- c("license", "datetakengranularity", "datetakenunknown", 
                "count_views", "count_faves", "count_comments", "accuracy", 
                "context")
  pics[cols.num] <- sapply(pics[cols.num], as.integer)
  cols.num <- c("ispublic", "isfriend", "isfamily", "geo_is_family", 
                "geo_is_friend", "geo_is_contact", "geo_is_public")
  pics[cols.num] <- sapply(pics[cols.num], as.logical)
  pics$dateupload <- as.POSIXct(as.numeric(pics$dateupload), 
                                tz = "GMT", origin = "1970-01-01")
  pics$lastupdate <- as.POSIXct(as.numeric(pics$lastupdate), 
                                tz = "GMT", origin = "1970-01-01")
  license_names <- c("All Rights Reserved", "Attribution-NonCommercial-ShareAlike License", 
                     "Attribution-NonCommercial License", "Attribution-NonCommercial-NoDerivs License", 
                     "Attribution License", "Attribution-ShareAlike License", 
                     "Attribution-NoDerivs License", "No known copyright restrictions", 
                     "United States Government Work", "Public Domain Dedication (CC0)", 
                     "Public Domain Mark")
  license_urls <- c("NA", "https://creativecommons.org/licenses/by-nc-sa/2.0/", 
                    "https://creativecommons.org/licenses/by-nc/2.0/", "https://creativecommons.org/licenses/by-nc-nd/2.0/", 
                    "https://creativecommons.org/licenses/by/2.0/", "https://creativecommons.org/licenses/by-sa/2.0/", 
                    "https://creativecommons.org/licenses/by-nd/2.0/", "https://www.flickr.com/commons/usage/", 
                    "http://www.usa.gov/copyright.shtml", "https://creativecommons.org/publicdomain/zero/1.0/", 
                    "https://creativecommons.org/publicdomain/mark/1.0/")
  license_info <- data.frame(license = 0:10, license_name = license_names, 
                             license_url = license_urls, stringsAsFactors = FALSE)
  pics <- merge(pics, license_info, by = "license")
  return(pics)
}

create_and_check_key <- function () 
{
  if (!file.exists("photosearcher_key.sysdata")) {
    ui_todo("Create a Flickr API key at https://www.flickr.com/services/apps/create/")
    utils::browseURL("https://www.flickr.com/services/apps/create/")
    ui_todo("Enter your Flickr API key:")
    utils::write.table(readline(), file = "photosearcher_key.sysdata", 
                       col.names = FALSE, row.names = FALSE)
  }
  api_key <- utils::read.table("photosearcher_key.sysdata", 
                               stringsAsFactors = FALSE)
  base_url <- paste("https://api.flickr.com/services/rest/", 
                    "?method=flickr.photos.search&api_key=", api_key, sep = "")
  photo_xml <- search_url(base_url = base_url)
  pages_data <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))
  warn <- as.character(unlist(pages_data))
  if ((warn[2]) == ("Invalid API Key (Key has invalid format)")) {
    stop("Invalid API Key: correct this in photosearcher_key.sysdata")
  }
  return(api_key)
}

create_bbox <- function (sf_layer = NULL) 
{
  layer_epsg <- unlist(sf::st_crs(sf_layer)[1])
  if ((is.na(layer_epsg)) | (layer_epsg != 4326)) {
    sf_layer <- sf::st_transform(sf_layer, crs = "+proj=longlat +datum=WGS84 +no_defs")
  }
  bbox <- sf::st_bbox(sf_layer)
  xmin <- bbox[1]
  ymin <- bbox[2]
  xmax <- bbox[3]
  ymax <- bbox[4]
  bbox <- as.character(paste(xmin, ",", ymin, ",", xmax, ",", 
                             ymax, sep = ""))
}

check_location <- function (api_key = NULL) 
{
  known_location <- paste("https://api.flickr.com/services/rest/", 
                          "?method=flickr.photos.search&api_key=", api_key, "&woe_id=35356", 
                          sep = "")
  r <- httr::GET(known_location)
  photo_xml <- xml2::read_xml(r)
  known_warn <- data.frame(xml2::xml_attrs(xml2::xml_children(photo_xml)))
  if ((known_warn[2, 1]) == ("Not a valid place type")) {
    stop("Flickr location services are down")
  }
}

get_url <- function (mindate_taken, maxdate_taken, mindate_uploaded = NULL, 
                     maxdate_uploaded = NULL, user_id = NULL, api_key, page, 
                     text = NULL, tags = NULL, tag_mode = NULL, bbox = NULL, 
                     woe_id = NULL, has_geo = TRUE) 
{
  if (is.null(mindate_uploaded)) {
    mindate_uploaded = mindate_taken
  }
  if (is.null(maxdate_uploaded)) {
    maxdate_uploaded = maxdate_taken
  }
  text <- gsub(" ", "+", trimws(text))
  tags <- gsub(" ", "+", trimws(tags))
  tags <- paste(tags, collapse = ",")
  mindate_taken <- gsub(" ", "+", trimws(mindate_taken))
  maxdate_taken <- gsub(" ", "+", trimws(maxdate_taken))
  base_url <- paste("https://api.flickr.com/services/rest/", 
                    "?method=flickr.photos.search&api_key=", api_key, "&text=", 
                    text, "&tags=", tags, "&tag_mode=", tag_mode, "&min_taken_date=", 
                    as.character(mindate_taken), "&max_taken_date=", as.character(maxdate_taken), 
                    ifelse(!(is.null(mindate_uploaded)), paste0("&min_upload_date=", 
                                                                mindate_uploaded), ""), ifelse(!(is.null(maxdate_uploaded)), 
                                                                                               paste0("&max_upload_date=", maxdate_uploaded), ""), 
                    ifelse(!(is.null(user_id)), paste0("&user_id=", user_id), 
                           ""), ifelse(!(is.null(bbox)), paste0("&bbox=", bbox), 
                                       ""), ifelse(!(is.null(woe_id)), paste0("&woe_id=", 
                                                                              woe_id), ""), ifelse(has_geo, paste0("&has_geo=", 
                                                                                                                   has_geo), ""), "&extras=", "date_taken,geo,tags,license,", 
                    "url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,", 
                    "url_l,url_o,count_views,count_comments,count_faves,", 
                    "date_upload,last_update,description", "&page=", page, 
                    "&sort=date-taken-asc", "&format=", "rest", sep = "")
  return(base_url)
}

search_url <- function (base_url) 
{
  r <- httr::GET(base_url)
  count_stat <- 0
  while (r$status_code != 200 & count_stat < 3) {
    Sys.sleep(0.5)
    r <- httr::GET(base_url)
    count_stat <- count_stat + 1
  }
  if (r$status_code != 200) {
    warning("Status code:", r$status, " for ", base_url, 
            " - message: ", httr::content(r, "text"))
  }
  error <- tryCatch({
    photo_xml <- xml2::read_xml(r)
    error <- "success"
  }, error = function(err) {
    warning(base_url, " skipped beacuse: ", err)
    error <- "error"
    photo_xml <- NULL
  })
  return(photo_xml)
}

find_errors <- function (error_xml = NULL) 
{
  if (xml2::xml_attrs(error_xml) == "fail") {
    warn_data <- data.frame(xml2::xml_attrs(xml2::xml_children(error_xml)))
    warn <- as.character(unlist(warn_data))
    stop(paste(warn[2]))
  }
}

geo_median_correct <- function (P, tol = 1e-07, maxiter = 200) 
{
  stopifnot(is.numeric(P))
  if (!is.matrix(P)){
    stop("Argument 'P' must be a matrix (of points in R^n).")
  } 
  m <- nrow(P)
  n <- ncol(P)
  if (n == 1) {
    return(list(p = median(P), d = sum(abs(P - median(P))), 
                reltol = 0, niter = 0))
  } else {
    p0 <- apply(P, 2, mean)
    p1 <- p0 + 1
    iter <- 1
    while (max(abs(p0 - p1)) > tol && iter < maxiter) {
      iter <- iter + 1
      p0 <- p1
      s1 <- s2 <- 0
      for (j in 1:m) {
        d <- Norm(P[j, ] - p0)
        s1 <- s1 + P[j, ]/d
        s2 <- s2 + 1/d
      }
      p1 <- s1/s2
    }
    if (iter >= maxiter) {
      warning("Maximum number of iterations reached; may not converge.")
    }
    d <- 0
    for (j in 1:m) {d <- d + Norm(P[j, ] - p1)}
    return(list(p = p1, d = d, reltol = max(abs(p0 - p1)), niter = iter))
  }
}

