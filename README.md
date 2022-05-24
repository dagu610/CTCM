# CTCM
R code and functions for conducting crowdsourced travel cost analyses based on Flickr photographs. Including a full script for an application on Jaegersborg Dyrehave, a deer park north of Copenhagen, Denmark, and functions for the identification of users' home locations. See this script for example usage of the functions listed below.

## CTCM_script.R
Full script including a CTCM analysis undertaken for the site of Jaegersborg Dyrehave in Denmark, from data extraction until final model estimation.

## debugged_functions.R
Collection of functions from other packages that were debugged to avoid some frequently occuring errors. This might limit some functionality for other purposes, which is not required for the CTCM application.

## get_home_location.R
Function that identifies countries and municipalities with the highest number of PUDs for a user with given user id, based on a given list of municipality polygons, the time frame considered defaults to 2004-21 but can be adjusted.

## get_parishes.R
Function extracting parish codes and names from a df containing users' home coordinates, using the Dataforsyningen API (works for Danish parishes only).

## get_profile_location.R
Function that extracts location information from Flickr user profiles and identifies country names (out of a vector of world countries), municipalities (out of a given vector of municipalities), and parishes (out of a given vector of parishes). Then appends identified information to the given input df of users.
