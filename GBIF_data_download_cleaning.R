
#In this script, I will be extracting and cleaning sample species datasets from the GBIF database

#I will first start by uploading a series of packages required for this analysis

library(countrycode)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(rgbif)
library(sf)
library(scrubr)
library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(writexl)
library(maps)

#My test analysis is for the Shrew-mole, "Neurotrichus gibbsii"

#I will start by pulling the species usage key
usageKey <- name_backbone(name = "Neurotrichus gibbsii", rank = "species") %>%
  pull(usageKey)
usageKey

#Next, is to Send a request to download the file
gbif_download_key <- occ_download(pred("taxonKey", 2436208),
                                  format = "SIMPLE_CSV",
                                  user = "yisaginath",
                                  pwd = "Ginathyuh80.",
                                  email = "yisaginath80@yahoo.com")

occ_download_wait(gbif_download_key)

#Next, is to download the datasets after the request has been sent
gbif_download <- occ_download_get(gbif_download_key,
                                  overwrite = TRUE) %>%
  occ_download_import() %>%
  setNames(tolower(names(.)))

head(gbif_download, 10) #I am now checking the first ten rows

#the next thing is to retrieve the citation for the downloaded data
print(gbif_citation(occ_download_meta(gbif_download_key))$download)

#I will now identify the countries where data were collected using country code
table(gbif_download$countrycode)

#Next, is to plot the data to get an overview
wm <- borders("world", colour = "gray50", fill = "gray50")
wm1 <- borders("usa", colour = "gray50", fill = "gray50")

ggplot() +
  coord_fixed() +
  wm1 +
  geom_point(data = gbif_download,
             aes(x = decimallongitude, y = decimallatitude),
             colour = "red",
             size = 0.5) +
  theme_bw() #I can also plot with world view for visualization purposes


#Data cleaning: I will apply four steps for cleaning my downloaded data

#Step 1: The first step is to remove records extracted from fossil and livings specimens, as well as those from alien and invasive populations
clean_step1 <- gbif_download %>%
  as_tibble() %>%
  filter(!basisofrecord %in% c("FOSSIL_SPECIMEN",
                               "LIVING_SPECIMEN"),
         !establishmentmeans %in% c("MANAGED",
                                    "INTRODUCED",
                                    "INVASIVE",
                                    "NATURALISED"))
print(paste0(nrow(gbif_download)-nrow(clean_step1), " records deleted; ",
             nrow(clean_step1), " records remaining."))

#Step 2: the next step is to filter and remove records with zero or NA coordinates
clean_step2 <- clean_step1 %>%
  filter(!is.na(decimallatitude),
         !is.na(decimallongitude))
print(paste0(nrow(gbif_download)-nrow(clean_step2), " records deleted; ",
             nrow(clean_step2), " records remaining."))

head(clean_step2, 10)
print(clean_step2)

#I will now plot and visualize the raw records vs the clean records
ggplot() +
  coord_fixed() +
  wm1 +
  geom_point(data = gbif_download,
             aes(x = decimallongitude, y = decimallatitude),
             colour = "red",
             size = 0.5) +
  geom_point(data = clean_step2,
             aes(x = decimallongitude, y = decimallatitude),
             colour = "blue",
             size = 0.5) +
  theme_bw()


#Step 3: I will now filter and remove records with coordinate uncertainty and precision issues
clean_step3 <- clean_step2 %>%
  filter(is.na(coordinateuncertaintyinmeters) |
           coordinateuncertaintyinmeters < 10000,
         is.na(coordinateprecision) |
           coordinateprecision > 0.01)

print(paste0(nrow(gbif_download)-nrow(clean_step3), " records deleted; ",
             nrow(clean_step3), " records remaining." ))

print(clean_step3)


#I will plot and visualize the raw records vs the clean records in step 3
ggplot() +
  coord_fixed() +
  wm1 +
  geom_point(data = gbif_download,
             aes(x = decimallongitude, y = decimallatitude),
             colour = "red",
             size = 0.5) +
  geom_point(data = clean_step3,
             aes(x = decimallongitude, y = decimallatitude),
             colour = "blue",
             size = 0.5) +
  theme_bw()


#Step 4: I will further filter out very old records (I will try for pre-1900 records)
clean_step4 <- clean_step3 %>%
  filter(year >= 1900) 
print(paste0(nrow(gbif_download)-nrow(clean_step4), " records deleted; ",
             nrow(clean_step4), " records remaining." ))

#Next is to plot and visualize the raw records vs the clean records from step 4
ggplot() +
  coord_fixed() +
  wm1 +
  geom_point(data = gbif_download,
             aes(x = decimallongitude, y = decimallatitude),
             colour = "red",
             size = 0.5) +
  geom_point(data = clean_step4,
             aes(x = decimallongitude, y = decimallatitude),
             colour = "blue",
             size = 0.5) +
  theme_bw()

#I am now saving my results in a file path
write_xlsx(gbif_download,"D:\\Documents\\GBIF_test\\Shrew-mole_raw.xlsx")
write_xlsx(clean_step4,"D:\\Documents\\GBIF_test\\Shrew-mole_clean.xlsx")
