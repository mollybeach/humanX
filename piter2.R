library(tidyverse)
library(tigris)
library(sf)
library(reshape2)
library(tmap)
library(tmaptools)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Part 1
require(data.table)
od <- fread('wa_od_main_JT00_2021.csv')
rac <- fread('wa_rac_S000_JT00_2021.csv')
wac <- fread('wa_wac_S000_JT00_2021.csv')
bellevue_tnums <- read.csv('Data USA - Geo Map of Income by Location in Bellevue, Wa.csv')

# Retrieve all the tract numbers and clean it of duplicates
tnums <-  gsub("Census Tract ","", bellevue_tnums$geo_name)
tnums <- unique(tnums)

# Read in Bellevue city boundary shape file
wa_tracts <- "cb_2020_53_tract_500k.shp"
wa_state <- read_shape(file=wa_tracts, as.sf = TRUE)

# Find all the Census Tracts in Bellevue in the wa_tracts shapefile
bellevue <- wa_tracts %>% 
  filter(NAME %in% tnums)

# Part 2

# Creates and returns a dataframe of the relationship between hometract and
# work tract with the specified key.
CreateDf <- function(key) {
  df <- dcast(bell_relate, work_tract ~ home_tract, value.var = key)
  df[is.na(df)] <- 0
  return (df)
}

# Extracts the specified tract numbers 
ExtractTracts <- function(data) {
  data$work_tract <- as.character(data$work_tract)
  data$name <- substr(data$work_tract, nchar(data$work_tract) - 4, nchar(data$work_tract))
  selected <- data[grep('23201|23202|23403|23804', data$name),]
  
  return(selected)
}

## OD data
# setting and cleaning the table 
od$w_geocode <- as.character(od$w_geocode)
od$h_geocode <- as.character(od$h_geocode)

od <- od %>%
  mutate(w_geocode = replace(w_geocode, length(w_geocode) == 14, "0"),
         h_geocode = replace(h_geocode, length(w_geocode) == 14, "0"))

# Creating columns for worktract and hometract
od$work_tract <- substr(od$w_geocode, 1, 11)
od$home_tract <- substr(od$h_geocode, 1, 11)

# renaming a work tract column
colnames(bellevue)[5] <- paste("work_tract")

# Joining the data to only show Bellevue data
overall_bell <- merge(bellevue, od, by ="work_tract", all.x = TRUE)

# Creating block level and check for duplicates
bell_block <- overall_bell %>%
  select(w_geocode, h_geocode, S000, work_tract, home_tract)
st_geometry(bell_block) <- NULL

block<- unique(bell_block[,1:2])
nrow(block) == nrow(bell_block)

## Added a count column for the number of sending blocks
block$block_ct <- 1

## Get number of blocks "sending" jobs to work block
work_block <- aggregate(. ~ w_geocode, data=block[,-2], FUN=sum)

## Use sum job variable S000 to get count of "jobs" to work block
jobs_wb <- aggregate(. ~ w_geocode, data=bell_block[,c(1,3)], FUN=sum)

## Merge the two dataframes together by work block FIPS
work_block <- merge(work_block, jobs_wb, by="w_geocode")

## Creates a data frame with home_tract and w_geocode
block_w <- unique(bell_block[,c(1,5)])

## Added a count column for the number of sending tracts
block_w$tract_ct <- 1

## Number of tracts "sending" jobs to work block and home block
block_w <- aggregate(. ~ w_geocode, data=block_w[,c(1,3)], FUN=sum)
final_odblock <- merge(work_block, block_w, by="w_geocode")

# Produce a dataframe with a worktract as rows and hometract as columns
bell_relate <- overall_bell %>%
  select(work_tract, home_tract, S000, SA01, SA02, SA03, SE01, SE02, SE03, SI01, SI02, SI03)

## Total number of jobs dataframe
tract_tot <- CreateDf("S000")
## Number of jobs of works age 29 or younger 
tract_y <- CreateDf("SA01")
## Number of jobs for worker age 30 to 54
tract_m <- CreateDf("SA02")
## Number of jobs for workers age 55 or older
tract_o <- CreateDf("SA03")
## Number of jobs for with earnings $1250/month or less
tract_lin <- CreateDf("SE01")
## Number of jobs with earnings $1251/month to $3333/month
tract_min <- CreateDf("SE02")
## Number of jobs with earnings greater than $3333/month
tract_hin <- CreateDf("SE03")
## Number of jobs in goods producing industry sector
tract_gds <- CreateDf("SI01")
## Number of jobs in trade, transportation, and utilties industry sector
tract_tu <- CreateDf("SI02")
## Number of jobs in all other services industry sectors
tract_all <- CreateDf("SI03")

# WAC
wac$w_geocode <- as.character(wac$w_geocode)
wac <- wac %>%
  mutate(w_geocode = replace(w_geocode, length(w_geocode) == 14, "0"))

# Make columns for work and home tract
wac$work_tract <- substr(wac$w_geocode, 1, 11)

# Aggregate the data (total number of: jobs, utilites, manufacturing, 
# wholesale trade, transportation and warehousing)
wt_wac <- aggregate(. ~ wac$work_tract,  data= wac[,c(2:52)], FUN=sum)
colnames(wt_wac)[1] <- "work_tract"

# Merge the data so its data pertains to bellevue tract area
wac_bell <- merge(bellevue, wt_wac, by ="work_tract", all.x = TRUE)
## RAC
rac$h_geocode <- as.character(rac$h_geocode)
rac <- rac %>%
  mutate(h_geocode = replace(h_geocode, length(h_geocode) == 14, "0"))

# Make columns for home tract
rac$home_tract <- substr(rac$h_geocode, 1, 11)

# Aggregate the data (total number of: jobs, utilites, manufacturing, 
# wholesale trade, transportation and warehousing)
ht_rac <- aggregate(. ~rac$home_tract,  data= rac[,c(2:42)], FUN=sum)

# MAPPING AT BLOCK LEVEL
wa_state$work_tract <- wa_state$GEOID
wa_data <- select(wa_state, GEOID, NAME, geometry, work_tract)
wac_small <- select(wac, w_geocode, CNS10, work_tract)
wac_block <- merge(x = wa_data, y = wac_small, by = "work_tract", all.y = TRUE)
block_level_map <- tm_shape(wac_block) +
  tm_fill("CNS10", title = "# of Jobs (count)") +
  tm_borders(alpha=0.5) + tm_text("NAME", size = 0.5) + 
  tm_legend(legend.position = c("left", "bottom"),
            legend.title.size=0.8) +
  tm_layout(title = "# of Jobs in Finance & Insurance\n         in Bellevue (Block Level)",
            title.position = c("center", "top"),
            title.size = 0.8)
# tmap_save(tract_level_map, filename="blocklevel.jpg")
# block_level_map

# MAPPING AT TRACT LEVEL
tract_level_map <- tm_shape(wac_bell) +
  tm_fill("CNS10", title = "# of Jobs (count)", 
          breaks = c(0, 10, 50, 100, 200, 300, Inf), 
          palette = c("#f1eef6", "#d0d1e6", "#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d")) +
  tm_borders(alpha=0.5) + tm_text("NAME", size = 0.5) + 
  tm_legend(legend.position = c("left", "bottom"),
            legend.title.size=0.8) +
  tm_layout(title = "# of Jobs in Finance & Insurance\n         in Bellevue (Tract Level)",
            title.position = c("right", "top"),
            title.size = 0.8)
tmap_save(tract_level_map, filename="tractlevel.jpg")

## Part 4 and 5 - extracting specified tracts from the data and visualizing them.
one <- ExtractTracts(tract_all)
two <- ExtractTracts(tract_gds)
three <- ExtractTracts(tract_hin)
four <- ExtractTracts(tract_lin)
five <- ExtractTracts(tract_m)
six <- ExtractTracts(tract_min)
sev <- ExtractTracts(tract_o)
eit <- ExtractTracts(tract_tot)
nine <- ExtractTracts(tract_tu)
ten <- ExtractTracts(tract_y)


master <- data.frame(one$work_tract)
colnames(master) <- "work_tract"
master$S000 <- rowSums(one[c(2:1455)])
master$SA01 <- rowSums(two[c(2:1455)])
master$SA02 <- rowSums(three[c(2:1455)])
master$SA03 <- rowSums(four[c(2:1455)])
master$SE01 <- rowSums(five[c(2:1455)])
master$SE02 <- rowSums(six[c(2:1455)])
master$SE03 <- rowSums(sev[c(2:1455)])
master$SI01 <- rowSums(eit[c(2:1455)])
master$SI02 <- rowSums(nine[c(2:1455)])
master$SI03 <- rowSums(ten[c(2:1455)])

wa_tracts$work_tract <- wa_tracts$GEOID
cleaneddata <- merge(bellevue, master, by = "work_tract", all.x=TRUE)
cleaned_workdata<- cleaneddata %>% filter(work_tract == "53033023201" |work_tract == "53033023202" |work_tract == "53033023403" |work_tract == "53033023804")

# Part 4
cleaned_homedata <- merge(bell_relate, one, by = "work_tract", all.x = TRUE)

c <- cleaned_homedata %>%
  select(home_tract, geometry)
d <- cleaned_workdata %>%
  select(work_tract, geometry)
st_crs(d) <- st_crs(c)

## tmap mode set to plotting
map <- tm_shape(cleaneddata) +
    tm_polygons("work_tract") +
  tm_shape(cleaneddata) + 
    tm_polygons("S000") +
  tm_legend(show = FALSE) +
  tm_layout(main.title = "# Workers from Home to Work in Bellevue", 
            main.title.position = "center",
            main.title.size = 0.8)
tmap_save(map, filename="whrelation.jpg")

# Part 5 
wadata <- merge(wa_data, master, by = "work_tract", all.x=TRUE)
finalcomplete <- wadata %>% filter(work_tract == "53033023201" |work_tract == "53033023202" |work_tract == "53033023403" |work_tract == "53033023804")

map2 <- qtm(finalcomplete, fill = "blue", title = "Census Tracts in Downtown - Work Tracts")

map3 <- tm_shape(wadata) + tm_polygons("work_tract") + tm_shape(wadata) + tm_polygons("S000") + tmap_options(max.categories = 1454)

#Assign objects to number of jobs in finance and total jobs
belle_CNS10 <- wac_bell$CNS10
belle_C000 <- wac_bell$C000

# Part 6
#Creates a function that calculates LQ for each tract in Bellevue
LQ <- function(belle_CNS10, belle_C000) {
  indv <- belle_CNS10/belle_C000
  all <- sum(belle_CNS10)/sum(belle_C000)
  indv/all
}

#Add an LQ column to wac_bell
tract_LQ <- mutate(wac_bell, LQ = LQ(belle_CNS10, belle_C000))

bbox <- bb(tract_LQ, ext = 1.2)

#Map the LQ in Bellevue tracts with tmap
LQ_map <- tm_shape(tract_LQ, bbox = bbox, unit = "mi") +
  tm_fill(col = "LQ", title = "Location\nQuotient") +
  tm_borders() +
  tm_layout(title = "Location Quotient of Finance Jobs by Tract, Bellevue", frame.double.line = TRUE, bg.color = "grey85") +
  tm_compass(type = "4star", position = c("left", "top"), fontsize = 1) +
  tm_scale_bar(breaks = c(0, 2, 4), size = .7) +
  tm_legend(position = c("left", "bottom"))

#Tracts in the northwestern part of Bellevue have a greater concentration of finance jobs compared to
#the other tracts in the city. This can be explained in part by downtown Bellevue being located in/around
#those tracts with the higher concentration of finance jobs. Large businesses and corporations cluster 
#in downtown, likely meaning more jobs. 

