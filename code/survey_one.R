# Libraries
library(dplyr)
library(tidyverse)
library(raster)
library(sf)
library(sp)
library(ggplot2)
library(gridExtra)
library(elevatr)
library(ggspatial)
library(ggmap) #citation("ggmap")
library(leaflet)
library(htmlwidgets)
library(kableExtra)
library(maps)
library(spdep)
library(ggsn)
library(performance)
library(ggResidpanel)
library(reshape2)
library(moranfast) # remotes::install_github('mcooper/moranfast') #citation probably needed
library(deldir)
library(rgeos)
library(ggpubr)
# Data
## Function Junction
tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  knitr::kable(res)
}

voronoipolygons = function(layer) {
  require(deldir)
  crds = layer@coords
  z = deldir(crds[,1], crds[,2])
  w = tile.list(z)
  polys = vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP = SpatialPolygons(polys)
  voronoi = SpatialPolygonsDataFrame(SP, data=data.frame(dummy = seq(length(SP)), 
                                                         row.names=sapply(slot(SP, 'polygons'), 
                                                                          function(x) slot(x, 'ID'))))
}

moranfast <- function(x, c1, c2, alternative='two.sided') {
  res <- calc_moran(x, c1, c2)
  
  names(res) <- c('observed', 'expected', 'sd')
  res <- as.list(res)
  
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  pv <- pnorm(res$observed, mean = res$expected, sd = res$sd)
  if (alternative == "two.sided"){
    if (res$observed <= -1/(length(x) - 1)){
      pv <- 2 * pv
    }else{
      pv <- 2 * (1 - pv)
    }
  }
  if (alternative == "greater"){
    pv <- 1 - pv
  }
  
  res[['p.value']] <- pv
  
  return(res)
}

# voronoipolygons = function(layer) {
#   require(deldir)
#   crds = layer@coords
#   z = deldir(crds[,1], crds[,2],round=F, sort=F,rw=st_bbox(marshall.sp.aq1))
#   w = tile.list(z)
#   polys = vector(mode='list', length=length(w))
#   require(sp)
#   for (i in seq(along=polys)) {
#     pcrds = cbind(w[[i]]$x, w[[i]]$y)
#     pcrds = rbind(pcrds, pcrds[1,])
#     polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
#   }
#   SP = SpatialPolygons(polys)
#   voronoi = SpatialPolygonsDataFrame(SP, data=data.frame(dummy = seq(length(SP)), row.names=sapply(slot(SP, 'polygons'), 
#                                                                                                    function(x) slot(x, 'ID'))))
# }


## API Key for Geocoding:
api_key = "AIzaSyAeoVaM_45SQ4G-lHeci1RhRNbhpxO3BDc"
register_google(key = api_key) 

## Loading Datasets
waveone<-read.csv("/Volumes/research/Marshall Fire Health/marshall_w1_deID.csv")
wavetwo<-read.csv("/Volumes/research/Marshall Fire Health/marshall_w2_deID.csv")
mapwave1=read.csv("/Volumes/research/Marshall Fire Health/marshall_w1.csv")
# waveone<-read.csv("../data/marshall_w1_deID.csv")
# wavetwo<-read.csv("../data/marshall_w2_deID.csv")

## Colors to Use
ryg.palette<-c("#DB4325", "#EDA247", "#E6E1BC","#57C4AD", "#006164")
ryg.palette.long<-c("#DB4325", "#EDA247", "#E6E1BC","#57C4AD", "#006164", "grey")
ryg.palette.short<-c("#EDA247", "#E6E1BC","#57C4AD", "#006164")
factpal <- colorFactor(c("#d7191c","#fdae61","#ffffbf","#a6d96a","#1a9641"), 
                       c("Strongly disagree","Somewhat disagree", "Neither agree nor disagree",
                         "Somewhat agree","Strongly agree"))

## Variables
wave.one<-waveone %>% # recoding of mailing address has one "" response
  # randomly had to change to recode_factor?
  mutate(mailingcity=recode_factor(mailingcity, 'Superior'='SUPERIOR', 'UNINCORPORATED'='BOULDER')) %>% 
  mutate(across(air_quality_1:air_quality_4, as.factor)) %>%
  mutate(across(remediation_1:remediation_4, as.factor)) %>%
  mutate(across(air_cleaning_1:air_cleaning_4, as.factor)) %>%
  mutate(group=as.factor(group)) %>% 
  mutate(ownership_status=recode_factor(ownership_status,
                                        "1"="Owned",
                                        "2"="Rented",
                                        "3"="Owned but Not Living",
                                        "4"="Purchased after Dec. 30, 2021",
                                        "5"="Other")) %>%
  mutate(air_quality_1=recode_factor(air_quality_1,
                                     "1"="Strongly disagree",
                                     "2"="Somewhat disagree",
                                     "3"="Neither agree nor disagree",
                                     "4"="Somewhat agree",
                                     "5"="Strongly agree",
                                     "6"="Don't know")) %>%
  mutate(air_quality_2=recode_factor(air_quality_2,
                                     "1"="Strongly disagree",
                                     "2"="Somewhat disagree",
                                     "3"="Neither agree nor disagree",
                                     "4"="Somewhat agree",
                                     "5"="Strongly agree",
                                     "6"="Don't know")) %>%
  mutate(air_quality_3=recode_factor(air_quality_3,
                                     "1"="Strongly disagree",
                                     "2"="Somewhat disagree",
                                     "3"="Neither agree nor disagree",
                                     "4"="Somewhat agree",
                                     "5"="Strongly agree",
                                     "6"="Don't know")) %>%
  mutate(air_quality_4=recode_factor(air_quality_4,
                                     "1"="Strongly disagree",
                                     "2"="Somewhat disagree",
                                     "3"="Neither agree nor disagree",
                                     "4"="Somewhat agree",
                                     "5"="Strongly agree",
                                     "6"="Don't know")) %>% 
  mutate(home_smell_1week=recode_factor(home_smell_1week,
                                        "0"="No",
                                        "1"="Yes",
                                        "2"="Not sure/don't remember")) %>%
  mutate(home_smell_type=recode_factor(home_smell_type,
                                       "1"="Like a campfire",
                                       "2"="Like chemicals or a chemical fire",
                                       "3"="Other")) %>%
  mutate(group=recode_factor(group,
                             "A"="within fire perimeter",
                             "B"="within 1/2 mile",
                             "C"="1/2 to 1 mile",
                             "D"="1 to 2 miles")) %>%
  mutate(remediation_1=recode_factor(remediation_1, 
                                     "1"="Did this already",
                                     "2"="Plan to do this",
                                     "3"="Neither"))%>%
  mutate(remediation_2=recode_factor(remediation_2, 
                                     "1"="Did this already",
                                     "2"="Plan to do this",
                                     "3"="Neither"))%>%
  mutate(remediation_3=recode_factor(remediation_3, 
                                     "1"="Did this already",
                                     "2"="Plan to do this",
                                     "3"="Neither"))%>%
  mutate(remediation_4=recode_factor(remediation_4, 
                                     "1"="Did this already",
                                     "2"="Plan to do this",
                                     "3"="Neither"))%>%
  mutate(air_cleaning=recode_factor(air_cleaning_1, 
                                    "1"="Changing air filters in HVAC")) %>%
  mutate(air_cleaning=recode_factor(air_cleaning_2, 
                                    "1"="Using air cleaners"))%>%
  mutate(air_cleaning=recode_factor(air_cleaning_3, 
                                    "1"="Hiring someone to clean indoor air")) %>%
  mutate(air_cleaning=recode_factor(air_cleaning_4, 
                                    "1"="Other")) %>%
  mutate(air_cleaning_3=recode_factor(air_cleaning_3,
                                      "1"="Hiring someone to clean indoor air"))%>%
  mutate(air_cleaning_4=recode_factor(air_cleaning_4,
                                      "1"="Other")) %>%
  mutate(education=recode_factor(education,
                                 "0"="Did not specify", 
                                 "1"="High School Graduate",
                                 "2"="GED or Equivalent",
                                 "3"="Some College",
                                 "4"="Associate's Degree",
                                 "5"="Bachelor's Degree",
                                 "6"="Master's Degree",
                                 "7"="Doctoral Degree")) %>%
  mutate(income=recode_factor(income,
                              "1"="Less than $20,000", 
                              "2"="$20,000 to $34,999",
                              "3"="$35,000 to $49,999",
                              "4"="$50,000 to $79,999",
                              "5"="$80,000 to $99,999",
                              "6"="$100,000 to $149,999",
                              "7"="$150,000 to $199,999",
                              "8"="$200,000 or more",
                              "9"="Prefer not to answer")) %>%
  filter(finished==1) %>% #only analyzing finished data, not in wave.two
  #filtering only respondents in Boulder Unincorporated, Superior or Louisville
  filter(mailingcity=="BOULDER"|mailingcity=="SUPERIOR"|mailingcity=="LOUISVILLE") 

wave.two<-wavetwo %>%
  mutate(air_quality_2=as.factor(air_quality_2)) %>%
  mutate(air_quality_4=as.factor(air_quality_4)) %>%
  mutate(across(remediation_1:remediation_4, as.factor)) %>%
  mutate(across(air_cleaning_1:air_cleaning_4, as.factor)) %>%
  mutate(group=as.factor(group)) %>% 
  mutate(renterowner=recode_factor(renterowner,
                                        "1"="Owned",
                                        "2"="Rented")) %>%
  mutate(air_quality_2=recode_factor(air_quality_2,
                                     "1"="Strongly disagree",
                                     "2"="Somewhat disagree",
                                     "3"="Neither agree nor disagree",
                                     "4"="Somewhat agree",
                                     "5"="Strongly agree",
                                     "6"="Don't know")) %>%
  mutate(air_quality_4=recode_factor(air_quality_4,
                                     "1"="Strongly disagree",
                                     "2"="Somewhat disagree",
                                     "3"="Neither agree nor disagree",
                                     "4"="Somewhat agree",
                                     "5"="Strongly agree",
                                     "6"="Don't know")) %>%
  mutate(group=recode_factor(group,
                             "A"="within fire perimeter",
                             "B"="within 1/2 mile",
                             "C"="1/2 to 1 mile",
                             "D"="1 to 2 miles")) %>%
  mutate(remediation_1=recode_factor(remediation_1, 
                                     "1"="Did this already",
                                     "2"="Plan to do this",
                                     "3"="Neither")) %>%
  mutate(remediation_2=recode_factor(remediation_2, 
                                     "1"="Did this already",
                                     "2"="Plan to do this",
                                     "3"="Neither")) %>% 
  mutate(remediation_3=recode_factor(remediation_3, 
                                     "1"="Did this already",
                                     "2"="Plan to do this",
                                     "3"="Neither")) %>%
  mutate(remediation_4=recode_factor(remediation_4,
                                     "1"="Did this already",
                                     "2"="Plan to do this",
                                     "3"="Neither")) %>%
  mutate(air_cleaning=recode_factor(air_cleaning_1, 
                                    "1"="Changing air filters in HVAC")) %>%
  mutate(air_cleaning=recode_factor(air_cleaning_2, 
                                    "1"="Using air cleaners")) %>%
  mutate(air_cleaning=recode_factor(air_cleaning_3, 
                                    "1"="Hiring someone to clean indoor air")) %>%
  mutate(air_cleaning=recode_factor(air_cleaning_4,
                                    "1"="Other")) %>%
  mutate(income=recode_factor(income,
                              "1"="Less than $20,000", 
                              "2"="$20,000 to $34,999",
                              "3"="$35,000 to $49,999",
                              "4"="$50,000 to $79,999",
                              "5"="$80,000 to $99,999",
                              "6"="$100,000 to $149,999",
                              "7"="$150,000 to $199,999",
                              "8"="$200,000 or more",
                              "9"="Prefer not to answer")) %>%
  mutate(education=recode_factor(education,
                                 "0"="Did not specify", 
                                 "1"="High School Graduate",
                                 "2"="GED or Equivalent",
                                 "3"="Some College",
                                 "4"="Associate's Degree",
                                 "5"="Bachelor's Degree",
                                 "6"="Master's Degree",
                                 "7"="Doctoral Degree"))

## geocoding - assigning lats and longs via Google API - will need key: ~5 errors
geo_marshall=mapwave1%>% #geocoding only completed surveys and those with lat and long
  mutate(mailingcity=recode_factor(mailingcity, 'Superior'='SUPERIOR', 'UNINCORPORATED'='BOULDER')) %>%
  mutate(across(air_quality_1:air_quality_4, as.factor)) %>%
  mutate(across(remediation_1:remediation_4, as.factor)) %>%
  mutate(across(air_cleaning_1:air_cleaning_4, as.factor)) %>%
  mutate(group=as.factor(group)) %>% 
  mutate(ownership_status=recode_factor(ownership_status,
                                        "1"="Owned",
                                        "2"="Rented",
                                        "3"="Owned but Not Living",
                                        "4"="Purchased after Dec. 30, 2021",
                                        "5"="Other")) %>%
  mutate(air_quality_1=recode_factor(air_quality_1,
                                     "1"="Strongly disagree",
                                     "2"="Somewhat disagree",
                                     "3"="Neither agree nor disagree",
                                     "4"="Somewhat agree",
                                     "5"="Strongly agree",
                                     "6"="Don't know")) %>%
  mutate(air_quality_2=recode_factor(air_quality_2,
                                     "1"="Strongly disagree",
                                     "2"="Somewhat disagree",
                                     "3"="Neither agree nor disagree",
                                     "4"="Somewhat agree",
                                     "5"="Strongly agree",
                                     "6"="Don't know")) %>%
  mutate(air_quality_3=recode_factor(air_quality_3,
                                     "1"="Strongly disagree",
                                     "2"="Somewhat disagree",
                                     "3"="Neither agree nor disagree",
                                     "4"="Somewhat agree",
                                     "5"="Strongly agree",
                                     "6"="Don't know")) %>%
  mutate(air_quality_4=recode_factor(air_quality_4,
                                     "1"="Strongly disagree",
                                     "2"="Somewhat disagree",
                                     "3"="Neither agree nor disagree",
                                     "4"="Somewhat agree",
                                     "5"="Strongly agree",
                                     "6"="Don't know")) %>% 
  mutate(home_smell_1week=recode_factor(home_smell_1week,
                                        "0"="No",
                                        "1"="Yes",
                                        "2"="Not sure/don't remember")) %>%
  mutate(home_smell_type=recode_factor(home_smell_type,
                                       "1"="Like a campfire",
                                       "2"="Like chemicals or a chemical fire",
                                       "3"="Other")) %>%
  mutate(group=recode_factor(group,
                             "A"="within fire perimeter",
                             "B"="within 1/2 mile",
                             "C"="1/2 to 1 mile",
                             "D"="1 to 2 miles")) %>%
  mutate(remediation_1=recode_factor(remediation_1, 
                                     "1"="Did this already",
                                     "2"="Plan to do this",
                                     "3"="Neither"))%>%
  mutate(remediation_2=recode_factor(remediation_2, 
                                     "1"="Did this already",
                                     "2"="Plan to do this",
                                     "3"="Neither"))%>%
  mutate(remediation_3=recode_factor(remediation_3, 
                                     "1"="Did this already",
                                     "2"="Plan to do this",
                                     "3"="Neither"))%>%
  mutate(remediation_4=recode_factor(remediation_4, 
                                     "1"="Did this already",
                                     "2"="Plan to do this",
                                     "3"="Neither"))%>%
  mutate(air_cleaning=recode_factor(air_cleaning_1, 
                                    "1"="Changing air filters in HVAC")) %>%
  mutate(air_cleaning=recode_factor(air_cleaning_2, 
                                    "1"="Using air cleaners"))%>%
  mutate(air_cleaning=recode_factor(air_cleaning_3, 
                                    "1"="Hiring someone to clean indoor air")) %>%
  mutate(air_cleaning=recode_factor(air_cleaning_4, 
                                    "1"="Other")) %>%
  mutate(air_cleaning_3=recode_factor(air_cleaning_3,
                                      "1"="Hiring someone to clean indoor air"))%>%
  mutate(air_cleaning_4=recode_factor(air_cleaning_4,
                                      "1"="Other")) %>%
  mutate(education=recode_factor(education,
                                 "0"="Did not specify", 
                                 "1"="High School Graduate",
                                 "2"="GED or Equivalent",
                                 "3"="Some College",
                                 "4"="Associate's Degree",
                                 "5"="Bachelor's Degree",
                                 "6"="Master's Degree",
                                 "7"="Doctoral Degree")) %>%
  mutate(income=recode_factor(income,
                              "1"="Less than $20,000", 
                              "2"="$20,000 to $34,999",
                              "3"="$35,000 to $49,999",
                              "4"="$50,000 to $79,999",
                              "5"="$80,000 to $99,999",
                              "6"="$100,000 to $149,999",
                              "7"="$150,000 to $199,999",
                              "8"="$200,000 or more",
                              "9"="Prefer not to answer")) %>%
  filter(finished==1) %>% #only analyzing finished data, not in wave.two
  filter(mailingcity=="BOULDER"|mailingcity=="SUPERIOR"|mailingcity=="LOUISVILLE")%>% 
  mutate(full_address = paste0(.$mailingaddr1,", ",.$mailingcity,", ", .$mailingstate,", ",.$mailingzip)) %>% 
  mutate_geocode(full_address, output="latlona") 

geo_marshall_1=filter(geo_marshall,!is.na(air_quality_1)) %>% 
  st_as_sf(coords=c("lon","lat"), crs = 4326)
marshall.sp.aq1 = as(geo_marshall_1,"Spatial")

geo_marshall_2=filter(geo_marshall,!is.na(air_quality_2)) %>% 
  st_as_sf(coords=c("lon","lat"), crs = 4326)
marshall.sp.aq2 = as(geo_marshall_2,"Spatial")

geo_marshall_3=filter(geo_marshall,!is.na(air_quality_3)) %>% 
  st_as_sf(coords=c("lon","lat"), crs = 4326)
marshall.sp.aq3 = as(geo_marshall_3,"Spatial")

geo_marshall_4=filter(geo_marshall,!is.na(air_quality_4)) %>% 
  st_as_sf(coords=c("lon","lat"), crs = 4326)
marshall.sp.aq4 = as(geo_marshall_4,"Spatial")

# Survey Data
## Number of respondants
# nrow(waveone)
# nrow(wavetwo)

## Number of respondants who completed the survey
# Respondants = c(nrow(wave.one),nrow(wave.two))
# Wave=c("One","Two")
# df1 = cbind(Wave,Respondants) %>% 
#   as.data.frame()
# # save_kable(knitr::kable(df1),"images/w1w2_counts.png")

#Percent male and Female -> There is Binary/Decl
# save_kable(tblFun(wave.one$Gender3),"images/w1.gender.table.png")
# save_kable(tblFun(wave.two$Gender3),"images/w2.gender.table.png")

# Education attainment
# save_kable(tblFun(wave.one$education),"images/w1.edu.table.png")
# save_kable(tblFun(wave.two$education),"images/w2.edu.table.png")

### Income for each wave 
# save_kable(tblFun(wave.one$income),"images/w1.income.table.png")
# save_kable(tblFun(wave.two$income),"images/w2.income.table.png")

### Race/ethnicity
# save_kable(tblFun(wave.one$RaceEthn2),"images/w1.ethn.table.png")
# save_kable(tblFun(wave.two$RaceEthn2),"images/w2.ethn.table.png")

### Owner/Renter 
# save_kable(tblFun(wave.one$ownership_status),"images/w1.ownrent.table.png")
# save_kable(tblFun(wave.two$renterowner),"images/w2.ownrent.table.png")

### Impact category 
# save_kable(tblFun(wave.one$impact_cat),"images/w1.impact.table.png")
# save_kable(tblFun(wave.two$impact_cat),"images/w2.impact.table.png")

### Distance category 
# save_kable(tblFun(wave.one$group),"images/w1.dist.table.png")
# save_kable(tblFun(wave.two$group),"images/w2.dist.table.png")

# Air Quality Perceptions

## Wave One:

### Wave One only: compare before and after the fire – perception of air quality in their neighborhood 
#trying a vertical graph
w1.before.neigh.plot=ggplot(filter(wave.one,!is.na(air_quality_1)), aes(fill=air_quality_1,x="Before the Fire")) +
  geom_bar(position="stack") +
  labs(title = "I am confident that the air in my \n neighborhood is safe to breathe") +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))
w1.after.neigh.plot=ggplot(filter(wave.one,!is.na(air_quality_2)), aes(fill=air_quality_2,x="After the Fire")) +
  geom_bar(position="stack") +
  xlab(NULL) +
  ylab("Count") +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))
grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot)
# ggsave("images/wave1.neigh.AQ1_2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))

### Wave One only: compare before and after the fire – perception of air quality in their neighborhood by distance 
w1.before.neigh.dist.plot = ggplot(filter(wave.one,!is.na(air_quality_1)), aes(fill=air_quality_1, x=group)) +
  geom_bar(position="stack") +
  labs(title = "Before the Marshall Fire, I was confident that the air \n in my neighborhood was safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)+
  theme(plot.title = element_text(hjust = 0.5))
w1.after.neigh.dist.plot = ggplot(filter(wave.one,!is.na(air_quality_2)), aes(fill=air_quality_2, x=group)) +
  geom_bar(position="stack") +
  labs(title = "Currently, I am confident that the air in my neighborhood \n is safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(w1.before.neigh.dist.plot,w1.after.neigh.dist.plot)
# ggsave("images/wave1.neigh.dist.AQ1_2.png",plot=grid.arrange(w1.before.neigh.dist.plot,w1.after.neigh.dist.plot))

### Wave One only: compare before and after the fire – perception of air quality in their neighborhood by impact category 
w1.before.neigh.impact.plot=ggplot(filter(wave.one,!is.na(air_quality_1)), aes(fill=air_quality_1, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = "Before the Marshall Fire, I was confident that the air in \n my neighborhood was safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5))
w1.after.neigh.impact.plot=ggplot(filter(wave.one,!is.na(air_quality_2)), aes(fill=air_quality_2, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = "Currently, I am confident that the air in my neighborhood \n is safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(w1.before.neigh.impact.plot,w1.after.neigh.impact.plot)
# ggsave("images/wave1.neigh.impact.AQ1_2.png",plot=grid.arrange(w1.before.neigh.impact.plot,w1.after.neigh.impact.plot))

### Wave One only: compare before and after the fire – perception of air quality in their home 
#trying a horizontal graph
w1.before.home.plot=ggplot(filter(wave.one,!is.na(air_quality_3)), aes(fill=air_quality_3,y="Before the Fire")) +
  geom_bar(position="stack") +
  labs(title = "I am confident that the air inside my home is safe to breathe") +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))
w1.after.home.plot=ggplot(filter(wave.one,!is.na(air_quality_4)), aes(fill=air_quality_4,y="After the Fire")) +
  geom_bar(position="stack") +
  ylab(NULL) +
  xlab("Count") +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))
grid.arrange(w1.before.home.plot,w1.after.home.plot)
# ggsave("images/wave1.home.AQ3_4.png",plot=grid.arrange(w1.before.home.plot,w1.after.home.plot))

### Wave One only: compare before and after the fire – perception of air quality in their home by distance 
w1.before.home.dist.plot=ggplot(filter(wave.one,!is.na(air_quality_3)), aes(fill=air_quality_3, x=group)) +
  geom_bar(position="stack") +
  labs(title = "Before the Marshall Fire, I was confident that the air inside \n my home was safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
w1.after.home.dist.plot=ggplot(filter(wave.one,!is.na(air_quality_4)), aes(fill=air_quality_4, x=group)) +
  geom_bar(position="stack") +
  labs(title = "Currently, I am confident that the air inside my home is \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
grid.arrange(w1.before.home.dist.plot,w1.after.home.dist.plot)
# ggsave("images/wave1.home.dist.AQ3_4.png",plot=grid.arrange(w1.before.home.dist.plot,w1.after.home.dist.plot))

### Wave One only: compare before and after the fire – perception of air quality in their home by impact category
w1.before.home.impact.plot=ggplot(filter(wave.one,!is.na(air_quality_3)), aes(fill=air_quality_3, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = "Before the Marshall Fire, I was confident that the air inside my home was \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)+
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))
w1.after.home.impact.plot=ggplot(filter(wave.one,!is.na(air_quality_4)), aes(fill=air_quality_4, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = "Currently, I am confident that the air inside my home is \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)+
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))
grid.arrange(w1.before.home.impact.plot,w1.after.home.impact.plot)
# ggsave("images/wave1.home.impact.AQ3_4.png",plot=grid.arrange(w1.before.home.impact.plot,w1.after.home.impact.plot))

## Wave One and Two:
### Wave One to Wave Two comparison – perception of air quality in neighborhood (AQ2) 
w1.after.neigh.plot=ggplot(filter(wave.one,!is.na(air_quality_2)), aes(fill=air_quality_2,y="Wave One")) +
  geom_bar(position="stack") +
  labs(title = "I am confident that the air in my neighborhood is safe to breathe") +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))
w2.after.neigh.plot=ggplot(filter(wave.two,!is.na(air_quality_2)), aes(fill=air_quality_2,y="Wave Two")) +
  geom_bar(position="stack") +
  ylab(NULL) +
  xlab("Count") +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))
grid.arrange(w1.after.neigh.plot,w2.after.neigh.plot)
# ggsave("images/w1xw2.neigh.AQ2.png",plot=grid.arrange(w1.after.neigh.plot,w2.after.neigh.plot))

### Wave One to Wave Two comparison – perception of air quality in neighborhood (AQ2) by distance 
w1.after.neigh.dist.plot=ggplot(filter(wave.one,!is.na(air_quality_2)), aes(fill=air_quality_2,y=group)) +
  geom_bar(position="stack") +
  labs(title = "I am confident that the air in my neighborhood is safe to breathe") +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))
w2.after.neigh.dist.plot=ggplot(filter(wave.two,!is.na(air_quality_2)), aes(fill=air_quality_2,y=group)) +
  geom_bar(position="stack") +
  ylab(NULL) +
  xlab("Count") +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))
grid.arrange(w1.after.neigh.dist.plot,w2.after.neigh.dist.plot)
# ggsave("images/w1xw2.neigh.dist.AQ2.png",plot=grid.arrange(w1.after.neigh.dist.plot,w2.after.neigh.dist.plot))

### Wave One to Wave Two comparison – perception of air quality in neighborhood (AQ2) by impact category 
w1.after.neigh.impact.plot=ggplot(filter(wave.one,!is.na(air_quality_2)), aes(fill=air_quality_2, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = "Before the Marshall Fire, I was confident that the air in my neighborhood was \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
w2.after.neigh.impact.plot=ggplot(filter(wave.two,!is.na(air_quality_2)), aes(fill=air_quality_2, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = "Before the Marshall Fire, I was confident that the air in my neighborhood was \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
grid.arrange(w1.after.neigh.impact.plot,w2.after.neigh.impact.plot)
# ggsave("images/w1xw2.neigh.impact.AQ2.png",plot=grid.arrange(w1.after.neigh.impact.plot,w2.after.neigh.impact.plot))

### Wave One to Wave Two comparison – perception of air quality in home (AQ4) 
w1.after.home.plot=ggplot(filter(wave.one,!is.na(air_quality_4)), aes(fill=air_quality_4,y="Wave One")) +
  geom_bar(position="stack") +
  labs(title = "I am confident that the air inside my home is safe to breathe") +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))
w2.after.home.plot=ggplot(filter(wave.two,!is.na(air_quality_4)), aes(fill=air_quality_4,y="Wave Two")) +
  geom_bar(position="stack") +
  ylab(NULL) +
  xlab("Count") +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))
grid.arrange(w1.after.home.plot,w2.after.home.plot)
# ggsave("images/w1xw2.home.AQ4.png",plot=grid.arrange(w1.after.home.plot,w2.after.home.plot))

### Wave One to Wave Two comparison – perception of air quality in home (AQ4) by distance 
w1.after.home.dist.plot=ggplot(filter(wave.one,!is.na(air_quality_4)), aes(fill=air_quality_4, x=group)) +
  geom_bar(position="stack") +
  labs(title = "I am confident that the air inside my home is \n safe to breathe",
       subtitle = "Wave One",
       x = NULL, fill ="") +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) 
w2.after.home.dist.plot=ggplot(filter(wave.two,!is.na(air_quality_4)), aes(fill=air_quality_4, x=group)) +
  geom_bar(position="stack") +
  labs(title = NULL,
       subtitle = "Wave Two",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) 
grid.arrange(w1.after.home.dist.plot,w2.after.home.dist.plot)
# ggsave("images/w1xw2.home.dist.AQ4.png",plot=grid.arrange(w1.after.home.dist.plot,w2.after.home.dist.plot))

### Wave One to Wave Two comparison – perception of air quality in home (AQ4) by impact category 
w1.after.home.impact.plot=ggplot(filter(wave.one,!is.na(air_quality_4)), aes(fill=air_quality_4, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = "I am confident that the air inside my home was \n safe to breathe",
       subtitle = "Wave One",
       x = NULL, fill ="") +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) 
w2.after.home.impact.plot=ggplot(filter(wave.two,!is.na(air_quality_4)), aes(fill=air_quality_4, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = NULL,
       subtitle = "Wave Two",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) 
grid.arrange(w1.after.home.impact.plot,w2.after.home.impact.plot)
# ggsave("images/w1xw2.home.impact.AQ4.png",plot=grid.arrange(w1.after.home.impact.plot,w2.after.home.impact.plot))

# Map

## Impact Category

# geo_marshall_impact=filter(geo_marshall,!is.na(impact_cat)) %>% 
#   st_as_sf(coords=c("lon","lat"), crs = 4326)
# marshall.sp.impact = as(geo_marshall_impact,"Spatial")
# factpal <- colorFactor(c("#d7191c","#fdae61","#ffffbf","#a6d96a","#1a9641"), 
#                        c("Complete loss", "Damaged, living there","Damaged, not living there",
#                          "No damage, living there","No damage, not living there"))
# 
# marshall_pts_impact_map_w1_leaf = leaflet(marshall.sp.impact) %>% 
#   addTiles() %>% 
#   # addPolygons(color="red",
#   #             opacity=0.5) %>% 
#   addCircleMarkers(group=test$impact_cat, color = ~factpal(impact_cat),
#                    radius = 1,
#                    opacity = 1,
#                    lng = marshall.sp.aq1@coords[,1],
#                    lat = marshall.sp.aq1@coords[,2])
# saveWidget(marshall_pts_impact_map_w1_leaf, file="images/marshall_impact_map_w1.html")
# 
# ggplot(geo_marshall) + 
#   annotation_map_tile() +
#   annotation_scale() +
#   geom_sf(aes(color=impact_cat)) +
#   scale_color_manual(values = c("Complete loss" = "#d7191c", 
#                                 "Damaged, living there" = "#fdae61",
#                                 "Damaged, not living there" = "#ffffbf",
#                                 "No damage, living there" = "#a6d96a",
#                                 "No damage, not living there" = "#1a9641")) +
#   ggtitle("Marshall Fire Survey Results", subtitle=paste0("Total Respondants Displayed: ",nrow(geo_marshall))) + 
#   theme(plot.title = element_text(hjust=0.5),
#         plot.subtitle = element_text(hjust=0.5)) +
#   guides(colour=guide_legend(title="Impact Level"))
# ggsave("images/map1.impact.cat.png", height = 7.5, width = 7.5)

### Map of perception of air quality in one’s neighborhood  - color from stacked plot colors 
#### Before the Fire

# leaflet(marshall.sp.aq1) %>%
#   addTiles() %>%
#   addCircleMarkers(group=marshall.sp.aq1$air_quality_1, color = ~factpal(air_quality_1),
#                    radius = 1,
#                    opacity = 1,
#                    lng = marshall.sp.aq1@coords[,1],
#                    lat = marshall.sp.aq1@coords[,2])

ggplot(filter(geo_marshall_1,!is.na(air_quality_1))) +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(color=as.factor(air_quality_1))) +
  scale_color_manual(values = c("Strongly disagree"="#d7191c",
                                "Somewhat disagree"="#fdae61",
                                "Neither agree nor disagree"="#ffffbf",
                                "Somewhat agree"= "#a6d96a",
                                "Strongly agree" = "#1a9641")) +
  labs(title="Marshall Fire Survey Results (NA's filtered out)", 
       subtitle=paste0("Wave One: Total Respondants Displayed: ",nrow(filter(geo_marshall_1,!is.na(air_quality_1)))),
       caption="Statement: Before the Marshall Fire, I was confident that the \n air in my neighborhood was safe to breath") +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5)) +
  guides(colour=guide_legend(title=""))
# ggsave("images/map1.AQ1.png", height = 7.5, width = 7.5)

#### After the fire

# leaflet(marshall.sp.aq2) %>%
#   addTiles() %>%
#   addCircleMarkers(group=marshall.sp.aq2$air_quality_2, color = ~factpal(air_quality_2),
#                    radius = 1,
#                    opacity = 1,
#                    lng = marshall.sp.aq2@coords[,1],
#                    lat = marshall.sp.aq2@coords[,2])

ggplot(filter(geo_marshall_2,!is.na(air_quality_2))) +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(color=as.factor(air_quality_2))) +
  scale_color_manual(values = c("Strongly disagree"="#d7191c",
                                "Somewhat disagree"="#fdae61",
                                "Neither agree nor disagree"="#ffffbf",
                                "Somewhat agree"= "#a6d96a",
                                "Strongly agree" = "#1a9641")) +
  labs(title="Marshall Fire Survey Results (NA's filtered out)", 
       subtitle=paste0("Wave One: Total Respondants Displayed: ",nrow(filter(geo_marshall_2,!is.na(air_quality_2)))),
       caption="Statement: Currently, I am confident that the air in \n my neighborhood is safe to breath") +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5)) +
  guides(colour=guide_legend(title=""))
# ggsave("images/map1.AQ2.png", height = 7.5, width = 7.5)

### Map of perception of air quality in one’s home - color from stacked plot colors 
#### Before the Fire

# leaflet(marshall.sp.aq3) %>%
#   addTiles() %>%
#   addCircleMarkers(group=marshall.sp.aq3$air_quality_3, color = ~factpal(air_quality_3),
#                    radius = 1,
#                    opacity = 1,
#                    lng = marshall.sp.aq3@coords[,1],
#                    lat = marshall.sp.aq3@coords[,2])

ggplot(filter(geo_marshall_3,!is.na(air_quality_3))) +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(color=as.factor(air_quality_3))) +
  scale_color_manual(values = c("Strongly disagree"="#d7191c",
                                "Somewhat disagree"="#fdae61",
                                "Neither agree nor disagree"="#ffffbf",
                                "Somewhat agree"= "#a6d96a",
                                "Strongly agree" = "#1a9641")) +
  labs(title="Marshall Fire Survey Results (NA's filtered out)", 
       subtitle=paste0("Wave One: Total Respondants Displayed: ",nrow(filter(geo_marshall_3,!is.na(air_quality_3)))),
       caption="Statement: Currently, I am confident that the air in \n my neighborhood is safe to breath") +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5)) +
  guides(colour=guide_legend(title=""))
# ggsave("images/map1.AQ3.png", height = 7.5, width = 7.5)

#### After the Fire

# leaflet(marshall.sp.aq4) %>%
#   addTiles() %>%
#   addCircleMarkers(group=marshall.sp.aq4$air_quality_4, color = ~factpal(air_quality_4),
#                    radius = 1,
#                    opacity = 1,
#                    lng = marshall.sp.aq4@coords[,1],
#                    lat = marshall.sp.aq4@coords[,2])

ggplot(filter(geo_marshall_4,!is.na(air_quality_4))) +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(color=as.factor(air_quality_4))) +
  scale_color_manual(values = c("Strongly disagree"="#d7191c",
                                "Somewhat disagree"="#fdae61",
                                "Neither agree nor disagree"="#ffffbf",
                                "Somewhat agree"= "#a6d96a",
                                "Strongly agree" = "#1a9641")) +
  labs(title="Marshall Fire Survey Results (NA's filtered out)", 
       subtitle=paste0("Wave One: Total Respondants Displayed: ",nrow(filter(geo_marshall_4,!is.na(air_quality_4)))),
       caption="Statement: Currently, I am confident that the air in \n my neighborhood is safe to breath") +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5)) +
  guides(colour=guide_legend(title=""))
# ggsave("images/map1.AQ4.png", height = 7.5, width = 7.5)

# Moran's I - Basic

moranfast(marshall.sp.aq1$air_quality_1, marshall.sp.aq1@coords[,1],marshall.sp.aq1@coords[,2])
calc_moran(marshall.sp.aq1$air_quality_1, marshall.sp.aq1@coords[,1],marshall.sp.aq1@coords[,2])
moranfast(marshall.sp.aq2$air_quality_2, marshall.sp.aq2@coords[,1],marshall.sp.aq2@coords[,2])
calc_moran(marshall.sp.aq3$air_quality_3, marshall.sp.aq3@coords[,1],marshall.sp.aq3@coords[,2])
moranfast(marshall.sp.aq3$air_quality_3, marshall.sp.aq3@coords[,1],marshall.sp.aq3@coords[,2])
calc_moran(marshall.sp.aq3$air_quality_3, marshall.sp.aq3@coords[,1],marshall.sp.aq3@coords[,2])
moranfast(marshall.sp.aq4$air_quality_4, marshall.sp.aq4@coords[,1],marshall.sp.aq4@coords[,2])
calc_moran(marshall.sp.aq4$air_quality_4, marshall.sp.aq4@coords[,1],marshall.sp.aq4@coords[,2])

# Moran's I - Voroni
##### Using BB Boundary
vp <- voronoipolygons(marshall.sp.aq1)
proj4string(vp) = proj4string(marshall.sp.aq1)
# gDifference(vp, marshall.sp.aq1)
v2 <- vp %>%
  st_as_sf(4326)
coords = vp %>% 
  st_as_sf(4326) %>% 
  mutate(dummy=1) %>% 
  st_centroid()

plot(vp)
plot(geo_marshall_1$geometry, col="red", add=T)
plot(find, col="green", add=T)
plot(coords$geometry, col="blue", add=T)

split=over(vp,marshall.sp.aq1)
find=marshall.sp.aq1[which(marshall.sp.aq1$surveyid %in% split$surveyid),]
marshall.vor.q=poly2nb(vp, queen=T)
marshall.vor.r=poly2nb(vp, queen=F)
listw.queen <- nb2listw(marshall.vor.q)
find$air_quality_1 = as.integer(find$air_quality_1)
globalMoran <- moran.test(find$air_quality_1, listw.queen)
localMoran <- localmoran(find$air_quality_1, listw.queen)
globalMoran 
localMoran

moran.plot(find$air_quality_1, listw.queen, 
           xlab="Perceptions of Air Quality", 
           ylab="Spatially Lagged Variable", 
           main="Moran Plot")

moran.map <- cbind(v2, localMoran)

ggplot(data = moran.map) +
  geom_sf(aes(fill= Ii)) +
  scale_fill_gradient(low="white", high="blue") +
  geom_sf(data=geo_marshall_1,aes(color=air_quality_1), pch=4)  +
  scale_color_manual(values = c("Strongly disagree"="#d7191c",
                                "Somewhat disagree"="#fdae61",
                                "Neither agree nor disagree"="#ffffbf",
                                "Somewhat agree"= "#a6d96a",
                                "Strongly agree" = "#1a9641")) +
  labs(title='Local Moran\'s I based on Air Quality Perceptions \n Before the Fire')

mapClustered.moran = function(data, var, local_moran, case){
  #add TryCatch() to make sure var is a character string or find diff work around
  quadrant <- vector(mode="numeric",length=nrow(local_moran))
  
  mean=(lapply(data[,var], mean, na.rm = TRUE)[var]) #not sure why mean() wouldn't work by it self
  m.qualification <- data[,var] - mean  
  
  m.local <- local_moran[,1] - mean(local_moran[,1])    
  
  signif <- 0.1 
  
  quadrant[m.qualification[,1] > 0 & m.local> 0] <- 4  
  quadrant[m.qualification[,1] < 0 & m.local< 0] <- 1      
  quadrant[m.qualification[,1] < 0 & m.local> 0] <- 2
  quadrant[m.qualification[,1] > 0 & m.local< 0] <- 3
  quadrant[local_moran[,5]>signif] <- 0   
  
  
  # #plot side-by-side
  # par(mfrow=c(1,2)) not working :((((((
  
  # plot clusters
  data_sp = as(data, "Spatial")
  brks <- c(0,1,2,3,4)
  colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
  plot(data_sp,border="lightgray",
       col=colors[findInterval(quadrant,brks,all.inside=FALSE)],
       main="Local Clusters Identified using Moran's I",
       sub = paste0(case,"\'s Case"))
  box()
  legend("bottomleft", legend = c("insignificant","low-low","low-high","high-low","high-high"),
         fill=colors,bty="n")
}

mapLocal.moran = function(data, local_moran){
  moran.map <- cbind(data, local_moran)
  ggplot(data = moran.map) +
    geom_sf(aes(fill= Ii)) +
    scale_fill_gradient(low='red', high='green') +
    ggtitle('Local Moran\'s I Map')+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}

local_LE <- localmoran(x = nepal$LE, listw=listw.queen)

mapClustered.moran(find, "air_quality_1", localMoran, "Queen")
mapLocal.moran(v2,localMoran)
# plot(vp)
# plot(marshall.vor.q, coords$geometry, col='red', add=T)
# title(main="Voroni Neighbor Links: Queen's Case")
 
# plot(vp)
# plot(marshall.vor.r, coords, col='blue', add=T)
# title(main="Voroni Neighbor Links: Rooks Case")

# marshall.vor.diff=diffnb(marshall.vor.q,marshall.vor.r)
# plot(vp)
# plot(marshall.vor.diff, coords, col='purple', add=T)
# title(main="Voroni Neighbor Links: Difference")

# We can also expand the neighborhood into a higher order of contiguity
# more.vor.q<-nblag(marshall.vor.q,3)
# plot(vp)
# plot(more.vor.q[[2]], coords$geometry, col='red', add=T)
# title(main="Cont. 2")

####### Difference shows no new connections

# Physical Health Symptoms

## Wave One:

### Wave One counts of symptoms bar plot or pie chart 
wave1.symptoms=grep("^symptoms_\\d", names(wave.one), value=T)
rnames=c("Dry Cough","Wet Cough","Wheeze","Itchy or watery eyes",
         "Sore Throat","Headache","Shortness of Breath",
         "Difficult or labored breathing","Sneezing or stuffy nose",
         "Nausea or vomiting ","Allergic skin reaction",
         "Strange taste in your mouth","None of these")
count=list()
for(i in wave1.symptoms) {
  v=sum(!is.na(wave.one[,i]))
  count=c(count,v)
}
df_sym=cbind(wave1.symptoms,count,rnames) %>% 
  as.data.frame() %>% 
  dplyr::select(rnames,count)
df_sym$count=as.integer(df_sym$count)
colnames(df_sym)=c("Symptom","Count")
# final_df = df %>% 
#   mutate(Percent=round(Count/nrow(wave.one)*100,2))
b=barplot(df_sym$Count, names.arg=df_sym$Symptom, las=2,
          main="Wave One - Count of Symptoms")
text(b, df_sym$Count-10, df_sym$Count, font=2)
#save("images/symptom_barchart.png")
## save_kable(knitr::kable(df_sym), "images/symptoms_count.png")

### Wave One table of symptom counts by distance category 
count.dist=v=wave.one %>% 
  filter(!is.na(.[,wave1.symptoms[1]])) %>% 
  group_by(group) %>%
  count(.[,wave1.symptoms[1]]) %>% 
  as.data.frame() %>% 
  mutate(symptom=rnames[1]) %>% 
  dplyr::select(group,symptom,n)
for(i in 2:length(wave1.symptoms)) {
  v=wave.one %>% 
    filter(!is.na(.[,wave1.symptoms[i]])) %>% 
    group_by(group) %>%
    count(.[,wave1.symptoms[i]]) %>% 
    as.data.frame() %>% 
    mutate(symptom=rnames[i]) %>% 
    dplyr::select(group,symptom,n)
  count.dist=rbind(count.dist,v)
}
colnames(count.dist)=c("Distance","Symptom", "Count")
ggplot(data=count.dist, aes(x=Symptom,y=Count,fill=Distance)) +
  geom_bar(stat="identity") + 
  labs(title="Wave One Symptoms - Distance",
       subtitle = "I can change the colors of these graphs")
ggplot(data=count.dist, aes(x=Distance,y=Count,fill=Symptom)) +
  geom_bar(stat="identity",
           subtitle = "Was curious what this would look like \n Might be an okay % graph idea") + 
  labs(title="Wave One Symptoms - Distance")

### Wave One table of symptom counts by impact category 
count.impact= filter(wave.one,impact_cat!="") %>% 
  filter(!is.na(.[,wave1.symptoms[1]])) %>% 
  group_by(impact_cat) %>%
  count(.[,wave1.symptoms[1]]) %>% 
  as.data.frame() %>% 
  mutate(symptom=rnames[1]) %>% 
  dplyr::select(impact_cat,symptom,n)
for(i in 2:length(wave1.symptoms)) {
  v=filter(wave.one,impact_cat!="") %>% 
    filter(!is.na(.[,wave1.symptoms[i]])) %>% 
    group_by(impact_cat) %>%
    count(.[,wave1.symptoms[i]]) %>% 
    as.data.frame() %>% 
    mutate(symptom=rnames[i]) %>% 
    dplyr::select(impact_cat,symptom,n)
  count.impact=rbind(count.impact,v)
}
colnames(count.impact)=c("Impact_Category","Symptom", "Count")
ggplot(data=count.impact, aes(x=Symptom,y=Count,fill=Impact_Category)) +
  geom_bar(stat="identity") + 
  labs(title=" Wave One Symptoms - Impact Category",
       subtitle = "I can change the colors of these graphs")

## Wave Two:

### Wave Two counts of symptoms bar plot or pie chart 

wave2.symptoms=grep("^symptoms_\\d", names(wave.two), value=T)[-13:-14] # has 2 more columns
count2=list()
for(i in wave2.symptoms) {
  v=sum(wave.two[,i]==1, na.rm=T)
  count2=c(count2,v)
}

df_sym2=cbind(wave2.symptoms,count2,rnames) %>% 
  as.data.frame() %>% 
  dplyr::select(rnames,count2)
df_sym2$count2=as.integer(df_sym2$count2)
colnames(df_sym2)=c("Symptom","Count")
# knitr::kable(df_sym2)
b=barplot(df_sym2$Count, names.arg=df_sym2$Symptom, las=2,
          main="Count of Symptoms")
text(b, df_sym2$Count+15, df_sym2$Count, font=2)


### Wave Two table of symptom counts by distance category 

count2.dist=v=wave.two %>% 
  filter(!is.na(.[,wave2.symptoms[1]])) %>% 
  group_by(group) %>%
  count(.[,wave2.symptoms[1]]) %>% 
  as.data.frame() %>% 
  mutate(symptom=rnames[1]) %>% 
  dplyr::select(group,symptom,n)
for(i in 2:length(wave1.symptoms)) {
  v=wave.two %>% 
    filter(!is.na(.[,wave2.symptoms[i]])) %>% 
    group_by(group) %>%
    count(.[,wave2.symptoms[i]]) %>% 
    as.data.frame() %>% 
    mutate(symptom=rnames[i]) %>% 
    dplyr::select(group,symptom,n)
  count2.dist=rbind(count2.dist,v)
}
colnames(count2.dist)=c("Distance","Symptom", "Count")
ggplot(data=count.dist, aes(x=Symptom,y=Count,fill=Distance)) +
  geom_bar(stat="identity") + 
  labs(title="Wave 2 Symptoms")
ggplot(data=count2.dist, aes(x=Distance,y=Count,fill=Symptom)) +
  geom_bar(stat="identity") + 
  labs(title="Wave 2 Symptoms")

### Wave Two table of symptom counts by impact category 

count2.impact= filter(wave.two,impact_cat!="") %>% 
  filter(!is.na(.[,wave2.symptoms[1]])) %>% 
  filter(.[,wave2.symptoms[1]]==1) %>% 
  group_by(impact_cat) %>%
  count(.[,wave2.symptoms[1]]) %>% 
  as.data.frame() %>% 
  mutate(symptom=rnames[1]) %>% 
  dplyr::select(impact_cat,symptom,n)

for(i in 2:length(wave2.symptoms)) {
  v=filter(wave.two,impact_cat!="") %>% 
    filter(!is.na(.[,wave2.symptoms[i]])) %>% 
    filter(.[,wave2.symptoms[i]]==1) %>% 
    group_by(impact_cat) %>%
    count(.[,wave2.symptoms[i]]) %>% 
    as.data.frame() %>% 
    mutate(symptom=rnames[i]) %>% 
    dplyr::select(impact_cat,symptom,n)
  count2.impact=rbind(count2.impact,v)
}
colnames(count2.impact)=c("Impact_Category","Symptom", "Count")
ggplot(data=count2.impact, aes(x=Symptom,y=Count,fill=Impact_Category)) +
  geom_bar(stat="identity") + 
  labs(title="Symptoms - Impact Category",
       subtitle = "I can change the colors of these graphs")

### Wave One to Wave Two comparison – physical symptoms (maybe select to only be the people who responded to both waves) 

compare = left_join(df_sym,df_sym2, by="Symptom")
colnames(compare)=c("Symptom","Wave_One","Wave_Two")
df = melt(compare, id=c("Symptom")) %>% 
ggplot(df) +
  geom_bar(aes(x = Symptom, y = value, fill = variable), 
           stat="identity", position = "dodge", width = 0.7) +
  scale_fill_manual("Result\n", values = c("red","blue"), 
                    labels = c(" Yresult", " Xresult")) +
  labs(x="\nNumber",y="Result\n") +
  theme_bw(base_size = 14)

### Map of Wave One symptoms 
##### Do a separate map for each type of symptom with a color for yes and a different color for no on that symptom 
geo_marshall_sym = geo_marshall %>%
  filter(impact_cat!="") %>% 
  st_as_sf(coords=c("lon","lat"), crs = 4326) %>% 
  mutate(symptoms_1=ifelse(is.na(symptoms_1),0,symptoms_1),
         symptoms_2=ifelse(is.na(symptoms_2),0,symptoms_2),
         symptoms_3=ifelse(is.na(symptoms_3),0,symptoms_3),
         symptoms_4=ifelse(is.na(symptoms_4),0,symptoms_4),
         symptoms_5=ifelse(is.na(symptoms_5),0,symptoms_5),
         symptoms_6=ifelse(is.na(symptoms_6),0,symptoms_6),
         symptoms_7=ifelse(is.na(symptoms_7),0,symptoms_7),
         symptoms_8=ifelse(is.na(symptoms_8),0,symptoms_8),
         symptoms_9=ifelse(is.na(symptoms_9),0,symptoms_9),
         symptoms_10=ifelse(is.na(symptoms_10),0,symptoms_10),
         symptoms_11=ifelse(is.na(symptoms_11),0,symptoms_11),
         symptoms_12=ifelse(is.na(symptoms_12),0,symptoms_12),
         symptoms_13=ifelse(is.na(symptoms_13),0,symptoms_13))
  
test_1=geo_marshall_sym %>% 
  dplyr::select(symptoms_1,geometry) %>% 
  mutate(highlight=ifelse(symptoms_1==1,"green","red"))
test_2=geo_marshall_sym %>% 
  dplyr::select(symptoms_2,geometry) %>% 
  mutate(highlight=ifelse(symptoms_2==1,"green","red"))
test_3=geo_marshall_sym %>% 
  dplyr::select(symptoms_3,geometry) %>% 
  mutate(highlight=ifelse(symptoms_3==1,"green","red"))
test_4=geo_marshall_sym %>% 
  dplyr::select(symptoms_4,geometry) %>% 
  mutate(highlight=ifelse(symptoms_4==1,"green","red"))
test_5=geo_marshall_sym %>% 
  dplyr::select(symptoms_5,geometry) %>% 
  mutate(highlight=ifelse(symptoms_5==1,"green","red"))
test_6=geo_marshall_sym %>% 
  dplyr::select(symptoms_6,geometry) %>% 
  mutate(highlight=ifelse(symptoms_6==1,"green","red"))
test_7=geo_marshall_sym %>% 
  dplyr::select(symptoms_7,geometry) %>% 
  mutate(highlight=ifelse(symptoms_7==1,"green","red"))
test_8=geo_marshall_sym %>% 
  dplyr::select(symptoms_8,geometry) %>% 
  mutate(highlight=ifelse(symptoms_8==1,"green","red"))
test_9=geo_marshall_sym %>% 
  dplyr::select(symptoms_9,geometry) %>% 
  mutate(highlight=ifelse(symptoms_9==1,"green","red"))
test_10=geo_marshall_sym %>% 
  dplyr::select(symptoms_10,geometry) %>% 
  mutate(highlight=ifelse(symptoms_10==1,"green","red"))
test_11=geo_marshall_sym %>% 
  dplyr::select(symptoms_11,geometry) %>% 
  mutate(highlight=ifelse(symptoms_11==1,"green","red"))
test_12=geo_marshall_sym %>% 
  dplyr::select(symptoms_12,geometry) %>% 
  mutate(highlight=ifelse(symptoms_12==1,"green","red"))
test_13=geo_marshall_sym %>% 
  dplyr::select(symptoms_13,geometry) %>% 
  mutate(highlight=ifelse(symptoms_13==1,"green","red"))

test1=ggplot(test_1)  +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(fill=symptoms_1), col=test_1$highlight) +
  #  gghighlight::gghighlight(symptoms_1==1, unhighlighted_colour = "red")  +
  labs(title=NULL, #paste0("Marshall Fire Survey, Wave One: "),
       subtitle=paste0("Symptom - ", rnames[1],"\nNumber of Cases: ",sum(test_1$symptoms_1==1)),
       caption="Need to get legend to display properly")  +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5),
        legend.position = "none")
test2=ggplot(test_2)  +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(fill=symptoms_2), col=test_2$highlight) +
  #  gghighlight::gghighlight(symptoms_2==1, unhighlighted_colour = "red")  +
  labs(title=NULL, #paste0("Marshall Fire Survey, Wave One: "),
       subtitle=paste0("Symptom - ", rnames[1],"\nNumber of Cases: ",sum(test_2$symptoms_2==1)),
       caption="Need to get legend to display properly")  +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5),
        legend.position = "none")
test3=ggplot(test_3)  +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(fill=symptoms_3), col=test_3$highlight) +
  #  gghighlight::gghighlight(symptoms_3==1, unhighlighted_colour = "red")  +
  labs(title=NULL, #paste0("Marshall Fire Survey, Wave One: "),
       subtitle=paste0("Symptom - ", rnames[1],"\nNumber of Cases: ",sum(test_3$symptoms_3==1)),
       caption="Need to get legend to display properly")  +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5),
        legend.position = "none")
test4=ggplot(test_4)  +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(fill=symptoms_4), col=test_4$highlight) +
  #  gghighlight::gghighlight(symptoms_4==1, unhighlighted_colour = "red")  +
  labs(title=NULL, #paste0("Marshall Fire Survey, Wave One: "),
       subtitle=paste0("Symptom - ", rnames[1],"\nNumber of Cases: ",sum(test_4$symptoms_4==1)),
       caption="Need to get legend to display properly")  +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5),
        legend.position = "none")
test5=ggplot(test_5)  +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(fill=symptoms_5), col=test_5$highlight) +
  #  gghighlight::gghighlight(symptoms_5==1, unhighlighted_colour = "red")  +
  labs(title=NULL, #paste0("Marshall Fire Survey, Wave One: "),
       subtitle=paste0("Symptom - ", rnames[1],"\nNumber of Cases: ",sum(test_5$symptoms_5==1)),
       caption="Need to get legend to display properly")  +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5),
        legend.position = "none")
test6=ggplot(test_6)  +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(fill=symptoms_6), col=test_6$highlight) +
  #  gghighlight::gghighlight(symptoms_6==1, unhighlighted_colour = "red")  +
  labs(title=NULL, #paste0("Marshall Fire Survey, Wave One: "),
       subtitle=paste0("Symptom - ", rnames[1],"\nNumber of Cases: ",sum(test_6$symptoms_6==1)),
       caption="Need to get legend to display properly")  +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5),
        legend.position = "none")
test7=ggplot(test_7)  +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(fill=symptoms_7), col=test_7$highlight) +
  #  gghighlight::gghighlight(symptoms_7==1, unhighlighted_colour = "red")  +
  labs(title=NULL, #paste0("Marshall Fire Survey, Wave One: "),
       subtitle=paste0("Symptom - ", rnames[1],"\nNumber of Cases: ",sum(test_7$symptoms_7==1)),
       caption="Need to get legend to display properly")  +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5),
        legend.position = "none")
test8=ggplot(test_8)  +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(fill=symptoms_8), col=test_8$highlight) +
  #  gghighlight::gghighlight(symptoms_8==1, unhighlighted_colour = "red")  +
  labs(title=NULL, #paste0("Marshall Fire Survey, Wave One: "),
       subtitle=paste0("Symptom - ", rnames[1],"\nNumber of Cases: ",sum(test_8$symptoms_8==1)),
       caption="Need to get legend to display properly")  +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5),
        legend.position = "none")
test9=ggplot(test_9)  +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(fill=symptoms_9), col=test_9$highlight) +
  #  gghighlight::gghighlight(symptoms_9==1, unhighlighted_colour = "red")  +
  labs(title=NULL, #paste0("Marshall Fire Survey, Wave One: "),
       subtitle=paste0("Symptom - ", rnames[1],"\nNumber of Cases: ",sum(test_9$symptoms_9==1)),
       caption="Need to get legend to display properly")  +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5),
        legend.position = "none")
test10=ggplot(test_10)  +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(fill=symptoms_10), col=test_10$highlight) +
  #  gghighlight::gghighlight(symptoms_10==1, unhighlighted_colour = "red")  +
  labs(title=NULL, #paste0("Marshall Fire Survey, Wave One: "),
       subtitle=paste0("Symptom - ", rnames[10],"\nNumber of Cases: ",sum(test_10$symptoms_10==1)),
       caption="Need to get legend to display properly")  +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5),
        legend.position = "none")
test11=ggplot(test_11)  +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(fill=symptoms_11), col=test_11$highlight) +
  #  gghighlight::gghighlight(symptoms_11==1, unhighlighted_colour = "red")  +
  labs(title=NULL, #paste0("Marshall Fire Survey, Wave One: "),
       subtitle=paste0("Symptom - ", rnames[11],"\nNumber of Cases: ",sum(test_11$symptoms_11==1)),
       caption="Need to get legend to display properly")  +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5),
        legend.position = "none")
test12=ggplot(test_12)  +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(fill=symptoms_12), col=test_12$highlight) +
  #  gghighlight::gghighlight(symptoms_12==1, unhighlighted_colour = "red")  +
  labs(title=NULL, #paste0("Marshall Fire Survey, Wave One: "),
       subtitle=paste0("Symptom - ", rnames[12],"\nNumber of Cases: ",sum(test_12$symptoms_12==1)),
       caption="Need to get legend to display properly")  +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5),
        legend.position = "none")
test13=ggplot(test_13)  +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(fill=as.factor(symptoms_13)), col=test_13$highlight) +
  #  gghighlight::gghighlight(symptoms_13==1, unhighlighted_colour = "red")  +
  labs(title=NULL, #paste0("Marshall Fire Survey, Wave One: ")
       subtitle=paste0("Symptom - ", rnames[13],"\nNumber of Cases: ",sum(test_13$symptoms_13==1)),
       caption="Need to get legend to display properly")  +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5),
        legend.position = "right",
        legend.title = element_blank())
grid.arrange(test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,ncol=4)
### Map of Wave Two symptoms
##### Do a separate map for each type of symptom with a color for yes and a different color for no on that symptom 

### Predictors of Physical Health Symptoms 
