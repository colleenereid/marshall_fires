wave_one<-read.csv("/Volumes/research/Marshall Fire Health/marshall_w1_deID.csv")
wave_two<-read.csv("/Volumes/research/Marshall Fire Health/marshall_w2_deID.csv")

#Number of respondants
nrow(wave_one)
nrow(wave_two)

#Number of respondants who completed the survey
wave_one %>% 
  filter(finished==1) %>% 
  summarise(Wave_One=n())
wave_two %>% 
  summarise(Wave_Two=n())

#Percent male and Female -> There is Binary/Decl
wave_one %>% 
  filter(finished==1) %>% 
  summarise(Male_w1=sum(Gender3=="Male"),
            Female_w1=sum(Gender3=="Female"),
            Percent_Male_w1=Male_w1/n()*100,
            Percent_Female_w1=Female_w1/n()*100)

wave_two  %>% 
  summarise(Male_w2=sum(Gender3=="Male"),
            Female_w2=sum(Gender3=="Female"),
            Percent_Male_w2=Male_w2/n()*100,
            Percent_Female_w2=Female_w2/n()*100) 

# Education attainment
# 1=HS
# 2=GED or Equiv
# 3=Some college
# 4=AA
# 5=BA
# 6=MA
# 7=PHD
wave_one %>% 
  filter(finished==1)  %>%
  mutate(education=ifelse(is.na(education),0,education)) %>% 
  summarise(na=sum(education==1)/n()*100,
            hs=sum(education==1)/n()*100,
            ged=sum(education==2)/n()*100,
            SCollege=sum(education==3)/n()*100,
            AA=sum(education==4)/n()*100,
            BA=sum(education==5)/n()*100,
            MA=sum(education==6)/n()*100,
            PHD=sum(education==7)/n()*100)

wave_two %>%
  mutate(education=ifelse(is.na(education),0,education)) %>% 
  summarise(na=sum(education==1)/n()*100,
            hs=sum(education==1)/n()*100,
            ged=sum(education==2)/n()*100,
            SCollege=sum(education==3)/n()*100,
            AA=sum(education==4)/n()*100,
            BA=sum(education==5)/n()*100,
            MA=sum(education==6)/n()*100,
            PHD=sum(education==7)/n()*100)

# Income 

# Race/ethnicity

# Owner/Renter 

# Impact category 

# Distance category

# Map
mapwave1=read.csv("data/marshall_w1.csv")
# fixes:
mapwave1[mapwave1$mailingaddr1=="553 GRANT ST",]$mailingaddr1 = "553 GRANT AVE"

#geocoding - assigning lats and longs via Google API - will need key
# ~5 errors
geo_marshall = mapwave1 %>% 
  mutate(full_address = paste0(.$mailingaddr1,", ",.$mailingcity,", ", .$mailingstate,", ",.$mailingzip)) %>% 
  mutate_geocode(full_address, output="latlona") %>%
  filter(!is.na(lat)) %>% 
  filter(impact_cat != "") %>% 
  st_as_sf(coords = c("lon", "lat"),  crs = 4326)

fire_destruction_AQ_and_area_plot = leaflet(geo_marshall) %>% 
  addTiles() %>% 
  # addPolygons(color="red",
  #             opacity=0.5) %>% 
  addCircleMarkers(color = c("Complete loss" = "#d7191c", 
                              "Damaged, living there" = "#fdae61",
                              "Damaged, not living there" = "#ffffbf",
                              "No damage, living there" = "#a6d96a",
                              "No damage, not living there" = "#1a9641"),
                   radius = 3.5,
                   opacity = 1,
                   lng = geo_marshall$long,
                   lat = geo_marshall$lat)

ggplot(geo_marshall) + 
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(color=impact_cat)) +
  scale_color_manual(values = c("Complete loss" = "#d7191c", 
                                "Damaged, living there" = "#fdae61",
                                "Damaged, not living there" = "#ffffbf",
                                "No damage, living there" = "#a6d96a",
                                "No damage, not living there" = "#1a9641")) +
  ggtitle("Marshall Fire Survey Results", subtitle=paste0("Total Respondants Displayed: ",nrow(geo_marshall))) + 
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  guides(colour=guide_legend(title="Impact Level"))
