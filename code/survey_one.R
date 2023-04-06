#Function Junction
tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  res
}

# waveone<-read.csv("/Volumes/research/Marshall Fire Health/marshall_w1_deID.csv")
# wavetwo<-read.csv("/Volumes/research/Marshall Fire Health/marshall_w2_deID.csv")
waveone<-read.csv("../data/marshall_w1_deID.csv")
wavetwo<-read.csv("../data/marshall_w2_deID.csv")
wave.one<-waveone %>%
  mutate(mailingcity=recode(mailingcity, 'Superior'='SUPERIOR', 'UNINCORPORATED'='BOULDER')) %>%
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
  filter(mailingcity=="BOULDER"|mailingcity=="SUPERIOR"|mailingcity=="LOUISVILLE") #filtering only respondents in Boulder Unincorporated, Superior or Louisville

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

#Number of respondants
nrow(waveone)
nrow(wavetwo)

#Number of respondants who completed the survey
wave.one %>% 
  summarise(Wave_One=n())
wave.two %>% 
  summarise(Wave_Two=n())

#Percent male and Female -> There is Binary/Decl
tblFun(wave.one$Gender3)
tblFun(wave.two$Gender3)

# Education attainment
tblFun(wave.one$education)
tblFun(wave.two$education)

### Income for each wave 
tblFun(wave.one$income)
tblFun(wave.two$income)

### Race/ethnicity
tblFun(wave.one$RaceEthn2)
tblFun(wave.two$RaceEthn2)

### Owner/Renter 
tblFun(wave.one$ownership_status)
tblFun(wave.two$renterowner)

### Impact category 
tblFun(wave.one$impact_cat)
tblFun(wave.two$impact_cat)

### Distance category 
tblFun(wave.one$group)
tblFun(wave.two$group)

# Air Quality Perceptions

## Wave One:

### Wave One only: compare before and after the fire – perception of air quality in their neighborhood 
#trying a vertical graph
w1.before.neigh.plot=ggplot(filter(wave.one,!is.na(air_quality_1)), aes(fill=air_quality_1,x="Before the Fire")) +
  geom_bar(position="stack") +
  labs(title = "I am confident that the air in my neighborhood is safe to breathe") +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))

w1.after.neigh.plot=ggplot(filter(wave.one,!is.na(air_quality_3)), aes(fill=air_quality_3,x="After the Fire")) +
  geom_bar(position="stack") +
  xlab(NULL) +
  ylab("Count") +
  scale_fill_manual(values = ryg.palette) +
  theme(plot.title = element_text(hjust = 0.5),   
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL))

grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot)
ggsave("images/wave1.ba.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))
### Wave One only: compare before and after the fire – perception of air quality in their neighborhood by distance 

ggplot(filter(wave.one,!is.na(air_quality_1)), aes(fill=air_quality_1, x=group)) +
  geom_bar(position="stack") +
  labs(title = "Before the Marshall Fire, I was confident that the air in my neighborhood was \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
ggsave("images/wave1.ba.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))
ggplot(filter(wave.one,!is.na(air_quality_3)), aes(fill=air_quality_3, x=group)) +
  geom_bar(position="stack") +
  labs(title = "Currently, I am confident that the air in my neighborhood is \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
ggsave("images/wave1.ba.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))
### Wave One only: compare before and after the fire – perception of air quality in their neighborhood by impact category 

ggplot(filter(wave.one,!is.na(air_quality_1)), aes(fill=air_quality_1, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = "Before the Marshall Fire, I was confident that the air in my neighborhood was \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
ggsave("images/wave1.ba.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))
ggplot(filter(wave.one,!is.na(air_quality_3)), aes(fill=air_quality_3, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = "Currently, I am confident that the air in my neighborhood is \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
ggsave("images/wave1.ba.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))
### Wave One only: compare before and after the fire – perception of air quality in their home 

#trying a horizontal graph
w1.before.home.plot=ggplot(filter(wave.one,!is.na(air_quality_2)), aes(fill=air_quality_2,y="Before the Fire")) +
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
ggsave("images/wave1.ba.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))

### Wave One only: compare before and after the fire – perception of air quality in their home by distance 

ggplot(filter(wave.one,!is.na(air_quality_2)), aes(fill=air_quality_2, x=group)) +
  geom_bar(position="stack") +
  labs(title = "Before the Marshall Fire, I was confident that the air in my neighborhood was \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
ggsave("images/wave1.ba.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))
ggplot(filter(wave.one,!is.na(air_quality_4)), aes(fill=air_quality_4, x=group)) +
  geom_bar(position="stack") +
  labs(title = "Currently, I am confident that the air in my neighborhood is \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
ggsave("images/wave1.ba.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))

### Wave One only: compare before and after the fire – perception of air quality in their home by impact category

ggplot(filter(wave.one,!is.na(air_quality_2)), aes(fill=air_quality_2, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = "Before the Marshall Fire, I was confident that the air in my neighborhood was \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
ggsave("images/wave1.ba.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))
ggplot(filter(wave.one,!is.na(air_quality_4)), aes(fill=air_quality_4, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = "Currently, I am confident that the air in my neighborhood is \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
ggsave("images/wave1.ba.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))

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
ggsave("images/wave1.ba.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))

### Wave One to Wave Two comparison – perception of air quality in neighborhood (AQ2) by distance 

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
ggsave("images/wave1.ba.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))

### Wave One to Wave Two comparison – perception of air quality in neighborhood (AQ2) by impact category 

ggplot(filter(wave.one,!is.na(air_quality_2)), aes(fill=air_quality_2, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = "Before the Marshall Fire, I was confident that the air in my neighborhood was \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
ggsave("images/wave1.ba.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))
ggplot(filter(wave.two,!is.na(air_quality_2)), aes(fill=air_quality_2, x=impact_cat)) +
  geom_bar(position="stack") +
  labs(title = "Before the Marshall Fire, I was confident that the air in my neighborhood was \n safe to breathe",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
ggsave("images/w1xw2.after.neigh.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))

### Wave One to Wave Two comparison – perception of air quality in home (AQ4) 

w1.after.home.plot=ggplot(filter(wave.one,!is.na(air_quality_4)), aes(fill=air_quality_4,y="Wave One")) +
  geom_bar(position="stack") +
  labs(title = "I am confident that the air in my neighborhood is safe to breathe") +
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

grid.arrange(w1.after.neigh.plot,w2.after.neigh.plot)
ggsave("images/w1xw2.after.neigh.AQ4.png",plot=grid.arrange(w1.after.neigh.plot,w2.after.neigh.plot))

### Wave One to Wave Two comparison – perception of air quality in home (AQ4) by distance 

ggplot(filter(wave.one,!is.na(air_quality_4)), aes(fill=air_quality_4, x=group)) +
  geom_bar(position="stack") +
  labs(title = "I am confident that the air inside my home was \n safe to breathe",
       subtitle = "Wave One",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
ggsave("images/w1.home.AQ2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))

ggplot(filter(wave.two,!is.na(air_quality_4)), aes(fill=air_quality_4, x=group)) +
  geom_bar(position="stack") +
  labs(title = "I am confident that the air inside my home was \n safe to breathe",
       subtitle = "Wave Two",
       x = "Distance from fire perimeter", fill ="") +
  scale_fill_manual(values = ryg.palette)
ggsave("images/w2.home.AQ4.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))

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
