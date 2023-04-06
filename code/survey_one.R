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