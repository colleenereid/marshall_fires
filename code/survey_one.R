#Function Junction
tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  knitr::kable(res)
}

waveone<-read.csv("/Volumes/research/Marshall Fire Health/marshall_w1_deID.csv")
wavetwo<-read.csv("/Volumes/research/Marshall Fire Health/marshall_w2_deID.csv")

wave.one<-waveone %>%
#  mutate(mailingcity=recode(mailingcity, 'Superior'='SUPERIOR', 'UNINCORPORATED'='BOULDER')) %>%
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
Respondants = c(nrow(wave.one),nrow(wave.two))
Wave=c("One","Two")
df1 = cbind(Wave,Respondants) %>% 
  as.data.frame()
save_kable(knitr::kable(df1),"images/w1w2_counts.png")
#Percent male and Female -> There is Binary/Decl
save_kable(tblFun(wave.one$Gender3),"images/w1.gender.table.png")
save_kable(tblFun(wave.two$Gender3),"images/w2.gender.table.png")

# Education attainment
save_kable(tblFun(wave.one$education),"images/w1.edu.table.png")
save_kable(tblFun(wave.two$education),"images/w2.edu.table.png")

### Income for each wave 
save_kable(tblFun(wave.one$income),"images/w1.income.table.png")
save_kable(tblFun(wave.two$income),"images/w2.income.table.png")

### Race/ethnicity
save_kable(tblFun(wave.one$RaceEthn2),"images/w1.ethn.table.png")
save_kable(tblFun(wave.two$RaceEthn2),"images/w2.ethn.table.png")

### Owner/Renter 
save_kable(tblFun(wave.one$ownership_status),"images/w1.ownrent.table.png")
save_kable(tblFun(wave.two$renterowner),"images/w2.ownrent.table.png")

### Impact category 
save_kable(tblFun(wave.one$impact_cat),"images/w1.impact.table.png")
save_kable(tblFun(wave.two$impact_cat),"images/w2.impact.table.png")

### Distance category 
save_kable(tblFun(wave.one$group),"images/w1.dist.table.png")
save_kable(tblFun(wave.two$group),"images/w2.dist.table.png")

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
ggsave("images/wave1.neigh.AQ1_2.png",plot=grid.arrange(w1.before.neigh.plot,w1.after.neigh.plot))

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
ggsave("images/wave1.neigh.dist.AQ1_2.png",plot=grid.arrange(w1.before.neigh.dist.plot,w1.after.neigh.dist.plot))

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
ggsave("images/wave1.neigh.impact.AQ1_2.png",plot=grid.arrange(w1.before.neigh.impact.plot,w1.after.neigh.impact.plot))

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
ggsave("images/wave1.home.AQ3_4.png",plot=grid.arrange(w1.before.home.plot,w1.after.home.plot))

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
ggsave("images/wave1.home.dist.AQ3_4.png",plot=grid.arrange(w1.before.home.dist.plot,w1.after.home.dist.plot))

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
ggsave("images/wave1.home.impact.AQ3_4.png",plot=grid.arrange(w1.before.home.impact.plot,w1.after.home.impact.plot))

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
ggsave("images/w1xw2.neigh.AQ2.png",plot=grid.arrange(w1.after.neigh.plot,w2.after.neigh.plot))

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
ggsave("images/w1xw2.neigh.dist.AQ2.png",plot=grid.arrange(w1.after.neigh.dist.plot,w2.after.neigh.dist.plot))

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
ggsave("images/w1xw2.neigh.impact.AQ2.png",plot=grid.arrange(w1.after.neigh.impact.plot,w2.after.neigh.impact.plot))

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
ggsave("images/w1xw2.home.AQ4.png",plot=grid.arrange(w1.after.home.plot,w2.after.home.plot))

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
ggsave("images/w1xw2.home.dist.AQ4.png",plot=grid.arrange(w1.after.home.dist.plot,w2.after.home.dist.plot))

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
ggsave("images/w1xw2.home.impact.AQ4.png",plot=grid.arrange(w1.after.home.impact.plot,w2.after.home.impact.plot))

# Map
mapwave1=read.csv("/Volumes/research/Marshall Fire Health/marshall_w1.csv")
# fixes:
mapwave1[mapwave1$mailingaddr1=="553 GRANT ST",]$mailingaddr1 = "553 GRANT AVE"

#geocoding - assigning lats and longs via Google API - will need key
# ~5 errors
geo_marshall=mapwave1%>% #geocoding only completed surveys and those with lat and long
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
  filter(mailingcity=="BOULDER"|mailingcity=="SUPERIOR"|mailingcity=="LOUISVILLE")%>% 
  mutate(full_address = paste0(.$mailingaddr1,", ",.$mailingcity,", ", .$mailingstate,", ",.$mailingzip)) %>% 
  mutate_geocode(full_address, output="latlona") 

## impact_cat
marshall.sp.impact = as(filter(geo_marshall,!is.na(impact_cat)),"Spatial")
factpal <- colorFactor(c("#d7191c","#fdae61","#ffffbf","#a6d96a","#1a9641"), c("Complete loss", "Damaged, living there","Damaged, not living there","No damage, living there","No damage, not living there"))

marshall_pts_impact_map_w1_leaf = leaflet(marshall.sp.impact) %>% 
  addTiles() %>% 
  # addPolygons(color="red",
  #             opacity=0.5) %>% 
  addCircleMarkers(group=test$impact_cat, color = ~factpal(impact_cat),
                   radius = 1,
                   opacity = 1,
                   lng = marshall.sp.aq1@coords[,1],
                   lat = marshall.sp.aq1@coords[,2])
saveWidget(marshall_pts_impact_map_w1_leaf, file="images/marshall_impact_map_w1.html")

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
ggsave("images/map1.impact.cat.png", height = 7.5, width = 7.5)
test=geo_marshall %>% 
  st_as_sf(coords = c("lon","lat"), crs=st_crs())
## AQ1
marshall.sp.aq1 = as(filter(test,!is.na(air_quality_1)),"Spatial")
factpal <- colorFactor(c("#d7191c","#fdae61","#ffffbf","#a6d96a","#1a9641"), c("1", "2","3","4","5"))
bb=st_bbox(marshall.sp.aq1) %>% 
  st_as_sfc(crs=st_crs(marshall.sp.aq1)) %>% 
  as.data.frame() %>% 
  mutate(var=1) %>% 
  dplyr::select(var,geometry)

bb=as(bb$geometry,"Spatial")

marshall_pts_neigh_aq1_w1_mapleaf=leaflet(bb) %>%
  addTiles() %>%
  addPolygons(color="red",
             opacity=0.5) %>%
  addCircleMarkers(color = ~factpal(marshall.sp.aq1$air_quality_1),
                   radius = 1,
                   opacity = 1,
                   lng = marshall.sp.aq1@coords[,1],
                   lat = marshall.sp.aq1@coords[,2])
# saveWidget(marshall_pts_neigh_aq1_w1_mapleaf, file="images/marshall_aq1_neigh_map_w1.html") #comment labels out

ggplot(filter(geo_marshall,!is.na(air_quality_1))) +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(color=as.factor(air_quality_1))) +
  scale_color_manual(values = c("1" = "#d7191c",
                                "2" = "#fdae61",
                                "3" = "#ffffbf",
                                "4" = "#a6d96a",
                                "5" = "#1a9641")) +
  labs(title="Marshall Fire Survey Results (NA's filtered out) \n Before the Fire",
       subtitle="I was confident the \n air in my neighborhood was safe to breath",
       caption=paste0("Wave One - Total Respondants Displayed: ",
                               nrow(filter(geo_marshall,!is.na(air_quality_1)))
                               )
       ) +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  guides(colour=guide_legend(title=""))
ggsave("images/map1.AQ1.png", height = 7.5, width = 7.5)

ggplot(filter(geo_marshall,!is.na(air_quality_2))) +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(color=as.factor(air_quality_2))) +
  scale_color_manual(values = c("1" = "#d7191c",
                                "2" = "#fdae61",
                                "3" = "#ffffbf",
                                "4" = "#a6d96a",
                                "5" = "#1a9641")) +
  labs(title="Marshall Fire Survey Results (NA's filtered out) \n After the Fire",
       subtitle="I am confident the \n air in my neighborhood was safe to breath",
       caption=paste0("Wave One - Total Respondants Displayed: ",
                      nrow(filter(geo_marshall,!is.na(air_quality_1)))
       )
  ) +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  guides(colour=guide_legend(title=""))
ggsave("images/map1.AQ2.png", height = 7.5, width = 7.5)

ggplot(filter(geo_marshall,!is.na(air_quality_3))) +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(color=as.factor(air_quality_3))) +
  scale_color_manual(values = c("1" = "#d7191c",
                                "2" = "#fdae61",
                                "3" = "#ffffbf",
                                "4" = "#a6d96a",
                                "5" = "#1a9641"))+
  labs(title="Marshall Fire Survey Results (NA's filtered out) \n Before the Fire",
       subtitle="I was confident the \n air inside my home was safe to breath",
       caption=paste0("Wave One - Total Respondants Displayed: ",
                      nrow(filter(geo_marshall,!is.na(air_quality_1)))
       )
  ) +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  guides(colour=guide_legend(title=""))
ggsave("images/map1.AQ3.png", height = 7.5, width = 7.5)

ggplot(filter(geo_marshall,!is.na(air_quality_4))) +
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(color=as.factor(air_quality_4))) +
  scale_color_manual(values = c("1" = "#d7191c",
                                "2" = "#fdae61",
                                "3" = "#ffffbf",
                                "4" = "#a6d96a",
                                "5" = "#1a9641")) +
  labs(title="Marshall Fire Survey Results (NA's filtered out) \n After the Fire",
       subtitle="I am confident the \n air in my neighborhood was safe to breath",
       caption=paste0("Wave One - Total Respondants Displayed: ",
                      nrow(filter(geo_marshall,!is.na(air_quality_1)))
       )
  ) +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  guides(colour=guide_legend(title=""))
ggsave("images/map1.AQ4.png", height = 7.5, width = 7.5)

# Moran's I - Basic
# remotes::install_github('mcooper/moranfast') #citation probably needed
library(moranfast)
marshall.dists = as.matrix(dist(cbind(marshall.sp.aq1@coords[,1], marshall.sp.aq1@coords[,2])))
marshall.dists.inv = 1/marshall.dists
diag(marshall.dists.inv) = 0
moranfast(marshall.sp.aq1$air_quality_1, marshall.sp.aq1@coords[,1],marshall.sp.aq1@coords[,2])

# Moran's I - Veroni
co_co = st_read("../aqi_kriging/co_counties/geo_export_ecf55ab8-2f88-4a1f-b4fb-b21f58fb9432.shp") %>% 
  st_transform(st_crs(marshall.sp))
map2SpatialPolygons(marshall.sp.aq1, IDs=marshall.sp.aq1$surveyid)
# Select only Denver metro counties and geometry
den_co = co_co %>%
  filter(county %in% c("BOULDER")) %>%
  dplyr::select(county, geometry)
den_co = as(den_co, "Spatial")
e <- as(raster::extent(), "SpatialPolygons")

# convert to a spatial object, reproject
den_co = as(den_co, "Spatial")#%>% st_transform(crs = st_crs(prg))
st_crs(marshall.sp)
geo_mar.queen <- poly2nb(marshall_ver, queen=T)
rnames=c("Dry Cough","Wet Cough","Wheeze","Itchy or watery eyes","Sore Throat","Headache","Shortness of Breath","Difficult or labored breathing","Sneezing or stuffy nose","Nausea or vomiting ","Allergic skin reaction","Strange taste in your mouth","Stress")

# Physical Health Symptoms

## Wave One:

### Wave One counts of symptoms bar plot or pie chart 
wave1.symptoms=grep("^symptoms_\\d", names(wave.one), value=T)
count=list()
for(i in wave1.symptoms) {
  v=sum(!is.na(wave.one[,i]))
  count=c(count,v)
}
df=cbind(wave1.symptoms,count,rnames) %>% 
  as.data.frame() %>% 
  dplyr::select(rnames,count)
df$count=as.integer(df$count)
colnames(df)=c("Symptom","Count")
final_df = df %>% 
  mutate(Percent=round(Count/nrow(wave.one)*100,2))
ggplot(final_df, aes(x=Symptom,))+
  geom_col()
save_kable(knitr::kable(final_df), "images/symptoms_count.png")
### Wave One table of symptom counts by distance category 
count=list()
wave.one %>% 
  dplyr::select(.[,i],group) %>% 
  group_by(group) %>% 
  summarise(sum=wave.one[,i]==1)
for(i in wave1.symptoms) {
  v=wave.one %>% 
    group_by(group) %>% 
    sum(!is.na(wave.one[,i]))
  count=c(count,v)
}

### Wave One table of symptom counts by impact category 





## Wave Two:


wave2.symptoms=grep("^symptoms_\\d", names(wave.two), value=T) # has 2 more columns


### Wave Two counts of symptoms bar plot or pie chart 





### Wave Two table of symptom counts by distance category 





### Wave Two table of symptom counts by impact category 





### Wave One to Wave Two comparison – physical symptoms (maybe select to only be the people who responded to both waves) 





### Predictors of Physical Health Symptoms 




