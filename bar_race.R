library(gganimate)
library(hrbrthemes)
library(readxl)
library(tidyverse)
library(shiny)
library(scales)
library(gapminder)
library(plotly)
library(RColorBrewer)

#setwd("C:/Users/Graham/Documents/R/My Codes/Shiny/Coronavirus App")
setwd("C:/Users/lalisimus/Documents/Cool_Codes")
#Data Import and Cleansing----

#Cases
covid_case_raw=read.csv(file="www/total-cases-covid-19.csv",stringsAsFactors = F)#Why dont we want strings as factors???
covid_case_raw$Date=gsub(",","",covid_case_raw$Date)#Remove the annoying comma
covid_case_raw$Month=regmatches(covid_case_raw$Date, regexpr("[A-z]+", covid_case_raw$Date))#Extract first word
covid_case_raw$Year=as.numeric(regmatches(covid_case_raw$Date, regexpr("(\\d{4})$", covid_case_raw$Date)))#Extract last 4 digits
covid_case_raw$Day=as.numeric(substr(covid_case_raw$Date,start=4,stop=6))#Extract characters between positions [start,stop] and force them into a numeric value
covid_case_raw$Date_formatted=as.Date(ISOdate(covid_case_raw$Year,match(covid_case_raw$Month,month.abb),covid_case_raw$Day))#paste those elements to create a Date format that R understands
covid_case_raw=covid_case_raw[order(covid_case_raw$Entity,covid_case_raw$Date_formatted),]#order the dataframe by Country and Date
covid_case_neat <- covid_case_raw %>%
  rename(country=Entity, code=Code, date=Date_formatted, cases = `Total.confirmed.cases.of.COVID.19..cases.`) %>%
  select(country,code,date,cases) %>%
  filter(!country %in% c("World","Africa","Asia","Europe","South America","North America","Oceania","World excl. China","World excl. China and South Korea","World excl. China, South Korea, Japan and Singapore"))

#Deaths
covid_death_raw=read.csv(file="www/total-deaths-covid-19.csv",stringsAsFactors = F)
covid_death_raw$Date=gsub(",","",covid_death_raw$Date)#Remove the annoying comma
covid_death_raw$Month=regmatches(covid_death_raw$Date, regexpr("[A-z]+", covid_death_raw$Date))#Extract first word
covid_death_raw$Year=as.numeric(regmatches(covid_death_raw$Date, regexpr("(\\d{4})$", covid_death_raw$Date)))#Extract last 4 digits
covid_death_raw$Day=as.numeric(substr(covid_death_raw$Date,start=4,stop=6))#Extract characters between positions [start,stop] and force them into a numeric value
covid_death_raw$Date_formatted=as.Date(ISOdate(covid_death_raw$Year,match(covid_death_raw$Month,month.abb),covid_death_raw$Day))#paste those elements to create a Date format that R understands
covid_death_raw=covid_death_raw[order(covid_death_raw$Entity,covid_death_raw$Date_formatted),]#order the dataframe by Country and Date
covid_death_neat <- covid_death_raw %>%
  rename(country=Entity, code=Code, date=Date_formatted, deaths = `Total.confirmed.deaths.due.to.COVID.19..deaths.`) %>%
  select(country,date,deaths) %>%
  filter(!country %in% c("World","Africa","Asia","Europe","South America","North America","Oceania","World excl. China","World excl. China and South Korea","World excl. China, South Korea, Japan and Singapore"))

#Lockdowns
lockdowns <- read_csv("www/lockdowns.csv") %>%
  rename(lockdown_status=lockdown_0326)

#Country Populations
pop_data <- world_bank_pop %>%
  filter(indicator=="SP.POP.TOTL") %>%
  select(country,'2017') %>%
  rename(code=country,population='2017')

#Data Wrangling----

#Join case, death, lockdown and population datasets together
covid_joined <- covid_case_neat %>%
  left_join(covid_death_neat, by=c("country","date")) %>%
  left_join(lockdowns, by="country") %>%
  left_join(pop_data, by="code") %>%
  select(country, date, cases, deaths, lockdown_status, population) %>%
  mutate(deaths_per_case=deaths/cases) %>%
  mutate(cases_per_100k_pop=cases/(population/100000)) %>%
  mutate(deaths_per_100k_pop=deaths/(population/100000))

#Create a data frame with just the most recent date for which we have death data
covid_joined_latest <- covid_death_neat %>%
  select(country,date) %>%
  filter(date==max(date)) %>%
  left_join(covid_joined, by=c("country","date")) %>%
  arrange(desc(cases))





#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################



library(tidyverse)
library(janitor)

library(tidyverse)
library(gganimate)



cases_formatted <- covid_case_neat %>%
  group_by(date) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-cases),
         Value_rel = cases/cases[rank==1],
         Value_lbl = paste0(" ",cases)) %>%
  group_by(country) %>% 
  filter(rank <=10) %>%
  ungroup()





staticplot = ggplot(cases_formatted, aes(rank, group = country, 
                                       fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = cases/2,
                height = cases,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=cases,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))



anim = staticplot + transition_states(date, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Cases per date : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "Cases Data Source: WHO")



# For GIF
animate(anim, 200, fps = 60,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))



























