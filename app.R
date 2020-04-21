#Packages----
library(readxl)
library(tidyverse)
library(shiny)
library(scales)
library(gapminder)
library(plotly)
library(RColorBrewer)

#Working Directory
#setwd("C:/Users/Graham/Documents/R/My Codes/Shiny/Coronavirus App")
#setwd("C:/Users/lalisimus/Documents/Cool_Codes")

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
  filter(!country %in% c("World","Africa","Asia","Asia excl. China","Europe","European Union","South America","North America","Oceania","World excl. China","World excl. China and South Korea","World excl. China, South Korea, Japan and Singapore","High income","Low income","Upper middle income","Lower middle income"))
country_list <- covid_case_neat %>%
  select(country) %>%
  distinct()

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
  filter(!country %in% c("World","Africa","Asia","Asia excl. China","Europe","European Union","South America","North America","Oceania","World excl. China","World excl. China and South Korea","World excl. China, South Korea, Japan and Singapore","High income","Low income","Upper middle income","Lower middle income"))

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

#Defining Functions----

#Define functions for days_since_XXX_threshold
dias_cases<-function(n,data){
  data$Flag_Threshold=ifelse(data$cases>=n,1,0)
  z=c()
  zz=sapply(unique(data$country),FUN=function(x){return(as.numeric(data$date[data$country==x]-data$date[max(which(data$country==x&data$Flag_Threshold==0))]) )})
  for(i in unique(data$country)){
    z2=as.numeric(zz[[i]])
    z2[z2<0]=0
    z=c(z,z2)
  }
  return(z)
}
dias_deaths<-function(n,data){
  data$Flag_Threshold=ifelse(data$deaths>=n,1,0)
  z=c()
  zz=sapply(unique(data$country),FUN=function(x){return(as.numeric(data$date[data$country==x]-data$date[max(which(data$country==x&data$Flag_Threshold==0))]) )})
  for(i in unique(data$country)){
    z2=as.numeric(zz[[i]])
    z2[z2<0]=0
    z=c(z,z2)
  }
  return(z)
}

#User Interface----
ui <- fluidPage(
  titlePanel("COVID-19 Data Explorer"),
  wellPanel(h4("Input Options"),
            fluidRow(column(4,sliderInput(inputId="num", label="Select How Many Countries To Include",value=10, min=1, max=200))),
            fluidRow(column(4,sliderInput(inputId="case_threshold",label="Select Case Threshold for Date Alignment",value=100, min=1, max=1000))),
            fluidRow(column(4,sliderInput(inputId="death_threshold",label="Select Death Threshold for Date Alignment",value=5, min=1, max=100))),
            #fluidRow(column(2,selectInput(inputId="ColorBy", label="Colour By:", choices = c("country", "lockdown_status"))),
            #fluidRow(column(2,checkboxInput(inputId="PerCap", label="Figures per population"))
  ),
  tabsetPanel(
    tabPanel(title="Cases",
             mainPanel(h4("Total Cases By Country and Date"),
                       plotlyOutput("cases_date"),
                       br(),
                       h4("Total Cases By Country and Date - Coloured by Lockdown Status"),                       
                       plotlyOutput("cases_lockdown"),
                       br(),
                       h4("Total Cases By Country - Aligned"),
                       plotlyOutput("cases_align"),
                       br(),
                       h4("Total Cases per 100k population By Country - Aligned"),                       
                       plotlyOutput("cases_align_PerCap"),
                       br(),
                       h4("Total Cases By Country - Latest Figures"), 
                       plotlyOutput("cases_bar")
             )
    ),
    tabPanel(title="Deaths",
             mainPanel(h4("Total Deaths By Country and Date"),
                       plotlyOutput("deaths_date"),
                       br(),
                       h4("Total Deaths By Country and Date - Coloured by Lockdown Status"),
                       plotlyOutput("deaths_lockdown"),
                       br(),
                       h4("Total Deaths By Country - Aligned"),                       
                       plotlyOutput("deaths_align"),
                       br(),
                       h4("Total Deaths per 100k population By Country - Aligned"),                       
                       plotlyOutput("deaths_align_PerCap"),
                       br(),
                       h4("Total Deaths By Country - Latest Figures"),                       
                       plotlyOutput("deaths_bar")
             )
    ),
    tabPanel(title="Deaths per Case",
             mainPanel(h4("Deaths v Cases Scatter"),
                       plotlyOutput("dpc_scatter"),
                       br(),
                       h4("Deaths per Case By Country and Date"),
                       plotlyOutput("dpc_date"),
                       br(),
                       h4("Deaths per Case By Country - Aligned"),
                       plotlyOutput("dpc_align"),
                       br(),
                       h4("Deaths per Case By Country - Latest Figures"),                        
                       plotlyOutput("dpc_bar")
             )
    ),
    tabPanel(title="DataTable",
             h3("Latest Data: ", max(covid_joined$date)),
             br(),
             DT::dataTableOutput("tableFiles")
    )
  )
)

#Server Function----
server <- function(input, output) {
  #Run the dias functions on cases and deaths datasets, using selected input thresholds
  covid_calc <- reactive({
    a <- covid_joined %>%
      mutate(days_since_case_threshold=dias_cases(input$case_threshold,covid_joined)) %>%
      mutate(days_since_death_threshold=dias_deaths(input$death_threshold,covid_joined))
  })
  
  #Create filtered versions of both datasets only containing the countries of interest
  selected_countries <- reactive({
    g <- covid_joined_latest %>%
      top_n(input$num, wt=cases) %>%
      select(country) %>%
      left_join(covid_calc(), by="country") %>%
      arrange(desc(cases))
  })
  selected_countries_latest <- reactive({
    h <- covid_joined_latest %>%
      top_n(input$num, wt=cases) %>%
      arrange(desc(cases))
  })
  
  #Plots on Cases tab ----
  output$cases_date <- renderPlotly({
    datos<-selected_countries()
    datos<-datos[order(datos$country,datos$date),]
    p<-plot_ly(datos,x=~date,y=~cases,name=~country,type="scatter",mode="lines")%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"))
    p
  })
  output$cases_lockdown <- renderPlotly({
    datos<-selected_countries()
    datos<-datos[order(datos$country,datos$date),]
    p<-plot_ly(datos,x=~date,y=~cases,name=~country,type="scatter",mode="lines",color=~lockdown_status)%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"), legend=~lockdown_status)
    p
  })
  output$cases_align <- renderPlotly({
    datos<-selected_countries()
    datos<-datos[datos$cases>=as.numeric(input$case_threshold),]
    datos<-datos[order(datos$country,datos$days_since_case_threshold),]
    p<-plot_ly(datos,x=~days_since_case_threshold,y=~cases,name=~country,type="scatter",mode="lines")%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"))
    p
  })
  output$cases_align_PerCap <- renderPlotly({
    datos<-selected_countries()
    datos<-datos[datos$cases>=as.numeric(input$case_threshold),]
    datos<-datos[order(datos$country,datos$days_since_case_threshold),]
    p<-plot_ly(datos,x=~days_since_case_threshold,y=~cases_per_100k_pop,name=~country,type="scatter",mode="lines")%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"))
    p
  })
  output$cases_bar <- renderPlotly({
    datos<-selected_countries_latest()
    datos$country <- factor(datos$country, levels = unique(datos$country)[order(datos$cases, decreasing = TRUE)])
    datos<-datos[order(datos$country),]
    p<-plot_ly(datos,x=~country,y=~cases,type="bar")%>%
      config(displayModeBar=T) %>%
      layout(
        xaxis = list(title = "",
                     categoryorder = "array",
                     categoryarray = ~cases))
    p
  })
  
  #Plots on Deaths tab----
  output$deaths_date <- renderPlotly({
    datos<-selected_countries()
    datos<-datos[order(datos$country,datos$date),]
    p<-plot_ly(datos,x=~date,y=~deaths,name=~country,type="scatter",mode="lines")%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"))
    p
  })
  output$deaths_lockdown <- renderPlotly({
    datos<-selected_countries()
    datos<-datos[order(datos$country,datos$date),]
    p<-plot_ly(datos,x=~date,y=~deaths,name=~country,type="scatter",mode="lines",color=~lockdown_status)%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"), legend=~lockdown_status)
    p
  })
  output$deaths_align <- renderPlotly({
    datos<-selected_countries()
    datos<-datos[order(datos$country,datos$days_since_death_threshold),]
    p<-plot_ly(datos,x=~days_since_death_threshold,y=~deaths,name=~country,type="scatter",mode="lines")%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"))
    p
  })
  output$deaths_align_PerCap <- renderPlotly({
    datos<-selected_countries()
    datos<-datos[order(datos$country,datos$days_since_death_threshold),]
    p<-plot_ly(datos,x=~days_since_death_threshold,y=~deaths_per_100k_pop,name=~country,type="scatter",mode="lines")%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"))
    p
  })
  output$deaths_bar <- renderPlotly({
    datos<-selected_countries_latest()
    datos$country <- factor(datos$country, levels = unique(datos$country)[order(datos$deaths, decreasing = TRUE)])
    datos<-datos[order(datos$country),]
    p<-plot_ly(datos,x=~country,y=~deaths,type="bar")%>%
      config(displayModeBar=T) %>%
      layout(
        xaxis = list(title = "",
                     categoryorder = "array",
                     categoryarray = ~deaths))
    p
  })
  
  #Plots on Deaths per Case tab----
  output$dpc_scatter <- renderPlotly({
    datos<-covid_joined_latest
    datos<-datos[datos$deaths_per_case>0,]
    datos<-datos[order(datos$deaths),]
    p<-plot_ly(datos,x=~cases,y=~deaths,type="scatter",mode="markers",color=~1/deaths_per_case,text=~country
    )%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"),xaxis=list(type="log"))
    p
  })
  output$dpc_date <- renderPlotly({
    datos<-selected_countries()
    datos<-datos[order(datos$country,datos$date),]
    p<-plot_ly(datos,x=~date,y=~deaths/cases,name=~country,type="scatter",mode="lines")%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"))
    p
  })
  output$dpc_align <- renderPlotly({
    datos<-selected_countries()
    datos<-datos[order(datos$country,datos$days_since_death_threshold),]
    p<-plot_ly(datos,x=~days_since_case_threshold,y=~deaths/cases,name=~country,type="scatter",mode="lines")%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"))
    p
  })
  output$dpc_bar <- renderPlotly({
    datos<-selected_countries_latest()
    datos$country <- factor(datos$country, levels = unique(datos$country)[order(datos$deaths_per_case, decreasing = TRUE)])
    datos<-datos[order(datos$country),]
    p<-plot_ly(datos,x=~country,y=~deaths_per_case,type="bar")%>%
      config(displayModeBar=T) %>%
      layout(
        xaxis = list(title = "",
                     categoryorder = "array",
                     categoryarray = ~deaths_per_case))
    p
  })
  
  #Table on Data Table tab----
  output$tableFiles <- DT::renderDataTable({
    DT::datatable(covid_joined_latest, options = list(lengthMenu = c(5, 30, 50), pageLength = 50))
  })
}
#Run App----
shinyApp(ui = ui, server = server)

#To Do List----

#Coding Tasks
#1 - Calculate new cases and new deaths from this data (no need for separate source), and plot them on a new tab
#2 - Automate data sourcing by using web scraping instead of manual download
#3 - Allow more selection options for countries, e.g. Europe-only, exclude low pop countries, only rich countries, etc
#4 - Make input$ColorBy work to select either the colour or lockdown chart, so only one is displayed at once
#5 - Fix legend of lockdown chart to show the three statuses: locked down, partial, none
#6 - Set specific colors for the lockdown chart: red for locked down, yellow for partial, green for none
#7 - Make input$PerCap work to select either the raw case/death figures or the per 100k population figures,  so only one is displayed at once
#8 - Fix color scale of Scatterplot so there is more variety and it goes from red (high) to green (low)

#Data Sourcing Tasks
#1 - Add more country demographic data, e.g. continent, population density, GDP per capita, etc
#2 - Add details and key dates of shutdowns in each country, e.g. school closures, flight restrictions
#3 - Add data on testing thoroughness
#4 - Add data on healthcare system types
#5 - Consider definition inconsistencies between countries, e.g. death FROM covid19 vs death WITH covid19
#6 - Add data on active or recovered cases