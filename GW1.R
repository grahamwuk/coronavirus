#Packages----
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
  fluidRow(
    column(4,
           sliderInput(inputId="num",label="Select How Many Countries To Include",value=10, min=1, max=200)
    ),
    column(4,
           selectInput(inputId="ColorBy", 
                       label="Colour By:", 
                       choices = c("country", "lockdown_status")
           )
    ),
    column(4,
           checkboxInput(inputId="PerCap", 
                         label="Make figures per 100k population" 
           )
    )
  ),
  tabsetPanel(
    tabPanel(title="Cases",
             mainPanel(plotlyOutput("cases_date")),
             sliderInput(inputId="case_threshold",label="Select Case Threshold for Alignment",value=100, min=1, max=1000),
             mainPanel(plotlyOutput("cases_align")),
             mainPanel(plotlyOutput("cases_bar"))),
    tabPanel(title="Deaths",
             mainPanel(plotlyOutput("deaths_date")),
             sliderInput(inputId="death_threshold",label="Select Death Threshold for Alignment",value=1, min=1, max=100),
             mainPanel(plotlyOutput("deaths_align")),
             mainPanel(plotlyOutput("deaths_bar"))),
    tabPanel(title="Deaths v Cases",
             mainPanel(plotlyOutput("dpc_scatter")),
             mainPanel(plotlyOutput("dpc_bar"))),
    tabPanel(title="DataTable",
             "Latest Data: ", max(covid_joined$date),
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
  #Define the yaxis column depending on the selected input
  # selected_yaxis <- reactive({
  #   m <- covid_calc()
  #   if (input$PerCap = TRUE) {m <- m %>% mutate(case_yaxis=cases_per_100kpop)}
  #   if (input$PerCap = TRUE) {m <- m %>% mutate(case_yaxis=deaths_per_100kpop)}
  #   if (input$PerCap = FALSE) {m <- m %>% mutate(case_yaxis=cases)}
  #   if (input$PerCap = FALSE) {m <- m %>% mutate(case_yaxis=deaths)}
  # })
  
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
  #Create all plots using ggplot
  
  output$cases_date <- renderPlotly({
    datos<-selected_countries()
    datos<-datos[order(datos$country,datos$date),]
    p<-plot_ly(datos,x=~date,y=~cases,name=~country,type="scatter",mode="lines")%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"))
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
  output$deaths_date <- renderPlotly({
    #   ggplot(data = selected_countries(), 
    #          mapping = aes(x = date, y = deaths, color=country)) +
    #     geom_line() +
    #     scale_y_log10(labels=comma)
    datos<-selected_countries()
    datos<-datos[order(datos$country,datos$date),]
    p<-plot_ly(datos,x=~date,y=~deaths,name=~country,type="scatter",mode="lines")%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"))
    p
  })
  output$deaths_align <- renderPlotly({
    # ggplot(data = selected_countries(), 
    #        mapping = aes(x = days_since_death_threshold, y = deaths, color=country)) +
    #   geom_line() +
    #   scale_y_log10(labels=comma,limits=c(input$death_threshold,NA))
    datos<-selected_countries()
    datos<-datos[order(datos$country,datos$days_since_death_threshold),]
    p<-plot_ly(datos,x=~days_since_death_threshold,y=~deaths,name=~country,type="scatter",mode="lines")%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"))
    p
  })
  output$dpc_scatter <- renderPlotly({
    # ggplot(data = covid_joined_latest,
    #        mapping = aes(x = cases, y = deaths, color=1/deaths_per_case)) +
    #   geom_point() +
    #   scale_x_log10(labels=comma) +
    #   scale_y_log10(labels=comma)
    datos<-covid_joined_latest
    datos<-datos[datos$deaths_per_case>0,]
    datos<-datos[order(datos$deaths),]
    p<-plot_ly(datos,x=~cases,y=~deaths,type="scatter",mode="markers",color=~1/deaths_per_case
    )%>%
      config(displayModeBar=T)%>%
      layout(yaxis=list(type="log"),xaxis=list(type="log"))
    p
  })
  output$cases_bar <- renderPlotly({
    # ggplot(data = selected_countries_latest(), 
    #        mapping = aes(x = country, y = cases)) +
    #   geom_col()
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
  output$deaths_bar <- renderPlotly({
    # ggplot(data = selected_countries_latest(), 
    #        mapping = aes(x = country, y = deaths)) +
    #   geom_col()
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
  output$dpc_bar <- renderPlotly({
    # ggplot(data = selected_countries_latest(), 
    #        mapping = aes(x = country, y = deaths_per_case)) +
    #   geom_col()
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
  output$tableFiles <- DT::renderDataTable(covid_joined_latest)
}
#Run App----
shinyApp(ui = ui, server = server)
