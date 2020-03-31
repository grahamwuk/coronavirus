#Packages
library(readxl)
library(tidyverse)
library(shiny)
library(scales)

#Data Preparation
#setwd("C:/Users/Graham/Documents/R/My Codes/Shiny/Coronavirus App")
covid_case_raw <- read_csv("www/total-cases-covid-19.csv")
covid_case_neat <- covid_case_raw %>%
  filter(Entity != "World" & Year>=-5) %>%
  rename(country=Entity, code=Code, day=Year, cases = `Total confirmed cases of COVID-19 (cases)`) %>%
  mutate(date_base=as.Date("2020-01-21")) %>%
  mutate(date=date_base+day) %>%
  select(country, date, cases, days_since_case100)

covid_death_raw <- read_csv("www/total-deaths-covid-19.csv")
covid_death_neat <- covid_death_raw %>%
  filter(Entity != "World" & Year>=-5) %>%
  rename(country=Entity, code=Code, day=Year, deaths = `Total confirmed deaths due to COVID-19 (deaths)`) %>%
  mutate(date_base=as.Date("2020-01-21")) %>%
  mutate(date=date_base+day) %>%
  select(country, date, deaths, days_since_death10)

lockdowns <- read_csv("www/lockdowns.csv") %>%
  rename(lockdown_status=lockdown_0326)

covid_joined <- covid_case_neat %>%
  left_join(covid_death_neat, by=c("country","date")) %>%
  left_join(lockdowns, by="country") %>%
  select(country, date, cases, deaths, days_since_case100, days_since_death10, lockdown_status) %>%
  mutate(deaths_per_case=deaths/cases)

latest_obs <- covid_death_neat %>%
  select(country, date) %>%
  filter(date==max(date))
latest_data <- latest_obs %>%
  left_join(covid_joined, by=c("country","date")) %>%
  select(country, date, cases, deaths, days_since_case100, days_since_death10) %>%
  mutate(deaths_per_case=deaths/cases) %>%
  arrange(desc(cases))
#View(latest_data)

#Currently Unused Data Prep - trying to join China data onto other countries to enable a forecast based on similar paths
covid_china <- covid_joined %>%
  filter(country=="China") %>%
  select(days_since_case100, cases, deaths) %>%
  mutate(cases_china = cases, deaths_china = deaths) %>%
  select(days_since_case100,cases_china, deaths_china)
covid_calc <- covid_joined %>%
  inner_join(covid_china, by="days_since_case100")

#User Interface
ui <- fluidPage(
  titlePanel("COVID-19 Data Explorer"),
  fluidRow(
    column(4,
           sliderInput(inputId="num",label="Select How Many Countries To Include",value=10, min=1, max=200),
    ),
    column(4,
           selectInput(inputId="ColorBy", 
                       label="Colour By:", 
                       choices = c("country", "lockdown_status")
           )
    ),
  ),
  tabsetPanel(
              tabPanel(title="Cases",
                       mainPanel(plotOutput("cases_date")),
                       mainPanel(plotOutput("cases_align")),
                       mainPanel(plotOutput("cases_bar"))),
              tabPanel(title="Deaths",
                       mainPanel(plotOutput("deaths_date")),
                       mainPanel(plotOutput("deaths_align")),
                       mainPanel(plotOutput("deaths_bar"))),
              tabPanel(title="Deaths v Cases",
                       mainPanel(plotOutput("dpc_scatter")),
                       mainPanel(plotOutput("dpc_bar"))),
              tabPanel(title="DataTable",
                       "Latest Data: ", max(covid_joined$date),
                       DT::dataTableOutput("tableFiles")
                       )
              )
)

#Server Function
server <- function(input, output) {

  selected_countries <- reactive({
    g <- latest_data %>%
      top_n(input$num, wt=cases) %>%
      select(country) %>%
      left_join(covid_joined, by="country") %>%
      arrange(desc(cases))
    })
  selected_countries_latest <- reactive({
    h <- latest_data %>%
      top_n(input$num, wt=cases) %>%
      arrange(desc(cases))
  })
  
  output$cases_date <- renderPlot({
    ggplot(data = selected_countries(), 
         mapping = aes(x = date, y = cases, color=eval(as.name(input$ColorBy)))) +
    geom_line() +
      scale_y_log10(labels=comma)
  })
  output$cases_align <- renderPlot({
    ggplot(data = selected_countries(), 
         mapping = aes(x = days_since_case100, y = cases, color=country)) +
    geom_line() +
      scale_y_log10(labels=comma)
  })
  output$deaths_date <- renderPlot({
    ggplot(data = selected_countries(), 
           mapping = aes(x = date, y = deaths, color=country)) +
      geom_line() +
      scale_y_log10(labels=comma)
  })
  output$deaths_align <- renderPlot({
    ggplot(data = selected_countries(), 
           mapping = aes(x = days_since_death10, y = deaths, color=country)) +
      geom_line() +
      scale_y_log10(labels=comma)
  })
  output$dpc_scatter <- renderPlot({
    ggplot(data = latest_data, 
           mapping = aes(x = cases, y = deaths, color=1/deaths_per_case)) +
      geom_point() +
      scale_x_log10(labels=comma) +
      scale_y_log10(labels=comma)
  })
  output$cases_bar <- renderPlot({
    ggplot(data = selected_countries_latest(), 
           mapping = aes(x = country, y = cases)) +
      geom_col()
  })
  output$deaths_bar <- renderPlot({
    ggplot(data = selected_countries_latest(), 
           mapping = aes(x = country, y = deaths)) +
      geom_col()
  })
  output$dpc_bar <- renderPlot({
    ggplot(data = selected_countries_latest(), 
           mapping = aes(x = country, y = deaths_per_case)) +
      geom_col()
  })
  output$tableFiles <- DT::renderDataTable(latest_data)
}
#Run App
shinyApp(ui = ui, server = server)