#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(shiny)
library(DT)
library(tidyverse)
library(shinythemes)

pokemon <- read_csv("pokemon.csv")
pokemon <- pokemon %>% mutate(catch_rate_pct = round((catch_rate/255)*100)) %>% select(-catch_rate)

navbarPage(theme = shinytheme("sandstone"),
  "My Application",
  tabPanel("About This App", 
           mainPanel(
             h3("Purpose"),
             br(),
             p("The purpose of this app is to explore the world of pokemon and understand diffeerent relationships between typing, catch rate, and overall power. Is there a clear-cut pokemon type that is strong or hard to catch throughout each generation? Which typing stands out the most in terms of total stats? Is there a correlation between weight and specific typing? Our data exploration (and yours) through this app will help us to better answer these questions."),
             br(),
             h3("Data Source"),
             p("This data consists of about 900 different pokemon across 8 generations. 47 different variables are being taken into account, such as indivdual stats (such as hitpoints or attack), typing (such as poison or fairy), damage multiplier (whether a move is super-effective or weak), and other aspects that influence a pokemon. This dataset was taken from ",
               tags$a(href = "https://www.kaggle.com/mariotormo/complete-pokemon-dataset-updated-090420", "Kaggle"),
               "which in turn scraped the data from Serebii, an official community website for pokemon."),
             br(),
             h3("Pages"),
             p("Each page corresponds to a dynamic interface, allowing anyone to create plots or toggle information with certain filters/buttons. The", strong("Data Exploration"), "tab will allow you to create various summaries for pokemon based on the variables used. Our focus will be TotalStats, a variable that has been created to aggregate all of the individual stats of a pokemon. The", strong("Modeling Page"), "will show some Machine Learning models that will give an idea of how well we can fit our models to new pokemon data (as there are more games that will be released. There will be individual tabs on this page that will give benefits/drawbacks to each model, some toggle settings to let the user control what goes into these models, and give the user options to play around with predictor values to gain predictions based on the responses. Lastly, there will be a", strong("Data"), "page that will allow the user to scroll through the data set and subset/export the data for their own use."),
             br(),
             h3("Gotta Catch Em All!"),
             tags$img(src="pokemon.jpg")
           )),
  
  
  
  tabPanel("Data Exploration",
           sidebarLayout(
             sidebarPanel(
               selectInput('attr1', 'Inform first attribute (X-axis)', choices = c("hp", "attack", "defense", "sp_attack", "sp_defense", "speed")),
               selectInput('attr2', 'Inform second attribute (Y-axis)', choices = c("hp", "attack", "defense", "sp_attack", "sp_defense", "speed"), selected = "attack"),
               sliderInput('gen', 'Select Generation', min = 1, max = 8, value = 1, step = 1),
               br(),
               h4("Generate Summary Statistics for Numeric Column"),
               selectInput('column', 'Choose Column For Summary', choices = c("height_m", "weight_kg", "hp", "attack", "defense", "sp_attack", "sp_defense", "speed", "catch_rate_pct"))
             ),
             mainPanel(strong(uiOutput("title", align = "center")), plotOutput("corrplot"), plotOutput("barplot"), dataTableOutput("table"))
           )),
  
  
  tabPanel("Modeling", mainPanel(tabsetPanel
          (tabPanel("Model Info"),
           tabPanel("Model Fitting" , sliderInput('Slider1', label = h3("Train/Test Split %"), min = 0, max = 100, value = 75), textOutput("cntTrain"), textOutput("cntTest"),),
           tabPanel("Prediction")))
  ),
  
  
  tabPanel("Data",
           sidebarLayout(
             sidebarPanel(
               h3('Subset Options'),
               selectInput('types', 'Select Typing', choices = c("Bug", "Dark", "Dragon", "Electric", "Fairy", "Fighting", "Fire", "Flying", "Ghost", "Grass", "Ground", "Ice", "Normal", "Poison", "Psychic", "Rock", "Steel", "Water"), multiple = TRUE, selectize = TRUE, selected = c("Bug", "Dark", "Dragon", "Electric", "Fairy", "Fighting", "Fire", "Flying", "Ghost", "Grass", "Ground", "Ice", "Normal", "Poison", "Psychic", "Rock", "Steel", "Water")),
               sliderInput('generations', 'Select Generation', min = 1, max = 8, value = 1, step = 1),
               selectInput('growth', 'Select Growth Rate', choices = c("Slow", "Medium Slow", "Medium Fast", "Fluctuating", "Fast", "Erratic"), multiple = TRUE, selectize = TRUE, selected = c("Slow", "Medium Slow", "Medium Fast", "Fluctuating", "Fast", "Erratic")),
               selectInput('status', 'Select Rarity', choices = c("Normal", "Mythical", "Sub Legendary", "Legendary"), multiple = TRUE, selectize = TRUE, selected = c("Normal", "Mythical", "Sub Legendary", "Legendary")),
               downloadButton('download',"Download the data")
             ), mainPanel(dataTableOutput("subset"))
           ))
)