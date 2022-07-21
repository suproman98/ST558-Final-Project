#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(shiny)
library(DT)

pokemon <- read_csv("~/pokemon.csv")

navbarPage(
    "My Application",
    tabPanel("Component 1", 
             mainPanel(
                 h3("Purpose"),
                 br(),
                 p("The purpose of this app is to explore the world of pokemon and understand diffeerent relationships between typing, catch rate, and overall power. Is there a clear-cut pokemon type that is strong or hard to catch throughout each generation? Which typing stands out the most in terms of total stats? Is there a correlation between weight and specific typing? Our data exploration (and yours) through this app will help us to better answer these questions."),
                 br(),
                 h3("Data Source"),
                 p("This data consists of about 800 different pokemon across 7 generations. 37 different variables are being taken into account, such as indivdual stats (such as hitpoints or attack), typing (such as poison or fairy), damage multiplier (whether a move is super-effective or weak), and other aspects that influence a pokemon. This dataset was taken from ",
                   tags$a(href = "https://www.kaggle.com/datasets/rounakbanik/pokemon", "Kaggle"),
                   "which in turn scraped the data from Serebii, an official community website for pokemon."),
                 br(),
                 h3("Pages"),
                 p("Each page corresponds to a dynamic interface, allowing anyone to create plots or toggle information with certain filters/buttons. The", strong("Data Exploration"), "tab will allow you to create various summaries for pokemon based on the variables used. Our focus will be TotalStats, a variable that has been created to aggregate all of the individual stats of a pokemon. The", strong("Modeling Page"), "will show some Machine Learning models that will give an idea of how well we can fit our models to new pokemon data (as there are more games that will be released. There will be individual tabs on this page that will give benefits/drawbacks to each model, some toggle settings to let the user control what goes into these models, and give the user options to play around with predictor values to gain predictions based on the responses. Lastly, there will be a", strong("Data"), "page that will allow the user to scroll through the data set and subset/export the data for their own use."),
                 br(),
             )),
    tabPanel("Component 2",
             sidebarLayout(
                 sidebarPanel(
                     # Make a list of checkboxes
                     radioButtons(
                         "radio",
                         label = h3("Radio buttons"),
                         choices = list("Choice 1" = 1, "Choice 2" = 2)
                     )),
                 mainPanel(textOutput("text"))
             )),
    tabPanel("Component 3", mainPanel(dataTableOutput("table")))
)