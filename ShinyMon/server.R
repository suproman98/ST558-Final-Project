#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(DT)
pokemon <- read_csv("~/pokemon.csv")

function(input, output) {
    
    output$text <- renderText({
        
    })
    
    output$table <- renderDataTable({pokemon})
    
}