#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Call Libraries
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(shiny)
library(DT)
library(tidyverse)
library(shinythemes)
library(randomForest)
library(caret)

# Filter/Clean Data
pokemon <- read_csv("pokemon.csv")
pokemon <- pokemon %>% mutate(catch_rate_pct = round((catch_rate/255)*100))
pokemodel <- pokemon %>% select(generation, status, type_number, type_1, height_m, weight_kg,	abilities_number, total_points, hp, attack, defense, sp_attack, sp_defense, speed, growth_rate, against_normal, against_fire, against_water, against_electric, against_grass, against_ice, against_fight, against_poison, against_ground, against_flying, against_psychic, against_bug, against_rock, against_ghost,	against_dragon, against_dark, against_steel, against_fairy, catch_rate_pct)
pokemodel <- na.omit(pokemodel)

# Create UI
navbarPage(theme = shinytheme("sandstone"),
  "My Application",
  
  ## Create About Page
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
  
  
# Create Data Exploration Tab

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
             mainPanel(strong(uiOutput("title", align = "center")), plotOutput("corrplot"), strong(uiOutput("title2", align = "center")), plotOutput("barplot"), dataTableOutput("table"))
           )),
  
  
# Create Modeling Page  

  ### Modeling Info Page
  tabPanel("Modeling", mainPanel(tabsetPanel
          (tabPanel("Model Info",
                    mainPanel(
                      h3("Multiple Linear Regression"),
                      br(),
                      p("Multiple Linear Regression is a supervised learning approach that takes in two or more predictors and higher order terms. We can add more main effect terms (predictors by themselves), interaction terms with var1:var2, and all possible combinations with var1*var2. This technique lets us see the variation of a model and how each predictor contributes to the overall variance."),
                    withMathJax(),
                    helpText('Formula for multiple linear regression is: $$y = \\beta_{0} + \\beta_{1}x_{1i} + \\beta_{2}x_{2i} + \\beta_{3}x_{1i}x_{2i} + E_{i}$$'),
                    p("We usually want to focus on reducing prediction error (usually using MSE in this case). The benefits of multiple linear regression are that a model like this can be used to predict an outcome based on the explanatory predictors used and can look at the relationship between the predictors. However, the drawbacks are that assumptions are made for inference (such as that the errors are normally distributed, the data allows for t and F-tests, and constant variance is assumed. When the assumptions made for MLR are violated, we need to use Generalized Linear Models or a different method."),
                    br(),
                    h3("Regression Tree"),
                    br(),
                    p("A regression tree is built through an algorithm that partitions the data into separate trees or branches. The tree iterates on each region, separating the data into smaller groups based on the splits that are made. The benefits of a tree are that it is very easy to interpret and the outputs are easy to understand. Predictors don't need to be scaled, there is built-in variable selection, and assumptions aren't necessary. Trees also automatically assume interaction within the model. However, the drawbacks are that any small change in the data can have a big effect on the tree, the algorithm is greedy, and the tree usually needs to be pruned to prevent overfitting. If prediction is the main focus, this method usually isn't the best."),
                    withMathJax(),
                    helpText('Minimizing Residual Sum of Squares for Each Predictor Value: $$R_{1}(j, s) = {(x|x_{j} < s)}\\ and\\ R_{2}(j, s) = {(x|x_{j} \\geq s)}$$'),
                    br(),
                    h3("Random Forest Modeling"),
                    br(),
                    p("Random Forest Modeling is a type of Bagging (bootstrap aggregating) method that treats a sample as a population, creates bootstrap samples to generate trees from, and averages the results. The difference is that the model doesn't use all the predictors but rather uses a subset of random predictors for each bootstrap sample/tree fit. The benefits of using this modeling is that you're focusing more on prediction and decreasing variability with individual tree fits, but the drawbacks are you lose interpretability of the model/outputs and the computation time is much higher."),
                    withMathJax(),
                    helpText('Selecting Classification Predictors: m = \\(\\sqrt{p}\\)'),
                    helpText('Selecting Regression Predictors: $$m = \\frac{p}{3}$$')
                    )),
 
  ### Model Fit Page
  tabPanel("Model Fitting", 
                    sliderInput("train_pct", label = h3("Train/Test Split %"), min = 0, max = 100, value = 75), 
                    selectInput("xvar", label = "Select variables:", choices = c(names(pokemodel %>% select(-catch_rate_pct))), multiple = TRUE, selected = NULL),
                    actionButton("model_run", "Run Supervised Models"), mainPanel(tableOutput("RMSE"), plotOutput("glm"), plotOutput("regtree"), plotOutput("rforest"))),
            
  ### Model Prediction Page         
  tabPanel("Prediction", mainPanel(dataTableOutput("prediction")))
            ))
  ),

  
  ### Create Data   
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