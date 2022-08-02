#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Call Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(shiny)
library(DT)
library(tidyverse)
library(randomForest)
library(caret)

# Filter/Clean data
pokemon <- read_csv("pokemon.csv")
pokemon <- pokemon %>% mutate(catch_rate_pct = round((catch_rate/255)*100)) 
pokemodel <- pokemon %>% select(generation, status, type_number, type_1, height_m, weight_kg,	abilities_number, total_points, hp, attack, defense, sp_attack, sp_defense, speed, growth_rate, against_normal, against_fire, against_water, against_electric, against_grass, against_ice, against_fight, against_poison, against_ground, against_flying, against_psychic, against_bug, against_rock, against_ghost,	against_dragon, against_dark, against_steel, against_fairy, catch_rate_pct)
pokemodel <- na.omit(pokemodel)


# Create the Server Function
function(input, output, session) {
    
# Data Exploration
    
    ### Generate Title for Correlation Plot
    output$title <- renderUI({
        text <- paste("Relationship between ", input$attr1, " and ", input$attr2, " from Generation ", input$gen, " Pokemon", sep = "")
        h3(text)
        tools::toTitleCase(text)
    })
    
    ### Generate Correlation Plot
    output$corrplot <- renderPlot({
        pokemon %>%
            filter(generation == input$gen) %>%
            select(attr1 = input$attr1, attr2 = input$attr2, name = name, type = type_1) %>%
            ggplot(aes(attr1, attr2))+
            geom_point(aes(text=name, color=type), size = 2, alpha = 0.7)+
            geom_smooth(method = "lm", size=1.5)+
            labs(x=paste(input$attr1), y=paste(input$attr2))
    })
    
    ### Generate Title for Bar Plot
    output$title2 <- renderUI({
        text <- paste("Relationship between Type and Catch Rate from Generation ", input$gen, " Pokemon", sep = "")
        h3(text)
        tools::toTitleCase(text)
    })
    
    ### Generate Bar Plot
    output$barplot <- renderPlot({
        pokemon %>% 
            filter(generation == input$gen) %>%
            select(Type = type_1, Catch_Rate_Pct = catch_rate_pct) %>%
            ggplot(aes(Type, Catch_Rate_Pct, fill=Type))+
            geom_boxplot()+
            coord_flip()
    })
    
    ### Create Numerical Summaries for Specific Variables
    pokesum <- reactive({
        req(input$column)
        pokemon %>%
            select(input$column) %>%
            summary(input$column) %>%
            as.data.frame() %>%
            separate(Freq, c("Stat", "Value"), sep=":") %>%
            pivot_wider(names_from=Stat, values_from=Value)
    })
    
    ### Generate Table for Numeric Summaries
    output$table <- renderDataTable(pokesum())
    

# Modeling 
    
    observeEvent(input$model_run, {
        
        {
            
            ### Split up training and testing data
            split <- input$train_pct/100
            
            ### Conditional logic based on predictor selection
            poke_supervise <- pokemodel
            if (!("generation" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-generation)
            } else if (!("status" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-status)
            } else if (!("type_1" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-type_1)
            } else if (!("type_number" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-type_number)
            } else if (!("height_m" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-height_m)
            } else if (!("weight_kg" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-weight_kg)
            } else if (!("abilities_number" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-abilities_number)
            } else if (!("total_points" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-total_points)
            } else if (!("hp" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-hp)
            } else if (!("attack" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-attack)
            } else if (!("defense" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-defense)
            } else if (!("sp_attack" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-sp_attack)
            } else if (!("sp_defense" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-sp_defense)
            } else if (!("speed" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-speed)
            } else if (!("growth_rate" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-growth_rate)
            } else if (!("against_fire" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_fire)
            } else if (!("against_water" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_water)
            } else if (!("against_grass" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_grass)
            } else if (!("against_fighting" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_fighting)
            } else if (!("against_flying" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_flying)
            } else if (!("against_dark" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_dark)
            } else if (!("against_electric" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_electric)
            } else if (!("against_psychic" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_psychic)
            } else if (!("against_ghost" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_ghost)
            } else if (!("against_ground" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_ground)
            } else if (!("against_rock" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_rock)
            } else if (!("against_steel" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_steel)
            } else if (!("against_ice" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_ice)
            } else if (!("against_fairy" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_fairy)
            } else if (!("against_dragon" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_dragon)
            } else if (!("against_bug" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_bug)
            } else if (!("against_normal" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_normal)
            } else if (!("against_poison" %in% input$xvar)) {
                poke_supervise <- pokemodel %>% select(-against_poison)
            } else if (!input$xvar){
                stop('Select Predictors')
            }
            
            ### Perform Train/Test split
            set.seed(123)
            train <- sample(1:nrow(poke_supervise), size = nrow(poke_supervise)*split)
            test <- setdiff(1:nrow(poke_supervise), train)
            poketrain <- poke_supervise[train,]
            poketest  <- poke_supervise[test,]
            
            ### Set train control
            cvtrain <- trainControl(method = "repeatedcv", number=5, repeats=3)
 
    ## Models
            
            ### Generalized Linear Model
            glm <- train(catch_rate_pct ~ ., 
                             data = poketrain,
                             method = "glm", 
                             preProcess = c("center", "scale"),
                             trControl = cvtrain)
            
            ### Regression Tree
            regtree <- train(catch_rate_pct ~ ., 
                               data = poketrain,
                               method = "rpart", 
                               preProcess = c("center", "scale"),
                               trControl = cvtrain,
                               tuneGrid = expand.grid(cp=c(0:100)/1000))
            
            ### Random Forest
            rforest <- train(catch_rate_pct ~ ., 
                            data = poketrain,
                            method = "rf", 
                            preProcess = c("center", "scale"),
                            trControl = cvtrain,
                            tuneGrid = expand.grid(mtry=c(1:15)))
            
            
            ### Make Predictions Using postResample
            
            ### GLM Resample
            glm_test_pred <- predict(glm, newdata = select(poketest, -catch_rate_pct))
            glm_train_pred <- predict(glm, newdata = select(poketrain, -catch_rate_pct))
            glm_test_resample <- postResample(glm_test_pred, poketest$catch_rate_pct)
            glm_train_resample <- postResample(glm_train_pred, poketrain$catch_rate_pct)
            
            ### Regression Tree Resample
            regtree_test_pred <- predict(regtree, newdata = select(poketest, -catch_rate_pct))
            regtree_train_pred <- predict(regtree, newdata = select(poketrain, -catch_rate_pct))
            regtree_test_resample <- postResample(regtree_test_pred, poketest$catch_rate_pct)
            regtree_train_resample <- postResample(regtree_train_pred, poketrain$catch_rate_pct)
            
            ### Random Forest Resample
            rf_test_pred <- predict(rforest, newdata = select(poketest, -catch_rate_pct))
            rf_train_pred <- predict(rforest, newdata = select(poketrain, -catch_rate_pct))
            rf_test_resample <- postResample(rf_test_pred, poketest$catch_rate_pct)
            rf_train_resample <- postResample(rf_train_pred, poketrain$catch_rate_pct)
            
            
            ### Create RMSE Data Frame
            output$RMSE <- renderTable({
                MLsum <- data.frame(rbind(glm_train_resample, glm_test_resample, 
                                           regtree_train_resample, regtree_test_resample, 
                                           rf_train_resample, rf_test_resample))
                rownames(MLsum) <- c("GLM Train", "GLM Test", "RTree Train", "RTree Test", "RF Train", "RF Test")
                return(MLsum)
            })
            
            
            ### Plots of Variable Importance
            output$glm <- renderPlot({
                graph <- ggplot(varImp(glm)) + 
                    ggtitle("GLM Variable Importance")
                return(graph)
            })
            output$regtree <- renderPlot({
                graph <- ggplot(varImp(regtree)) + 
                    ggtitle("RTree Variable Importance")
                return(graph)
            })
            output$rforest <- renderPlot({
                graph <- ggplot(varImp(rforest)) + 
                    ggtitle("RF Variable Importance")
                return(graph)
            })
            
        }
    })
    
    
    ## Predictions
    poke_supervise <- reactive({
        req(input$column)
        pokemon %>%
            select(input$column) %>%
            summary(input$column)
    
    output$prediction <- renderDataTable({
        pred_poke <- data.frame(poke_supervise)
        glm_pred <- predict(glm, pred_poke)
        rtree_pred <- predict(regtree, pred_poke)
        rf_pred <- predict(rforest, pred_poke)
        pred_final <- data.frame(GLM = glm_pred, RTree = rtree_pred, RF = rf_pred)
    })
    


# Data Tab    
    
        ### Subset data
        pokesub <- reactive({ 
            subpoke <- subset(pokemon, type_1 %in% input$types & generation %in% input$generations & growth_rate %in% input$growth & status %in% input$status)
        return(subpoke)
        })
        
        ### Download subsetted data
        output$subset <- renderDataTable({pokesub()})
        output$download <- downloadHandler(
            filename = function(){"PokemonData.csv"}, 
            content = function(fname){
                write.csv(dataset(), fname)
            }
        )
}

