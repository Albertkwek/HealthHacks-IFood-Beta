#############################
#Relevant Imports
#############################

#install.packages("RODBC")
require(RODBC)
library(shiny)
setwd("C:\\Users\\Albert\\Desktop\\HealthHacks\\")

#############################
#Data Loading (Run this once, and comment them off later)
#############################

# channel <- odbcConnectAccess("FNDDS2013-2014.mdb")
# NutDesc = sqlQuery(channel, paste("select * from NutDesc"))
# FNDDSNutVal = sqlQuery(channel, paste("select * from FNDDSNutVal"))
# FoodPortionDesc = sqlQuery(channel, paste("select * from FoodPortionDesc"))
# FoodWeights = sqlQuery(channel, paste("select * from FoodWeights"))
# MainFoodDesc = sqlQuery(channel, paste("select * from MainFoodDesc"))
# #write.csv(NutDesc, file = "NutDesc.csv")
# 
# 
#


Quota <- read.csv("Book1.csv")
colnames(Quota)[1] = 'Gender'
#############################
#Run these once before running the app!!!!
#############################
# food_list <- MainFoodDesc$`Main food description`
# food_eaten_df <- data.frame(food_selected= character(), portion_selected = character(), serving_selected = integer())
#############################


#############################
#Functions for Recommendation
#############################

food_track = function(quota,food_code,portion_code,amount){
  taken = rep(0,ncol(quota))
  for (i in c(1:ncol(quota))){
    nutrient_code = NutDesc[NutDesc[,"Tagname"]==names(quota)[i],]$`Nutrient code`
    temp1 = FoodWeights[FoodWeights[,'Food code']==food_code,]
    weight = temp1[temp1[,"Portion code"]==portion_code,]$`Portion weight`
    temp2 = FNDDSNutVal[FNDDSNutVal[,'Food code']==food_code,]
    nutrient_value = weight / 100 * temp2[temp2[,'Nutrient code']==nutrient_code,]$`Nutrient value`
    taken[i] = amount*as.numeric(nutrient_value)
  }
  return(taken)
}

record = function(quota,food_code,portion_code,amount){
  taken = food_track(quota,food_code,portion_code,amount)
  for (i in c(1:ncol(quota))){
    quota[,i] = quota[,i]-taken[i]
  }
  return (quota)
}

recommendation = function(x,y,quota) {
  taken = matrix(0, nrow = length(x), ncol = ncol(quota))
  for (i in c(1:length(x))){
    taken[i,]=food_track(quota,x[i],y[i],1)
  }
  amount = rep(0,length(x))
  round.up = TRUE
  while (round.up) {
    round.up = FALSE
    for (i in c(1:length(x))){
      for (j in c(1:ncol(quota))){
        quota[,j] = quota[,j]-taken[i,j]
      }
      quota.up = 0
      print(quota)
      for (j in names(quota)) {if (quota[1,j] <= 0) {
        quota.up = 1
        for (k in c(1:ncol(quota))){
          quota[,k] = quota[,k]+taken[i,k]
        }
        }}
      if (quota.up == 0) {
        amount[i] = amount[i] + 1
        round.up = TRUE}
    }
  }
  return (amount)
}

#############################
#App implementation
#############################

shinyServer( function(input,output,session){
  ######################  
  # Update food page
  ######################  
  
  #This block of code updates the selection for age when 'gender' is selected
  observe({
    agegroups <- Quota[Quota$Gender == input$Gender,"Life.Stage.Group"] 
    updateSelectInput(session, "Age", choices = agegroups)
  })
  
  observe({
    food_code <- MainFoodDesc[MainFoodDesc$`Main food description` == input$Food, 'Food code']
    portion_query <- sqlQuery(channel, paste("select * from (select [Portion code], [Portion weight] from MainFoodDesc, Foodweights where [MainFoodDesc.Food code] = ", food_code, " and [MainFoodDesc.Food code] =  [Foodweights.Food code]) y, 
                                  FoodPortionDesc where [y.Portion code] = [FoodPortionDesc.Portion code] and [y.Portion code] < 90000"))
    portions <- portion_query[,"Portion description"]
    updateSelectInput(session,"Portion", choices = portions)
  })
  
  values <- reactiveValues()
  values$FoodSelecteddf <- data.frame(food_code = integer(0) ,food_selected = character(0), portion_selected = character(0), serving_selected = integer(0)) 
  
  newfood <- observe({
    if(input$FoodConfirm > 0) {
      isolate(values$FoodSelecteddf <- rbind(values$FoodSelecteddf, data.frame(food_code = MainFoodDesc[MainFoodDesc$`Main food description` == input$Food, 'Food code'],
                                                                               food_selected = input$Food, portion_selected = input$Portion, serving_selected = input$Servings)))
    }
  })

  output$FoodSelected <- DT::renderDataTable({
    DT::datatable(values$FoodSelecteddf)
  })

  observeEvent(input$GenderConfirm, {
    values$quotadf <- Quota[Quota$Gender==input$Gender & Quota$Life.Stage.Group==input$Age,c(3:29)]
  })
  
  observeEvent(input$FoodConfirm,{
    food_code = MainFoodDesc[MainFoodDesc$`Main food description` == input$Food, 'Food code']
    portion_code = FoodPortionDesc[FoodPortionDesc$`Portion description` == input$Portion, 'Portion code']
    amount = input$Servings
    values$quotadf <- isolate(record(values$quotadf,food_code,portion_code,amount))
  })

  output$QuotaLeft <- DT::renderDataTable({
    DT::datatable(values$quotadf)
  })
  
  ######################  
  # Recommendation Page
  ######################    
  
  observe({
    food_code <- MainFoodDesc[MainFoodDesc$`Main food description` == input$FoodPref, 'Food code']
    portion_query <- sqlQuery(channel, paste("select * from (select [Portion code], [Portion weight] from MainFoodDesc, Foodweights where [MainFoodDesc.Food code] = ", food_code, " and [MainFoodDesc.Food code] =  [Foodweights.Food code]) y, 
                                  FoodPortionDesc where [y.Portion code] = [FoodPortionDesc.Portion code] and [y.Portion code] < 90000"))
    portions <- portion_query[,"Portion description"]
    updateSelectInput(session,"PortionPref", choices = portions)
  })
  
  values$PreferedFooddf <- data.frame(food_code = integer(0) ,food_selected = character(0), portion_code = integer(0),
                                      portion_selected = character(0)) 
  
  preferedfood <- observe({
    if(input$PreferFoodAdd > 0) {
      isolate(values$PreferedFooddf  <- rbind(values$PreferedFooddf , data.frame(food_code = MainFoodDesc[MainFoodDesc$`Main food description` == input$FoodPref, 'Food code'],
                                                                               food_selected = input$FoodPref, portion_code = FoodPortionDesc[FoodPortionDesc$`Portion description` == input$PortionPref, 'Portion code'],
                                                                               portion_selected = input$PortionPref)))
    }
  })

  output$PrefFoodSelected <- DT::renderDataTable({
    DT::datatable(values$PreferedFooddf)
  })
  
  values$Recdf <- data.frame(food_selected = character(0), portion_selected = character(0), servings_recommended = integer(0)) 
  
  
  observeEvent(input$RecommendationConfirm, {
    x = values$PreferedFooddf$food_code
    y = values$PreferedFooddf$portion_code
    a = recommendation(x,y,values$quotadf)
    values$Recdf= data.frame(food_selected = values$PreferedFooddf$food_selected, portion_selected = values$PreferedFooddf$portion_selected,
                        servings_recommended = a)
  })
  
  output$Recommendationdf <- DT::renderDataTable({
    DT::datatable(values$Recdf)
  })
 
})