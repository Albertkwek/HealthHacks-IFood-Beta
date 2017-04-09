


shinyUI(fluidPage(
  navbarPage("iFood Beta",
             tabPanel("Food Recording",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("styles.css"),
                        includeScript("gomap.js")
                      ),
                      fluidRow(
                        column(3, selectInput(inputId = "Gender", label = "Your Gender", c("Infant" = "Infants", "Children" = "Children", "Male" = "Males", "Female" = "Females", "Pregnancy" = "Pregnancy", "Lactation" = "Lactation"))),
                        column(3, conditionalPanel("input.Gender", selectInput("Age", label = "Your Age",c("All Ages" = "")))),
                        br(),
                        actionButton("GenderConfirm", "Gender Confirm")
                      ),
                      fluidRow(
                        column(3, selectInput(inputId = "Food", label = "Enter what you have eaten", food_list)),
                        column(3, conditionalPanel("input.Food", selectInput(inputId = "Portion", label = "Select your portion",c("All portions" = "")))),
                        column(3, conditionalPanel("input.Portion", numericInput(inputId = "Servings", label = "select your servings", 1, min = 0 ))),
                        br(),
                        actionButton("FoodConfirm", "Go")
                      ),
                      fluidRow(
                        conditionalPanel("input.FoodConfirm",DT::dataTableOutput("FoodSelected")),
                        conditionalPanel("input.GenderConfirm",DT::dataTableOutput("QuotaLeft"))
                      )
             ),
             tabPanel("Food Recommendation",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("styles.css"),
                        includeScript("gomap.js")
                      ),
                      fluidRow(
                        column(3, selectInput(inputId = "FoodPref", label = "Enter what you want to eat", food_list)),
                        column(3, conditionalPanel("input.FoodPref", selectInput("PortionPref", label = "Select your portion",c("All portions" = "")))),
                        actionButton("PreferFoodAdd", "Add Food")
                      ),
                      fluidRow(
                        conditionalPanel("PreferFoodAdd", DT::dataTableOutput("PrefFoodSelected")),
                        actionButton("RecommendationConfirm", "Recommend away!"),
                        conditionalPanel("RecommendationConfirm", DT::dataTableOutput("Recommendationdf"))
                      )
             )
        )
))