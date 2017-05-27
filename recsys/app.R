#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Matrix)
library(recommenderlab)
library(readr)

matrix <- read_delim("STEAM1.csv", ";", escape_double = FALSE, locale = locale(grouping_mark = "."), trim_ws = TRUE)
matrix[,1] = NULL
matrix2 = matrix
matrix = data.matrix(matrix)
sqmatrix = sqrt(matrix)
numNAs <- apply(sqmatrix, 1, function(z) sum(is.na(z)))
sqmatrix = sqmatrix[numNAs < 94,] # оценено больше 5 игр
sqmatrix1 = sqmatrix
for (i in (1:dim(sqmatrix)[2])) {
  sqmatrix1[,i] = cut(sqmatrix[,i], breaks = unique(quantile(sqmatrix[,i], c(.0, .20, .40, .60, 0.80, 1), na.rm = TRUE)),include.lowest = TRUE) }

steam_matrix_v <- as(sqmatrix1, "realRatingMatrix")



# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("STEAM games recommender system"),
  sidebarLayout(
    sidebarPanel(   
    selectInput("game1", h3("Select and rate a few video games you played"), c(colnames(matrix2)), selected = 1),
    sliderInput("rating1", "", min = 1, max = 5,
                value = 3),
    selectInput("game2", "", choices = colnames(matrix2), selected = "War Thunder"),
    sliderInput("rating2", "", min = 1, max = 5,
                value = 3),
    selectInput("game3", "", choices = colnames(matrix2), selected = "Dota 2"),
    sliderInput("rating3", "", min = 1, max = 5,
                value = 3),
    selectInput("game4", "", choices = colnames(matrix2), selected = "Grand Theft Auto V"),
    sliderInput("rating4", "", min = 1, max = 5,
                value = 3),
    selectInput("game5", "", choices = colnames(matrix2), selected = "The Witcher 3 Wild Hunt"),
    sliderInput("rating5", "", min = 1, max = 5,
                value = 3),
    selectInput("game6", "", choices = colnames(matrix2), selected = "Fallout 4"),
    sliderInput("rating6", "", min = 1, max = 5,
                value = 3),
    submitButton("Submit")),
    mainPanel(h3("You might like these games"),
              tableOutput('results'))
  ))
  
  
# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$results <- renderTable({
   
    set.seed(100)
    steam.test.ind = sample(seq_len(nrow(steam_matrix_v)), size = nrow(steam_matrix_v)*0.2)
    steam.test = steam_matrix_v[steam.test.ind,]
    steam.main = steam_matrix_v[-steam.test.ind,]
    
    recc_model <- Recommender(data = steam.test, method = "UBCF", parameter = list(method = "cosine"))
    user = (rep(NA,99))
    
    
    user[which(colnames(matrix2) == input$game1)] = as.numeric(input$rating1)
    user[which(colnames(matrix2) == input$game2)] = as.numeric(input$rating2)
    user[which(colnames(matrix2) == input$game3)] = as.numeric(input$rating3)
    user[which(colnames(matrix2) == input$game4)] = as.numeric(input$rating4)
    user[which(colnames(matrix2) == input$game5)] = as.numeric(input$rating5)
    user[which(colnames(matrix2) == input$game6)] = as.numeric(input$rating6)
    user = matrix(user,nrow = 1,ncol = 99)
    user_matrix <- as(user, "realRatingMatrix")
    recommends <- predict(object = recc_model, newdata = user_matrix, n = 4)
    recc <- recommends@items[[1]]
    games_user <- recommends@itemLabels[recc]
    print(games_user)
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

