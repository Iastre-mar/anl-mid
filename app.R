#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
getwd()
setwd("/home/iastre/Документы/Skillbox/Аналитика средний/hw_4/hw_4")
tit <- read.csv("Titanic.csv", row.names = 1)
kach_kol <- function(df){
    kach = c()
    kol = c()
    for (x in colnames(df)){
        vec <- filter(select(df, x), (select(df, x)[1] != "" & select(df, x)[1] != " "))
        uniq <- unique(vec)
        nunique <- nrow(uniq)
        if (nunique < 6){
            kach = c(kach, x)
        } else {
            if ((is.numeric(uniq[1,]) & (nunique != length(vec[,1])))){
            kol = c(kol, x)
        }}}
    return(list(kach = kach, kol = kol))
}
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Titanic passengers"),
    
    # Sidebar with a slider input for number of bins 
    
    fluidRow(
        
        column(4,
               selectInput("kach", 
                           "Качественный показатель(Barplot)",
                           choices = kach_kol(tit)$kach), offset=0
        ),
        column(4,
               selectInput("kol", "Количественный показатель(Hist)",
                           choices = kach_kol(tit)$kol)
        )
    ),
    fluidRow(
        column(4,
               selectInput('ucolor_kach','Цвет столбиковой диаграммы:',choices = colors(distinct=TRUE)), offset=0
       ),
        column(4,
               selectInput('ucolor_kol','Цвет гистограммы:',choices = colors(distinct=TRUE))
    ),
    fluidRow(
        
        column(4,
               sliderInput("bins2",
                           "Количество столбцов в гистограмме:",
                           min = 1,
                           max = 50,
                           value = 5), offset=4)),
    div(
        mainPanel(splitLayout(cellWidths = c("50%", "50%"), plotOutput("distPlot1"), plotOutput("distPlot2"))))),
        style = "position=center")



server <- function(input, output) {
    output$distPlot1 <- renderPlot({
        x    <- table(na.omit(filter(select(tit, input$kach), (select(tit, input$kach)[1] != "" & select(tit, input$kach)[1] != " "))))
        
        # draw the histogram with the specified number of bins
        barplot(x[x>0], col = input$ucolor_kach,
                xlab = input$kach, ylab = "Количество пассажиров", main="Barplot качественной характеристики")})
    output$distPlot2 <- renderPlot({
        y       <- na.omit(select(tit,input$kol))
        #std     <- sd(y[,1])
        #y       <- y[((sapply(y, FUN=mean)) - 3 * std < y) & ((sapply(y, FUN=mean)) + 3 * std > y)]
        min2    <- min(y)
        max2    <- max(y)
        breaks2 <- seq(min2, max2, length.out = input$bins2 + 1)
        
        # draw the histogram with the specified number of bins
        hist(x=y[,1], col = input$ucolor_kol, breaks = breaks2,
             xlab = input$kol, ylab = "Количество пассажиров", main="Гистограмма количественной характеристики")})
    
}

# Run the application 
shinyApp(ui = ui, server = server)

