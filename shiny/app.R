#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(RColorBrewer)
library(scales)
library(car)


gss <- read_csv("https://raw.githubusercontent.com/ryanburge/gss_shiny/master/shiny.csv")
year <- gss$year
gss$partyid <- factor(gss$partyid, levels=unique(gss$partyid))
gss <- na.omit(gss)

gss$reltrad <- Recode(gss$reltrad, "1='Evangelical Protestants';
                       2='Mainline Protestants';
                       3='Black Protestants';
                       4='Catholic'; 
                       5='Jewish';
                       6= 'Other Faith';
                       7= 'No Faith'")


ui <- shinyUI(fluidPage(
  
  titlePanel("Party Identification of Religious Groups over the Last Four Decades"),
  
  fluidRow(
    column(4,
           wellPanel(
  
        sliderInput("years", "Year:", 
                  min= min(year),
                  max= max(year),
                  value=min(year), step = 1, sep ="", animate = TRUE)), 
    selectInput("trad", "Tradition:", as.character(levels(as.factor(gss$reltrad)))
                , selectize=TRUE)),
    
   
    h5("This Shiny App has a full write-up at", a("Religion in Public", href="https://religioninpublic.blog/2017/05/11/44-years-of-religion-and-politics-in-one-graph/")),
    h5("Some years are not available from the GSS and therefore no plot will be displayed"),
    h5("There’s also the option to hit the “play” button and watch a tradition’s partisan distribution change over the entire time period."), 
    
    helpText("Data from the General Social Survey (1972-2016)"),
    
    column(12, 
      plotOutput("plot2")
    )
  )
))



server <- function(input,output){
  
  gss1<-reactive({
    gss %>% filter(reltrad == input$trad) %>%  filter(year == input$years) %>%  mutate(partyid = factor(partyid, levels = c("Strong Democrat", "Moderate Democrat","Lean Democrat", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")))  })
  
 output$plot2<-renderPlot({
    ggplot(gss1(),aes(x=partyid, fill=factor(partyid)), color= factor(partyid))+geom_bar(aes(y = (..count..)/sum(..count..))) + theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="RdBu", direction=-1) + theme(legend.position="none")  + xlab("Party Identification") + ylab("Percent of Respondents") + scale_y_continuous(labels=percent)}, height = 600)}

  

  


shinyApp(ui, server)
