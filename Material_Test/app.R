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
library(shinymaterial)
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



ui <- material_page(title = "Party Identification of Religious Groups over the Last Four Decades", nav_bar_color = "blue darken-4",
                    
                    material_row(
                      material_column(
                        width = 2,
                        material_card(
                          title = "",
                          depth = 4,
                    material_dropdown(
                      input_id = "trad",
                      label = "Tradition",
                      color = "BLACK",
                      choices = as.character(levels(as.factor(gss$reltrad)),
                      selected = "Black Protestant"
                    )),
                    material_slider(
                      input_id = "years",
                      label = "Years:",
                      min_value = min(year),
                      max_value = max(year),
                      initial_value = min(year),
                      color = "RED"
                    )),
                    material_modal(
                      modal_id = "showcase_modal",
                      button_text = "Data Info",
                      button_icon = "",
                      title = "Some Background on the Data",
                      button_color = "blue darken-4",
                      tags$p("This Shiny App has a full write-up at Religion in Public. Data from the General Social Survey (1972-2016)")
                    )),
                    
                    material_column(
                      width = 9,
                      material_card(
                        title = "Political Ideology",
                        depth = 4,
                        plotOutput("plot2")
                      ))
                    ))


server <- function(input,output){
  
  gss1<-reactive({
    gss %>% filter(reltrad == input$trad) %>%  
      filter(year == input$years) %>%  
      mutate(partyid = factor(partyid, levels = c("Strong Democrat", "Moderate Democrat","Lean Democrat", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")))  })
  
  output$plot2<-renderPlot({
    ggplot(gss1(),aes(x=partyid, fill=factor(partyid)), color= factor(partyid))+
      geom_bar(aes(y = (..count..)/sum(..count..))) + theme(axis.text.x = element_text(angle = 90)) + 
      scale_fill_brewer(palette="RdBu", direction=-1) + theme(legend.position="none")  + 
      xlab("Party Identification") + ylab("Percent of Respondents") + scale_y_continuous(labels=percent)})}



shinyApp(ui, server)
