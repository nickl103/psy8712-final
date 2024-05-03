#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(shiny) 
library(dplyr)
library(ggplot2)


import_tbl <- readRDS("import.RDS") #reading in skinny data for 

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("PSY 8712 Final Project"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("genderselect",
                   label="Which gender would you like to see?",
                   choices=c("Male","Female","All"),
                   selected="All"),
      radioButtons("pidselect",
                   label="Which party identification would you like to see?",
                   choices=c("Republican","Democrat","Independent","All"),
                   selected="All"),
      radioButtons("votechoiceselect",
                   label="Do you want to see voters for Joe Biden, Donald Trump, or both?",
                   choices=c("Joe Biden",
                             "Donald Trump",
                             "Both"),
                   selected="Both"),
      selectInput("incomeselect",
                   label="What income amount would you like to see?",
                   choices= c("Under $10,000","$10,000 to $24,999","$25,000 to $49,999","$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 or more", "All"),
                  selected= "All"),
      selectInput("stateselect",
                  label= "What state would you like to see?",
                  choices=c("ME","NH","VT","MA","RI","CT","NY","NJ","PA","OH","IN","IL","MI","WI","MN","IA","MO","ND","SD","NE","KS","DE","MD","DC","VA","WV","NC","SC","GA","FL","KY","TN","AL","MS","AR","LA","OK","TX","MT","ID","WY","CO","NM","AZ","UT","NV","WA","OR","CA","AK","HI","All"),
                  selected= "All")
      ),
    
    mainPanel(
      plotOutput("distPlot")
    )
    )
)
  


server <- function(input, output) {
  output$distPlot <- renderPlot({
    
 
       if (input$genderselect != "All"){
      filtered_tbl <- filter(import_tbl, gender == input$genderselect)
       }
    else if (input$genderselect == "All"){
      filtered_tbl <- import_tbl
    }
    
      if(input$pidselect != "All"){
        filtered_tbl <- filter(import_tbl, pid == input$pidselect)
      }
    else if (input$pidselect == "All"){
      filtered_tbl <- import_tbl
    }
    
      if(input$votechoiceselect != "Both"){
        filtered_tbl <- filter(import_tbl, vote_2020 == input$votechoiceselect)
      }
    else if (input$votechoiceselect == "Both"){
      filtered_tbl <- import_tbl
    }
  
      if(input$incomeselect != "All"){
        filtered_tbl <- filter(import_tbl, income == input$incomeselect)
      }
      else if (input$incomeselect == "All"){
        filtered_tbl <- import_tbl
      }
      if(input$stateselect != "All"){
        filtered_tbl <- filter(import_tbl, state == input$stateselect)
      }
    else if (input$stateselect == "All"){
      filtered_tbl <- import_tbl
    }
    
    ggplot(filtered_tbl,
           aes(x= caseid, y=accuracy_score)) +
      geom_jitter() 
    
     })
}

shinyApp(ui = ui, server = server)