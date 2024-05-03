#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(shiny) #need this library for shiny
library(dplyr) #need this library for data filtering
library(ggplot2) #need this library for graph


import_tbl <- readRDS("import.RDS") #reading in skinny data for 

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("PSY 8712 Final Project"), #making the title for my web page
  sidebarLayout(
    sidebarPanel( #making side panel with all my questions
      radioButtons("genderselect", #creating a radio buttons option and internal name 
                   label="Which gender would you like to see?", #question users will see
                   choices=c("Male","Female","All"), #options users will see
                   selected="All"), #automatic selected option is all
      radioButtons("pidselect", #internal name
                   label="Which party identification would you like to see?", #question users will see
                   choices=c("Republican","Democrat","Independent","All"), #options users will see
                   selected="All"), #automatic selected option is all
      radioButtons("votechoiceselect", #creating a radiobuttons option and internal name
                   label="Do you want to see voters for Joe Biden, Donald Trump, or both?",#question users will see
                   choices=c("Joe Biden", #options users will see 
                             "Donald Trump",
                             "Both"),
                   selected="Both"), #automatic selected option is both 
      selectInput("incomeselect", #creating a select option for income for users and internal name
                   label="What income amount would you like to see?", #questions users will see
                   choices= c("Under $10,000","$10,000 to $24,999","$25,000 to $49,999","$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 or more", "All"), #options users will see
                  selected= "All"), #automatic selected option is all
      selectInput("stateselect", #creating a select option for state for users and internal name
                  label= "What state would you like to see?", #questions users will see
                  choices=c("ME","NH","VT","MA","RI","CT","NY","NJ","PA","OH","IN","IL","MI","WI","MN","IA","MO","ND","SD","NE","KS","DE","MD","DC","VA","WV","NC","SC","GA","FL","KY","TN","AL","MS","AR","LA","OK","TX","MT","ID","WY","CO","NM","AZ","UT","NV","WA","OR","CA","AK","HI","All"), #options users will see
                  selected= "All") #automatic selected 
      ),
    
    mainPanel( #the main panel
      plotOutput("distPlot") #the plot that I made below is what's going to be displayed in the main panel
    )
    )
)
  


server <- function(input, output) {
  output$distPlot <- renderPlot({ #render plot for main panel
    
 
       if (input$genderselect != "All"){ #if the All option is not selected 
      filtered_tbl <- filter(import_tbl, gender == input$genderselect) #then filter by selected gender
       }
    else if (input$genderselect == "All"){ #if all option is selected, do not filter
      filtered_tbl <- import_tbl
    }
    
      if(input$pidselect != "All"){ #if All option is not selected 
        filtered_tbl <- filter(import_tbl, pid == input$pidselect)#then filter by selected pid
      }
    else if (input$pidselect == "All"){ #if All is selected 
      filtered_tbl <- import_tbl #then do not filter
    }
    
      if(input$votechoiceselect != "Both"){ # if both is not selected
        filtered_tbl <- filter(import_tbl, vote_2020 == input$votechoiceselect) #then filter by selected president choice
      }
    else if (input$votechoiceselect == "Both"){ #if both is selected
      filtered_tbl <- import_tbl #do not filter
    }
  
      if(input$incomeselect != "All"){ #if All is not selected
        filtered_tbl <- filter(import_tbl, income == input$incomeselect) #filter by selected income level
      }
      else if (input$incomeselect == "All"){ #if All is selected
        filtered_tbl <- import_tbl #do not filter
      }
      if(input$stateselect != "All"){ #if all is not selected
        filtered_tbl <- filter(import_tbl, state == input$stateselect) #filter by selected state
      }
    else if (input$stateselect == "All"){ #if all is selected
      filtered_tbl <- import_tbl #do not filter
    }
    
    ggplot(filtered_tbl,#create a plot using the filtered tibble
           aes(x= caseid, y=accuracy_score)) + #caseid on x and accuracy score on y
      geom_jitter() #jitter plot because it makes the graph better to look at. 
    
     })
}

shinyApp(ui = ui, server = server) #launch app