library(shiny) 
library(readr)
library(shinyWidgets)
library(shiny) 
library(plotly) 
library(dplyr)
library(formattable)
library(leaflet)
library(leaflet.extras) 

ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"
    ) ,
    tags$style(HTML('
    .container-fluid {
      padding-right: 15px;
      padding-left: 15px;
      margin-right: 35px;
      margin-left: 35px;
    } 
    
     .fixed-container {
          position: fixed;
          top: 50px; /* Adjust the top position as needed */
          left: 50px; /* Adjust the left position as needed */
        }
        
        .plot-container {
          height: 400px; /* Set your desired height or use relative units */
        }
    
    
  ')) 
  ) , 
    
  uiOutput('dynamichead'), 
  
  fluidRow(style='background: blue ;  
          
           background: #093498;
           height: 20px;' 
           ) , uiOutput('dynamicUI')   
  
   
  
)  
            
    
#priceDistribution 
 

 




