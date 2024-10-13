library(readr)
library(shinyWidgets)
library(shiny) 
library(plotly) 
library(dplyr)
library(dichromat) 
library(ggsci)
library(leaflet) 
library(leaflet.providers)
library(leaflet.extras) 


data <- read_csv("netherladsrentdata.csv")

filterByCity <- function(data, cities) {
  if ("All" %in% cities) {
    return(data)
  } else {
    return(subset(data, city %in% cities))
  }
}

filterByPropertyType <- function(data, propertyType) {
  if (is.null(propertyType) || propertyType == "All") {
    return(data)
  } else {
    return(subset(data, propertyType %in% propertyType))
  }
}

filterByFurnishType <- function(data, furnishType) {
  if (is.null(furnishType) || furnishType == "All") {
    return(data)
  } else {
    return(subset(data, furnish %in% furnishType))
  }
}


server <- function(input, output) {
  selectedTab <- reactiveVal("default") 
  
 
  
  
  observeEvent(input$generateReport, {
    showModal(modalDialog(
      title = h4("Report Window" , style='color: #151413;
             text-align: center;') , 
    
      tabsetPanel(
        tabPanel("Country Level", 
                 
                 actionButton("submitCountry", "Submit") ),
        tabPanel("City Level",br(),  fluidPage(style='margin-left: 120px; 
                                         color: #151413;
                                         text-align: center;', 
                 selectInput("cityInput", "Select City",  
                             choices = c("All", unique(data$city)), 
                             ),
                 br(),
                 selectInput("propertyInput", "Select Property Type",
                             choices = c("All", unique(data$propertyType)),
                                         multiple = TRUE),
                 br(),
                 selectInput("furnishInput", "Select Furnish Type",
                             choices = c("All", unique(data$furnish)) , multiple = TRUE)), br(), 
                 actionButton("submitCity", "Submit" , style='background: #fbfbff;
    width: 180px;
    text-align: center;
    margin-left: 190px;
    font-weight: 800;')      
        )),
        
        ) 
    )
  }) 
  
  
  filteredData <- reactive({
    filtered <- data
    
    if (!("All" %in% input$cityInput)) {
      filtered <- filtered %>% filter(city %in% input$cityInput)
    }
    
    if (!is.null(input$propertyInput) && input$propertyInput != "All") {
      filtered <- filtered %>% filter(propertyType == input$propertyInput)
    }
    
    if (!is.null(input$furnishInput) && input$furnishInput != "All") {
      filtered <- filtered %>% filter(furnish == input$furnishInput)
    }
    
    return(filtered)
  })
    

    
    
      
    
      
  
  
  
  observeEvent(input$submitCountry, {
    selectedTab("country")
  })
  
  observeEvent(input$submitCity, {
    selectedTab("city")
  })  
  
  
  
  
  
  output$dynamichead <- renderUI({
    if (selectedTab() == "default") {
      fluidRow(style='background: blue ;  
           
           background: #093458;
           height: 80px;' , 
               
               column( style='width:180px;
                    height: 60px;
                    background: aqua;
                    margin-top: 9px;
                    margin-left: 60px; 
                   ' , 
                       
                       width = 2,
                       h4("Country" , style='font-size: 10px; 
                                  margin-top: 3px;
                                  text-align: center;
                                  font-size: 12px;
                                  margin-bottom: -2px;'), 
                       
                       h3('Netherlands' ,style='font-size: 28px;
                                margin-bottom: 12px; 
                                margin-top: 2px;
                                text-align: center;' )
               ) ,
               column(width=3  ,  
                      HTML('<i class="fas fa-map-marker-alt fa-3x fa-beat-fade" style="color: #f3f2f2;
                                                             margin-top: 15px; 
                                                             margin-bottom: 40px; 
                                                             border-radius: 50%;
                                                             animation: beat 5s infinite;
                                                             "></i>')), 
               
               column(2 , actionButton("generateReport", "Generate Report" , 
                                       style='    color: #333;
    background-color: #fff;
    border-color: #ccc;
    margin-top: 10px;
    height: 60px;
    width: 180px;')) , column(2, uiOutput('infoboxP')) , column(1, uiOutput('infoboxH')),
               column(1,uiOutput('infoboxPK'))
               
      )
       
      
      
    } else if (selectedTab() == "country") {
      fluidRow(
        column(
          width = 6,
          "Country Level UI"
          # Add your UI components for the Country Level here
        )
      )
    } else if (selectedTab() == "city") {
      fluidRow(style='background: blue ;  
           
           background: #093458;
           height: 80px;' , 
               
               column( style='width:180px;
                    height: 60px;
                    background: aqua;
                    margin-top: 9px;
                    margin-left: 60px; 
                   ' , 
                       
                       width = 2,
                       h4("City" , style='font-size: 10px; 
                                  margin-top: 3px;
                                  text-align: center;
                                  font-size: 12px;
                                  margin-bottom: -2px;'), 
                       
                       h3(textOutput('city') ,style='font-size: 28px;
                                margin-bottom: 12px; 
                                margin-top: 2px;
                                text-align: center;' )
               ) ,
               column(width=3  ,  
                      HTML('<i class="fas fa-map-marker-alt fa-3x fa-beat-fade" style="color: #f3f2f2;
                                                             margin-top: 15px; 
                                                             margin-bottom: 40px; 
                                                             border-radius: 50%;
                                                             animation: beat 5s infinite;
                                                             "></i>')), 
               
               column(2 , actionButton("generateReport", "Generate Report" , 
                                       style='    color: #333;
    background-color: #fff;
    border-color: #ccc;
    margin-top: 10px;
    height: 60px;
    width: 180px;')) , column(2, uiOutput('infoboxPC')) , column(1, uiOutput('infoboxHC')),
               column(1,uiOutput('infoboxPKC'))
               
      )
    }
    
    
    
    
  })
  
  
  output$dynamicUI <- renderUI({
    if (selectedTab() == "default") {
      fluidPage(style='margin-top: -19px;
    background: #0d0031f5 ; 
    margin-right: -15px;
    margin-left: -15px; 
            height: 700px;', 
                
                fluidRow(
                  
                  br(),
                  column(4, class= "plot-container",  plotlyOutput("pricePlot")),
                  column(8,
                         fluidRow(
                           column(style = 'margin-left:140px;',class= "plot-container", 3, plotlyOutput("pieChart")),
                           column(2,style = 'margin-left:0px;',class= "plot-container", plotlyOutput("pieChartF")),
                           column(1,style = 'margin-left:140px;',class= "plot-container", plotlyOutput("livebar"))
                         )))    
                
                
                
                
                , fluidRow(
                  column(4,style ='margin-top:-70px;', plotlyOutput("prop")), 
                  column(8,
                         fluidRow(
                           column(style = '
    overflow-y: scroll;
    height: 375px;
    width: 400px;
    margin-left: 155px;
    margin-top: -130px;
    background: #1a1a40b5; 
    color: #fff6f6ad
                     ', 3,
                                  formattableOutput("forTable")) , 
                           column(style = 'margin-left: 5px;
                     width: 200px;
                     margin-top: -130px;
                     background: #1a1a40b5;
                       height: 145px;' , 2,
                                  plotlyOutput("priceDifference"))    
                         ) , column(2, id='AzA' , 
                                    style='width: 16.66666667%;
    margin-left: 530px;
    margin-top: -224px;' ,
                                    
                                    plotlyOutput(('rentApp')))
                  ) )  
                
                
      ) 
      
      
    } else if (selectedTab() == "country") {
      fluidRow(
        column(
          width = 6,
          "Country Level UI"
          # Add your UI components for the Country Level here
        )
      )
    } else if (selectedTab() == "city") {
      fluidPage(style='margin-top: -19px;
    background: #0d0031f5 ; 
    margin-right: -15px;
    margin-left: -15px; 
            height: 700px;', 
                
                fluidRow(
                  
                  br(),
                  column(4, class= "plot-container",  plotlyOutput("citybar")),
                  column(8, fluidRow(column(3, style= 'margin-left:150px;', 
                                            leafletOutput('accommodationMap' ,  width = 740 , height = 260)
                  ))
                  ))    
                
                
                
                
                , fluidRow(
                  column(4,style ='margin-top:-70px;', fluidRow(
                    column(style = 'margin-left:0px;',class= "plot-container", 1, plotlyOutput("pieChartC")),
                    column(1,style = 'margin-left:239px;',class= "plot-container", plotlyOutput("pieChartFC")),
                    
                  )), 
                  column(8,
                         fluidRow(
                           column(style = '
    overflow-y: scroll;
    height: 375px;
    width: 400px;
    margin-left: 155px;
    margin-top: -130px;
    background: #1a1a40b5; 
    color: #fff6f6ad
                     ', 3,
                                  formattableOutput("forTable2")) , 
                           column(style = 'margin-left: 5px;
                     width: 200px;
                     margin-top: -130px;
                     background: #1a1a40b5;
                       height: 145px;' , 2,
                                  plotlyOutput("priceDifference"))    
                         ) , column(2, id='AzA' , 
                                    style='width: 16.66666667%;
    margin-left: 530px;
    margin-top: -224px;' ,
                                    
                                    plotlyOutput(('pricePlot2')))
                  ) )  
                
                
      )
    }
    
    
    
    
  }) 
  
  


    
    
    output$infoboxP<-renderUI({
        infobox <- column( style='width:fit-content;
                    height: 60px;
                    background: #f1ffff;
                    margin-top: 9px;
                    margin-left: 60px; 
                   ' , 
                           
                           width = 2,
                           h4("AVG Rent Price" , style='font-size: 10px; 
                                  margin-top: 3px;
                                  text-align: center;
                                  font-size: 12px;
                                  margin-bottom: -2px;'), 
                           
                           h3(paste0(as.character(round(mean(data$rent))) ,' €') ,style='font-size: 28px;
                                margin-bottom: 12px; 
                                margin-top: 2px;
                                text-align: center;' )) }) 
        
    output$infoboxH<-renderUI({
      infobox <- column( style='width:150px;
                    height: 60px;
                    background: #f1ffff;
                    margin-top: 9px;
                    margin-left: -28px;; 
                   ' , 
                         
                         width = 2,
                         h4("Total of properties " , style='font-size: 10px; 
                                  margin-top: 3px;
                                  text-align: center;
                                  font-size: 12px;
                                  margin-bottom: -2px;'), 
                         
                         h3(paste0(as.character(round(nrow(data)))) ,style='font-size: 28px;
                                margin-bottom: 12px; 
                                margin-top: 2px;
                                text-align: center;' )) }) 
    output$infoboxPK<-renderUI({
      infobox <- column( style='width:150px;
                    height: 60px;
                    background: #f1ffff;
                    margin-top: 9px;
                    margin-left: 30px; 
                   ' , 
                         
                         width = 1,
                         h4("AVG rent per m²" , style='font-size: 6px; 
                                  margin-top: 3px;
                                  text-align: center;
                                  font-size: 12px;
                                  margin-bottom: -2px;'), 
                         
                         h3(paste0(as.character(round(mean(data$rent/data$areaSqm)))," €") ,style='font-size: 28px;
                                margin-bottom: 12px; 
                                margin-top: 2px;
                                text-align: center;' )) }) 
    
    # ----- defult level ------   
     output$pricePlot <- renderPlotly({
      d <- data %>% 
        group_by(city) %>% 
        summarise(rent = round(mean(rent))) %>% 
        arrange(desc(rent))
      d <- head(d, 10) 
      d$rank <- rownames(d)
      custom_colors <- rev(pal_material("light-blue")(10))
      # Create the plot
      p <- plot_ly(d, x = ~rent, y = ~reorder(city, rent), 
                   type = "bar", orientation = "h", 
                   marker = list(color = custom_colors,
                                 hoverlabel = list(bgcolor = "lightblue",
                                                   font = list(color = "black"),
                                                   align = "left",
                                                   namelength = -1,
                                                   bordercolor = "black",
                                                   borderwidth = 4,
                                                   pad = list(30, 30))),
                   hovertemplate = '<span style=" margin-left: 15px;font-size: 30px;color: black;text-align: center;text-overflow: clip;">Rank: %{customdata}</span><br><br><span style="font-size: 30px;color: black;text-align: center;text-overflow: clip;">%{y}</span><br><extra></extra>',
                   customdata = ~rank) %>% 
        layout(
          title = "AVG Rent per City",
          xaxis = list(title = "Rent Price (in Euro)"),
          yaxis = list(title = ""),
          height = 320,
          width = 590,
          plot_bgcolor = "#1a1a40b5",
          paper_bgcolor = "#1a1a40b5",
          font = list(color = 'white'),
          xaxis = list(showgrid = FALSE),
          yaxis = list(showgrid = FALSE)
        )
      
      p %>% layout(yaxis = list(showticklabels = FALSE))
    

      
    })
    
    
    output$livebar <- renderPlotly({
      d <- data %>% 
        group_by(living) %>% 
        summarise(count = n()) %>% 
        arrange(desc(count))
      
      custom_colors <- c('#5389bd', '#5aa3db', '#82b8dc' ,'#82b8dc','#82b8dc')
      
      p <- plot_ly(d, x = ~reorder(living, count), y = ~count,
                   marker = list(color = custom_colors),
                    type = "bar", orientation = "v") %>%
        layout(
          title = "",
          xaxis = list(title = ""),
          yaxis = list(title = "Properties"),
          height = 250,
          width = 223,
          plot_bgcolor = "#1A1A40",
          paper_bgcolor = "#1A1A40",
          font = list(color = 'white'),  # Set font color to red
          showlegend = FALSE,  # Remove legend
          xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
          yaxis = list(showgrid = FALSE)  # Remove y-axis gridlines
        )
      
      p
    })
    
    
    
    output$prop <- renderPlotly({
      d <- data %>% 
        group_by(city) %>% 
        summarise(prop = n() , areaSqm=mean(areaSqm)) %>% 
        arrange(desc(prop))
      d <- head(d, 10) 
      custom_colors <- rev(pal_material("light-blue")(10))
      d$rank <- rownames(d)
      
      # Create the plot
      p <- plot_ly(d, x = ~prop, y = ~reorder(city, prop), 
                   type = "bar", orientation = "h", 
                   marker = list(color = custom_colors,
                                 hoverlabel = list(bgcolor = "lightblue",
                                                   font = list(color = "black"),
                                                   align = "left",
                                                   namelength = -1,
                                                   bordercolor = "black",
                                                   borderwidth = 1,
                                                   pad = list(10, 10))),
                   hovertemplate = '<span style="font-size: 30px;color: black;text-align: center;text-overflow: clip;">Rank: %{customdata}</span><br><br><span style="font-size: 30px;color: black;text-align: center;text-overflow: clip;">%{y}</span><br><extra></extra>',
                   customdata = ~rank) %>% 
        layout(
          title = "Properties by City",
          xaxis = list(title = "Number of properties"),
          yaxis = list(title = ""),
          height = 320,
          width = 591,
          plot_bgcolor = "#1a1a40b5",
          paper_bgcolor = "#1a1a40b5",
          font = list(color = 'white'),
          xaxis = list(showgrid = FALSE),
          yaxis = list(showgrid = FALSE)
        )
      
      p %>% layout(yaxis = list(showticklabels = FALSE))
    }) 
    
    
    
    output$hover_info <- renderUI({
      if(isTRUE(input[["hovering"]])){ 
        style <- paste0("left: ", input[["left_px"]] + 10 + 3, "px;", # 10 = border-width after
                        "top: ", input[["top_px"]] - 40 - 11 - 4) # 40 = line-height/2 * number of lines; 11 = padding; 4 = border thickness
        div(
          class = "arrow_box", style = style,
          p(HTML(paste0("<b> mpg: </b>", input$dx, "<br/>",
                        "<b> wt: </b>", input$dy, "<br/>",
                        "<b> name: </b>", input$dname, "<br/>",
                        input$dtext)), 
            style="margin: 0; padding: 11px; line-height: 16px;")
        )
    
    
    
       
    output$infoboxH<-renderUI({
      infobox <- column( style='width:150px;
                    height: 60px;
                    background: aqua;
                    margin-top: 9px;
                    margin-left: -28px;; 
                   ' , 
                         
                         width = 2,
                         h4("Total of properties " , style='font-size: 10px; 
                                  margin-top: 3px;
                                  text-align: center;
                                  font-size: 12px;
                                  margin-bottom: -2px;'), 
                         
                         h3(paste0(as.character(round(nrow(data)))) ,style='font-size: 28px;
                                margin-bottom: 12px; 
                                margin-top: 2px;
                                text-align: center;' )) }) 
    output$infoboxPK<-renderUI({
      infobox <- column( style='width:150px;
                    height: 60px;
                    background: aqua;
                    margin-top: 9px;
                    margin-left: 30px; 
                   ' , 
                         
                         width = 1,
                         h4("AVG rent per m²" , style='font-size: 6px; 
                                  margin-top: 3px;
                                  text-align: center;
                                  font-size: 12px;
                                  margin-bottom: -2px;'), 
                         
                         h3(paste0(as.character(round(mean(data$rent/data$areaSqm)))," €") ,style='font-size: 28px;
                                margin-bottom: 12px; 
                                margin-top: 2px;
                                text-align: center;' )) }) 
    
    output$pricePlot <- renderPlotly({
      d <- data %>% 
        group_by(city) %>% 
        summarise(rent = round(mean(rent))) %>% 
        arrange(desc(rent))
      d <- head(d, 10) 
      
      custom_colors <- rev(pal_material("light-blue")(10))
      
      p <- plot_ly(d, x = ~rent, y = ~reorder(city, rent) , text=~city,
                   marker=list(color=custom_colors),
                   type = "bar", orientation = "h") %>%
        layout(
          title = "",
          xaxis = list(title = "Average Price"),
          yaxis = list(title = ""),
          height = 300,
          width = 580,
          plot_bgcolor = "#ffffff",
          paper_bgcolor = "#ffffff",
          font = list(color = '#202020'),  # Set font color to red
          xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
          yaxis = list(showgrid = FALSE)  # Remove y-axis gridlines
        )
      
      p %>% layout(yaxis= list(showticklabels = FALSE))
    

      
    })
    
    
    output$livebar <- renderPlotly({
      d <- data %>% 
        group_by(living) %>% 
        summarise(count = n()) %>% 
        arrange(desc(count))
      
      custom_colors <- c('#5389bd', '#5aa3db', '#82b8dc' ,'#82b8dc','#82b8dc')
      
      p <- plot_ly(d, x = ~reorder(living, count), y = ~count,
                   marker = list(color = custom_colors),
                    type = "bar", orientation = "v") %>%
        layout(
          title = "",
          xaxis = list(title = ""),
          yaxis = list(title = "Properties"),
          height = 250,
          width = 223,
          plot_bgcolor = "#ffffff",
          paper_bgcolor = "#ffffff",
          font = list(color = '#202020'),  # Set font color to red
          showlegend = FALSE,  # Remove legend
          xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
          yaxis = list(showgrid = FALSE)  # Remove y-axis gridlines
        )
      
      p
    })
    
    
    
    output$prop <- renderPlotly({
      d <- data %>% 
        group_by(city) %>% 
        summarise(prop = n()) %>% 
        arrange(desc(prop))
      d <- head(d, 10) 
      custom_colors <- rev(pal_material("light-blue")(10))
      p <- plot_ly(d, x = ~prop, y = ~reorder(city, prop), 
                   type = "bar", orientation = "h", 
                   marker=list(color=custom_colors),
                   text = ~city) %>%
        layout(
          title = "Properties by City",
          xaxis = list(title = "Number of properties"),  # Remove x-axis tick labels
          yaxis = list(title = ""),
          height = 300,
          width = 580,
          plot_bgcolor = "#ffffff",
          paper_bgcolor = "#ffffff",
          font = list(color = '#202020'),  # Set font color to white
          xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
          yaxis = list(showgrid = FALSE)  # Remove y-axis gridlines
        )
      
      p %>% layout(yaxis= list(showticklabels = FALSE))
    })
    
  
    output$pieChart <- renderPlotly({
      # Sample data for the pie chart
      d = data %>% group_by(internet) %>% summarise(count1=n()) 
      custom_colors <- c('#5389bd', '#5aa3db','#ea6947', '#82b8dc', '#0a497347')
      # Create the pie chart using Plotly
      plot_ly(d, labels = ~internet, values = ~count1,marker = list(colors = custom_colors) , type = "pie", hole = 0.65) %>%
        layout(height = 250, width = 230, 
               plot_bgcolor = "#ffffff",
               paper_bgcolor = "#ffffff",
               font = list(color = '#202020'),  # Set font color for better visibility
               xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
               yaxis = list(showgrid = FALSE),
               legend = list(orientation = 'h')# Add legend at the bottom
        )   
    })
    
    output$pieChartF <- renderPlotly({
      # Sample data for the pie chart
      d <- data %>% group_by(furnish) %>% summarise(count1 = n()) 
      custom_colors <- c('#5389bd', '#5aa3db','#ea6947', '#82b8dc', '#0a497347')
      # Create the donut chart using Plotly
      plot_ly(d, labels = ~furnish, values = ~count1,marker = list(colors = custom_colors)  , type = "pie" , hole = 0.65) %>%
        layout(height = 250, width = 310, 
               plot_bgcolor = "#ffffff",
               paper_bgcolor = "#ffffff",
               font = list(color = '#202020'),  # Set font color for better visibility
               xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
               yaxis = list(showgrid = FALSE),
               legend = list(orientation = 'h')# Add legend at the bottom
        ) 
    }) 
    
    
    
    
    
    
    output$forTable <- renderFormattable({
      d = data %>% group_by(furnish , internet , living) %>%
        summarise(rent=round(mean(rent),2)) %>% arrange(desc(rent))
      
      
      formattable(d, 
                  align = c("l",rep("r", NCOL(data) - 1)),
                  list( 
                    rent = color_bar("#ea6947") 
                  )) 
      
    })
    
    output$priceDistribution <- renderPlotly({
      plot_ly(data, x = ~rent, color = ~furnish,  mode = "lines") %>%
        layout(title = "",
               xaxis = list(title = "Price"),
               yaxis = list(title = "Type"),  
               height = 250, width = 310, 
               plot_bgcolor = "#ffffff",
              paper_bgcolor = "#ffffff",
      font = list(color = '#202020'),  # Set font color for better visibility
      xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
      yaxis = list(showgrid = FALSE),
      legend = list(orientation = 'h')) 
    })
    
    
    
    
    output$priceDifference <- renderPlotly({
      
      
      d3<-data %>% filter(furnish == 'Furnished') %>% 
        select(furnish , rent) 
      
      d31<-data %>% filter(furnish != 'Furnished') %>% 
        select(furnish , rent) 
      
      difference<-round(mean( d3$rent - d31$rent),3) 
      
      fig <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        value = difference,
        title = list(text = "", font = list(size = 24)),
        
        gauge = list(
          axis = list(range = list(NULL, 500), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          bgcolor = "white",
          borderwidth = 2,
          bordercolor = "gray",
          
          steps = list(
            list(range = c(0, 250), color = '#82b8dc'),
            list(range = c(250, 400), color = '#5aa3db')),
          threshold = list(
            line = list(color = "#ea6947", width = 4),
            thickness = 0.75,
            value = 490))) 
      fig <- fig %>%
        layout( 
          height = 145, width = 200, 
          margin = list(l=20,r=30),
          paper_bgcolor = "white",
          font = list(color = "black", family = "Arial"))
      
      fig
    })
    
    output$priceDifference2 <- renderPlotly({
      
      
      d3<-data %>% filter(internet == 'Yes') %>% 
        select(internet , rent) 
      
      d31<-data %>% filter(furnish != 'No') %>% 
        select(internet , rent) 
      
      difference<-round(mean( d3$rent - d31$rent),3) 
      
      fig <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        value = difference,
        title = list(text = "", font = list(size = 24)),
        
        gauge = list(
          axis = list(range = list(NULL, 500), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          bgcolor = "white",
          borderwidth = 2,
          bordercolor = "gray",
          steps = list(
            list(range = c(0, 250), color = '#82b8dc'),
            list(range = c(250, 400), color = '#5aa3db')),
          threshold = list(
            line = list(color = "#ea6947", width = 4),
            thickness = 0.75,
            value = 490))) 
      fig <- fig %>%
        layout( 
          height = 145, width = 200, 
          margin = list(l=20,r=30),
          paper_bgcolor = "white",
          font = list(color = "black", family = "Arial"))
      
      fig
    })
    
    
    
   output$rentApp<- renderPlotly({
    
    data =data %>% group_by(smokingInside ,furnish ) %>% summarise(rent=mean(rent))
    
    
    custom_colors <- c('#5389bd', '#5aa3db', '#82b8dc', '#0a497347')
    
    fig <- plot_ly(data , x = ~smokingInside, y = ~rent , type = 'bar',
                   color = ~furnish, colors = custom_colors)
    
    # Customize the chart layout
    fig <- fig %>% layout(title = "Average Size of Apartments by City",
                          xaxis = list(title = "Apartment Type"),
                          yaxis = list(title = "Average Size (sq. ft)"),
                          barmode = "group")
    
    # Display the chart
    fig %>% layout(title = "",
               xaxis = list(title = "Price"),
               yaxis = list(title = "Type"),  
               height = 200, width = 428, 
               plot_bgcolor = "#ffffff",
              paper_bgcolor = "#ffffff",
      font = list(color = '#202020'),  # Set font color for better visibility
      xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
      yaxis = list(showgrid = FALSE),
      legend = list(orientation = 'h')) 
    })
    
    
 }  
          }) 
  
    output$pieChart <- renderPlotly({
      # Sample data for the pie chart
      d = data %>% group_by(internet) %>% summarise(count1=n()) 
      custom_colors <- c('#5389bd', '#5aa3db','#ea6947', '#82b8dc', '#0a497347')
      # Create the pie chart using Plotly
      plot_ly(d, labels = ~internet, values = ~count1,marker = list(colors = custom_colors) , type = "pie", hole = 0.65) %>%
        layout(height = 250, width = 230, 
               plot_bgcolor = "#1a1a40b5",
               paper_bgcolor = "#1a1a40b5",
               font = list(color = 'white'),  # Set font color for better visibility
               xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
               yaxis = list(showgrid = FALSE),
               legend = list(orientation = 'h')# Add legend at the bottom
        ) %>% layout(title =list(text='Properties with Internet', y = 0.99, x = 0.40, xanchor = 'center', yanchor =  'top'), font=list(size = 11))   
    })
    
    output$pieChartF <- renderPlotly({
      # Sample data for the pie chart
      d <- data %>% group_by(furnish) %>% summarise(count1 = n()) 
      custom_colors <- c('#5389bd', '#5aa3db', '#82b8dc', '#0a497347')
      # Create the donut chart using Plotly
      plot_ly(d, labels = ~furnish, values = ~count1,marker = list(colors = custom_colors)  , type = "pie" , hole = 0.65) %>%
        layout(height = 250, width = 310,  
               
               plot_bgcolor = "#1a1a40b5",
               paper_bgcolor = "#1a1a40b5",
               font = list(color = 'white'),  # Set font color for better visibility
               xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
               yaxis = list(showgrid = FALSE),
               legend = list(orientation = 'h')# Add legend at the bottom
        ) %>% layout(title =list(text='Furnished properties', y = 0.99, x = 0.40, xanchor = 'center', yanchor =  'top'), font=list(size = 11))
    }) 
    
    
    output$forTable <- renderFormattable({
      d = data %>% group_by( propertyType ,furnish , internet) %>%
        summarise(rent=round(mean(rent),2)) %>% arrange(desc(rent)) 
      
      
      formattable(d, 
                  align = c("l",rep("r", NCOL(data) - 1)),
                  list( 
                    rent = color_bar("#0996ed") 
                  )) 
      
    })
    
    
    output$forTable2 <- renderFormattable({
      d <- filteredData() %>%
        group_by(propertyType, furnish, internet) %>%
        summarise(rent = round(mean(rent), 2)) %>%
        arrange(desc(rent))
      
      formattable(d, 
                  align = c("l", rep("r", ncol(d) - 1)),
                  list(
                    rent = color_bar("#0996ed")
                  ))
    }) 
    
    
    
    
    
    output$priceDistribution <- renderPlotly({
      plot_ly(data, x = ~rent, color = ~furnish,  mode = "lines") %>%
        layout(title = "",
               xaxis = list(title = "Price"),
               yaxis = list(title = "Type"),  
               height = 250, width = 310, 
               plot_bgcolor = "#ffffff",
              paper_bgcolor = "#ffffff",
      font = list(color = '#202020'),  # Set font color for better visibility
      xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
      yaxis = list(showgrid = FALSE),
      legend = list(orientation = 'h')) 
    })
    
    output$priceDifference <- renderPlotly({
      
      
      d3<-data %>% filter(furnish == 'Furnished') %>% 
        select(furnish , rent) 
      
      d31<-data %>% filter(furnish != 'Furnished') %>% 
        select(furnish , rent) 
      
      difference<-round(mean( d3$rent - d31$rent),3) 
      
      fig <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        value = difference , 
        title = list(text = "", font = list(size = 10)),
        
        gauge = list(
          axis = list(range = list(NULL, 500), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          bgcolor = "white",
          borderwidth = 2,
          bordercolor = "gray",
          
          steps = list(
            list(range = c(0, 250), color = '#82b8dc'),
            list(range = c(250, 400), color = '#5aa3db')),
          threshold = list(
            line = list(color = "#ea6947", width = 4),
            thickness = 0.75,
            value = 490))) 
      fig <- fig %>%
        layout( 
          height = 145, width = 325, 
          margin = list(l=20,r=30),
          paper_bgcolor = "#1A1A40",
          font = list(color = 'white', family = "Arial"))
      
      fig  %>% layout(title =list(text='Average monthly furniture cost(Euro)', y = 0.20, x = 0.50, xanchor = 'center', yanchor =  'top'), font=list(size = 11))
    })
    
#plot_bgcolor = "#1A1A40",
#paper_bgcolor = "#1A1A40",
#font = list(color = 'white')
    
    output$priceDifference2 <- renderPlotly({
      
      
      d3<-data %>% filter(internet == 'Yes') %>% 
        select(internet , rent) 
      
      d31<-data %>% filter(internet != 'Yes') %>% 
        select(internet , rent) 
      
      difference<-round(mean( d3$rent - d31$rent),3) 
      
      fig <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        value = difference,
        title = list(text = "", font = list(size = 24)),
        
        gauge = list(
          axis = list(range = list(NULL, 500), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          bgcolor = "white",
          borderwidth = 2,
          bordercolor = "gray",
          steps = list(
            list(range = c(0, 250), color = '#82b8dc'),
            list(range = c(250, 400), color = '#5aa3db')),
          threshold = list(
            line = list(color = "#ea6947", width = 4),
            thickness = 0.75,
            value = 490))) 
      fig <- fig %>%
        layout( 
          height = 145, width = 200, 
          margin = list(l=20,r=30),
          paper_bgcolor = "#1A1A40",
          font = list(color = "white", family = "Arial"))
      
      fig
    })
    
   output$rentApp<- renderPlotly({
    
    data =data %>% group_by(smokingInside ,furnish ) %>% summarise(rent=mean(rent))
    
    
    custom_colors <- c('#5389bd', '#5aa3db', '#82b8dc', '#0a497347')
    
    fig <- plot_ly(data , x = ~smokingInside, y = ~rent , type = 'bar',
                   color = ~furnish, colors = custom_colors)
    
    # Customize the chart layout
    fig <- fig %>% layout(title = "Average Size of Apartments by City",
                          xaxis = list(title = "Apartment Type"),
                          yaxis = list(title = "Average Size (sq. ft)"),
                          barmode = "group")
    
    # Display the chart
    fig %>% layout(title = "",
               xaxis = list(title = "Price"),
               yaxis = list(title = "Type"),  
               height = 230, width = 340, 
               plot_bgcolor = "#1A1A40",
               paper_bgcolor = "#1A1A40",
               font = list(color = 'white'),  # Set font color for better visibility
      xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
      yaxis = list(showgrid = FALSE),
      legend = list(orientation = 'h')) 
    })
    
     
   
   
  #----- City level ---- 
   
   
   output$citybar <- renderPlotly({
     plot_data <- filteredData() %>%
       group_by(propertyType, furnish) %>%
       summarize(avg_rent = mean(rent),
                 .groups = "drop")
     
     # Define custom colors
     custom_colors <- c('#5389bd', '#5aa3db', '#11425a', '#52c9d7', '#82b8dc')
     
     # Create the plot
     plot <- plot_ly(plot_data, x = ~propertyType, y = ~avg_rent, color = ~furnish, 
                     colors = custom_colors, type = "bar" )
     
     # Add labels and title
     plot <- plot %>% layout(xaxis = list(title = ""),
                             yaxis = list(title = "Average Rent Price"),
                             title = "Average Rent Price per Apartment Type (Divided by Furnish)",
                             height = 320,
                             width = 590,
                             plot_bgcolor = "#1a1a40b5",
                             paper_bgcolor = "#1a1a40b5",
                             font = list(color = 'white'),
                             xaxis = list(showgrid = FALSE),
                             yaxis = list(showgrid = FALSE) ,  
                             legend = list(orientation = 'h'), 
                             colorway = custom_colors )
     
     # Display the plot
     plot
   })
   
   
   output$pieChartC <- renderPlotly({
     # Sample data for the pie chart
     d = filteredData() %>% group_by(internet) %>% summarise(count1=n()) 
     custom_colors <- c('#5389bd', '#5aa3db','#ea6947', '#82b8dc', '#0a497347')
     # Create the pie chart using Plotly
     plot_ly(d, labels = ~internet, values = ~count1,marker = list(colors = custom_colors) , type = "pie", hole = 0.65) %>%
       layout(height = 320, width = 280, 
              plot_bgcolor = "#1a1a40b5",
              paper_bgcolor = "#1a1a40b5",
              font = list(color = 'white'),  # Set font color for better visibility
              xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
              yaxis = list(showgrid = FALSE),
              legend = list(orientation = 'h')# Add legend at the bottom
       ) %>% layout(title =list(text='Properties with Internet Share', y = 0.99, x = 0.40, xanchor = 'center', yanchor =  'top'), font=list(size = 11))   
   })
   
   output$pieChartFC <- renderPlotly({
     # Sample data for the pie chart
     d <- filteredData() %>% group_by(smokingInside) %>% summarise(count1 = n()) 
     custom_colors <- c('#5389bd', '#5aa3db', '#82b8dc', '#0a497347')
     # Create the donut chart using Plotly
     plot_ly(d, labels = ~smokingInside, values = ~count1,marker = list(colors = custom_colors)  , type = "pie" , hole = 0.65) %>%
       layout(height = 320, width = 310,  
              
              plot_bgcolor = "#1a1a40b5",
              paper_bgcolor = "#1a1a40b5",
              font = list(color = 'white'),  # Set font color for better visibility
              xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
              yaxis = list(showgrid = FALSE),
              legend = list(orientation = 'h')# Add legend at the bottom
       ) %>% layout(title =list(text='Allow smoking share', y = 0.99, x = 0.40, xanchor = 'center', yanchor =  'top'), font=list(size = 11))
   })   
   

   output$accommodationMap <- renderLeaflet({
     filteredData <- filteredData()  # Assuming you have a filteredData reactive object
     
     # Calculate the mean latitude and longitude
     mean_lat <- mean(filteredData$latitude)
     mean_lng <- mean(filteredData$longitude)
     
     color_palette <- colorNumeric(palette = pal_material("light-blue")(10),
                                   domain = filteredData$rent )
     
     
     # Create a Leaflet map
     map <- leaflet() %>%
       addProviderTiles(providers$CartoDB.Positron) %>% 
       setView(lng = mean_lng, lat = mean_lat, zoom = 10)  # Set the view based on mean latitude and longitude
     
     # Define rent categories and colors
    
     
     # Create a color palette based on rent categories
     #color_palette <- colorFactor(palette = rent_colors, domain = rent_categories)
     
     # Add markers to the map with different colors based on rent category
     map <- map %>%
       addCircleMarkers(data = filteredData, lng = ~longitude, lat = ~latitude,
                        color = ~color_palette(rent) ,
                        radius = 5, stroke = FALSE, 
                        fill = TRUE,
                        fillOpacity = 0.7,
                        label = ~paste("Rent Price: €", rent)) %>%  
       addLegend(
         position = "bottomright",
         pal = color_palette,
         values = c(min(filteredData$rent), max(filteredData$rent)),
         title = "Rent Price",
         labFormat = labelFormat(suffix = " €"),
         opacity = 0.7
       )
       
        
     
     # Display the map
     map
   })
      
   output$pricePlot2 <- renderPlotly({
     
     
     # Create the histogram
     plot <- plot_ly(filteredData(), x = ~rent, type = "histogram")
     
     # Add title and axis labels
     plot <- plot %>% layout(title = "Price Distribution of Rent",
                            
                             xaxis = list(title = "Price"),
                               
                             height = 230, width = 340, 
                             plot_bgcolor = "#1A1A40",
                             paper_bgcolor = "#1A1A40",
                             font = list(color = 'white'),  # Set font color for better visibility
                             xaxis = list(showgrid = FALSE),  # Remove x-axis gridlines
                             yaxis = list(showgrid = FALSE),
                             legend = list(orientation = 'h')) 
  
       
     
     # Return the plot
     plot %>% layout(xaxis = list(showticklabels = FALSE))
   })

   output$city <- renderText({
     text <- filteredData()$city[1] 
     
   } 
     
   )
   
    output$infoboxPC<-renderUI({
         filteredData <- filteredData()
        infobox <- column( style='width:fit-content;
                    height: 60px;
                    background: #f1ffff;
                    margin-top: 9px;
                    margin-left: 60px; 
                   ' , 
                           
                           width = 2,
                           h4("AVG Rent Price" , style='font-size: 10px; 
                                  margin-top: 3px;
                                  text-align: center;
                                  font-size: 12px;
                                  margin-bottom: -2px;'), 
                           
                           h3(paste0(as.character(round(mean(filteredData$rent))) ,' €') ,style='font-size: 28px;
                                margin-bottom: 12px; 
                                margin-top: 2px;
                                text-align: center;' )) }) 
        
    output$infoboxHC<-renderUI({
      filteredData <- filteredData() 
      infobox <- column( style='width:150px;
                    height: 60px;
                    background: #f1ffff;
                    margin-top: 9px;
                    margin-left: -28px;; 
                   ' , 
                         
                         width = 2,
                         h4("Total of properties " , style='font-size: 10px; 
                                  margin-top: 3px;
                                  text-align: center;
                                  font-size: 12px;
                                  margin-bottom: -2px;'), 
                         
                         h3(paste0(as.character(round(nrow(filteredData)))) ,style='font-size: 28px;
                                margin-bottom: 12px; 
                                margin-top: 2px;
                                text-align: center;' )) }) 
 
    
    output$infoboxPKC<-renderUI({
      
      filteredData <- filteredData()
      
      infobox <- column( style='width:150px;
                    height: 60px;
                    background: #f1ffff;
                    margin-top: 9px;
                    margin-left: 30px; 
                   ' , 
                         
                         width = 1,
                         h4("AVG rent per m²" , style='font-size: 6px; 
                                  margin-top: 3px;
                                  text-align: center;
                                  font-size: 12px;
                                  margin-bottom: -2px;'), 
                         
                         h3(paste0(as.character(round(mean(filteredData$rent/filteredData$areaSqm)))," €") ,style='font-size: 28px;
                                margin-bottom: 12px; 
                                margin-top: 2px;
                                text-align: center;' )) })  
   
}     




