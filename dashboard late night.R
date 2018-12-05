
library(DT)
library(shiny)
library(shinydashboard)
library(dplyr)

final_data <- read.csv("final_data.csv",header = T,stringsAsFactors = F)



ui <- dashboardPage(
  dashboardHeader(title = "Hello there"),
  dashboardSidebar( width = 300,
    sidebarMenu(id="hell" ,
      menuItem(strong(h4("Dine-In")), tabName = "Dinein"),
      
      menuItem(strong(h4("Delivery")), tabName = "Del"),
      tabItems(
        ###################      First tab content        #######################
        tabItem(tabName = "Dine-In",
                title = "Input",
                
                #######################  Slider input for PROXIMITY    ################################
                
                sliderInput(inputId = "proximity",
                            label = "Distance",
                            min = 0,max = 5,value = c(0,5),post = " Km"),
                
                #######################  Input for CUISINE    ################################
                
                checkboxGroupInput(inputId = "Cuisines",
                                   label = "Cuisines",
                                   choices = list("North Indian",
                                                  "Salad/sandwich"="Fast Food",
                                                  "Desserts"="Desserts"),
                                   selected = "North Indian")
        ),
        ##################   second tab content   #####################
        tabItem(tabName = "Del",
                tabsetPanel("tab",selected = "On call Delivery",
                tabPanel(title = "On call Delivery",value = "On call Delivery",
                         
                         checkboxGroupInput(inputId = "Cuisines2",
                                            label = "Cuisines",
                                            choices = list("North Indian",
                                                           "Salad/sandwich"="Fast Food",
                                                           "Desserts"="Desserts"),
                                            selected = "North Indian")),
                
                
                
                
                tabPanel(title = "App Delivery",value = "App delivery",
                         
                         selectInput(inputId = "apps",
                                     label = "Select one of the following",
                                     choices = c("Zomato","Swiggy")))
                         
                         
                )
                
        )))  
  ),
  
  dashboardBody(
    
    (h2("List of Restaurants:")),
    dataTableOutput("table")
    
  ) 
  
)



server<-function(input,output){
  observe({ input$hell
  #######################  filtering the data in output    ################################
  output$table<-renderDataTable({
    
    if(input$hell=="Dinein"){
    
    
    final_data %>% filter( 
                           Cuisines %in% input$Cuisines,
                           Radius>=input$proximity[1],              ######   proximity     ######
                           Radius<=input$proximity[2])%>% select(`Restaurant_Name`,
                                                                 Address,
                                                                 `Aggregate_Rating`,
                                                                 Phone)              ######   proximity     ######
    }else{
      final_data %>% filter(AppName==input$apps)%>% select(`Restaurant_Name`,Address,`Aggregate_Rating`)
    }
                    
       }, options = list(
         lengthMenu = c(5, 10, 15, 20)))
})
  # observe({input$hell
  #   output$table<-DT::renderDataTable({
  #     if(input$hell=="Del"){
  #       
  #       final_data %>% filter(Cuisines %>% input$Cuisines2) %>% select(`Restaurant Name`,Phone)
  #     }
  #     
  #   })
  #   
  #   
  #   
  # })
  
  
  
                            }


################       Shiny app       ###################
shinyApp(ui = ui,server = server)




