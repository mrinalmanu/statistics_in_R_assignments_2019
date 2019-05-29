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
library(tidyr)
library(dplyr)

data <-  mtcars
data[c(2, 8:11)] <- lapply(data[c(2, 8:11)], factor)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
     # Sidebar: for input here
      sidebarPanel(
        
        # select data for x-axis
        
         textInput(inputId = 'title',
                  label = 'Plot title',
                  placeholder = 'Enter the title'),
        
         selectInput(inputId = 'x',
                     label = 'X-axis',
                     choices = c('Miles/(US) gallon'="mpg",
                                 'Displacement (cu.in.)'="disp",
                                 'Gross horsepower'="hp",
                                 'Rear axle ration'="drat",
                                 'Weight (1000 lbs)'="wt",
                                 '1/4 mile time'="qsec"),
                     selected = 'mpg'),
         
        # select data for y-axis 
         selectInput(inputId = 'y', 
                     label = 'Y-axis',
                     choices = c('Miles/(US) gallon'="mpg",
                                 'Displacement (cu.in.)'="disp",
                                 'Gross horsepower'="hp",
                                 'Rear axle ration'="drat",
                                 'Weight (1000 lbs)'="wt",
                                 '1/4 mile time'="qsec"),
                     selected = 'disp'),
        
        selectInput(inputId = 'colour', 
                    label = 'Color by',
                    choices = c('Number of cylinders' = 'cyl',
                                'Engine type' = 'vs',
                                'Transmission' = 'am',
                                'Number of forward gears' = 'gear',
                                'Number of carburators' = 'carb'),
                    selected = 'Number of cylinders'),
        
        sliderInput(inputId = 'alpha',
                    label = 'Alpha',
                    min = 0, max = 1,
                    value = 0.5),
        
        numericInput(inputId = 'size',
                     label = "Dot size",
                     value = 3,
                     min = 1, max = 10),
                     
        
        checkboxInput(inputId = 'show_d',
                      label = 'Show data:',
                      value = FALSE),
        
        checkboxInput(inputId = 'show_s',
                      label = 'Show summary:',
                      value = FALSE),
        
        
        actionButton("appl","Apply"),
        actionButton("save","Save plot"),
        helpText("Note: title name will be used as file name")
        
      ),
   
      # Show a plot of the generated distribution
      mainPanel(
        
        # Plot panel: for outputs here
         plotOutput("scatter"),
         
         fluidRow(
           column(width = 6,
                  tableOutput(outputId = "selected_data")),
           
           column(width = 6,
                  tableOutput(outputId = "summary"))
         )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$scatter <- renderPlot({
     ggplot(data, aes_string(x = input$x, y = input$y, col = input$colour)) +
       geom_point(alpha = input$alpha, size = req(input$size)) +
       ggtitle(tools::toTitleCase(isolate(input$title)))
     })
   
   output$selected_data <- renderTable({
     if(input$show_d) data %>% select_(input$x, input$y, input$colour)
     })
   
   output$summary <- renderTable({
     if(input$show_s){
       data %>% select_(input$x, input$y, input$colour) %>% 
         group_by_(input$colour) %>% 
         summarise_all(funs(mean, sd))
     }
   })
   
   new_data <- reactive({
     data %>% select_(input$x, input$y, input$colour) 
   })
   output$selected_data <- renderTable({
     if(input$show_d){
       new_data() 
     } 
   })
   
   output$summary <- renderTable({
     if(input$show_s){
       new_data %>% 
         group_by_(input$colour) %>% 
         summarise_all(funs(mean, sd))
     }
   }) 
} 

# Run the application 
shinyApp(ui = ui, server = server)

