library(shiny)
library(tidyverse)
library(scales)
library(plotly)
#install.packages("viridis")

#load("X:/Xueyun.Sun/Advanced R/final project/final_data.RData")
load("final_data.RData")


#### Define UI for application that plots features of fake data ----------- ####
ui <- fluidPage(
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis
      textInput(inputId = "plot_title",
                label= "Enter Title Here:",
                placeholder = "Enter Text"),
      
      hr(), #Horizontal Line for visual separation
      
      
      selectInput(inputId = "Reg", 
                  label = "Region:",
                  choices = c("NORTHEAST", "MIDWEST", "SOUTH", "WEST"), 
                  selected = "NORTHEAST"),
      
      # Select Colors
      selectInput(inputId = "color_p", 
                  label = "Choose Point Color:",
                  choices = c("Red", "Blue", "Black", "Green"), 
                  selected = "Blue"),
      
      selectInput(inputId = "color_l", 
                  label = "Choose Line Color:",
                  choices = c("Red", "Blue", "Black", "Green","Orange","Purple"), 
                  selected = "Orange"),
      
      # Set alpha level
      sliderInput(inputId = "alpha", 
                  label = "Point Transparency:", 
                  min = 0, max = 1, 
                  value = 0.5),
      
      hr(), #Horizontal Line for visual separation
      
      # Set min/max of Cohort Values
      selectInput(inputId = "min", 
                  label = "Choose Cohort Range (Min):", 
                  choices = c(2002, 2003, 2004, 2005, 2006, 2007,2008,2009,2010,2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                  selected= 2002),
      
      selectInput(inputId = "max",
                  label = "Choose Cohort Range (Max):", 
                  choices = c(2002, 2003, 2004, 2005, 2006, 2007,2008,2009,2010,2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                  selected= 2018)
    ),
    
    # Output: Show scatterplot --------------------------------------
    mainPanel(
      tabsetPanel(
        tabPanel("RACE - HISPANIC", plotlyOutput(outputId = "scatterplot_H")),
        tabPanel("RACE - AISAN", plotlyOutput(outputId = "scatterplot_A")),
        tabPanel("RACE - BLACK", plotlyOutput(outputId = "scatterplot_B")),
        tabPanel("RACE - WHITE", plotlyOutput(outputId = "scatterplot_W")),
        
        tabPanel("Data",  DT::dataTableOutput(outputId="datasheet"))
      )
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  
  dat1 <- reactive({
    ds1 <- d[d$REGION %in% input$Reg, ]
    return(ds1)
  })
  
  
  # Create scatterplot object the plotOutput function is expecting --
  
  
  output$scatterplot_W<- renderPlotly({
    d1<-subset(dat1(), dat1()$RACE=="WHITE")
    ggplot(data =d1, aes_string(x = d1$YEAR, y = d1$MEDIAN_INCOME)) +
      geom_point(colour=input$color_p, alpha=input$alpha) + theme_classic() +
      scale_y_continuous(labels = dollar) + xlim(as.numeric(input$min), as.numeric(input$max)) +
      xlab("YEAR") +geom_line(colour="red")+ labs(title=input$plot_title)
  }) 
  
  output$scatterplot_A<- renderPlotly({
    d1<-subset(dat1(), dat1()$RACE=="ASIAN")
    ggplot(data =d1, aes_string(x = d1$YEAR, y = d1$MEDIAN_INCOME)) +
      geom_point(colour=input$color_p, alpha=input$alpha) + theme_classic() +
      scale_y_continuous(labels = dollar) + xlim(as.numeric(input$min), as.numeric(input$max)) +
      xlab("YEAR") +geom_line(colour=input$color_l)+ labs(title=input$plot_title)
  }) 
  
  
  output$scatterplot_B<- renderPlotly({
    d1<-subset(dat1(), dat1()$RACE=="BLACK")
    ggplot(data =d1, aes_string(x = d1$YEAR, y = d1$MEDIAN_INCOME)) +
      geom_point(colour=input$color_p, alpha=input$alpha) + theme_classic() +
      scale_y_continuous(labels = dollar) + xlim(as.numeric(input$min), as.numeric(input$max)) +
      xlab("YEAR") +geom_line(colour="green")+ labs(title=input$plot_title)
  }) 
  
  output$scatterplot_H<- renderPlotly({
    d1<-subset(dat1(), dat1()$RACE=="HISPANIC")
    ggplot(data =d1, aes_string(x = d1$YEAR, y = d1$MEDIAN_INCOME)) +
      geom_point(colour=input$color_p, alpha=input$alpha) + theme_classic() +
      scale_y_continuous(labels = dollar) + xlim(as.numeric(input$min), as.numeric(input$max)) +
      xlab("YEAR") +geom_line(colour=input$color_l)+ labs(title=input$plot_title)
  }) 
   
  output$datasheet<-DT::renderDataTable({
    DT::datatable(data=d[,1:4],
                  options=list(pageLength= 20),
                  rownames=FALSE)
  })
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
