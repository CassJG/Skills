#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)
library(ggplot2)
library(tidyr)
library(stringr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Blood Transfusion Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dropdown", label = "Select an option:", 
                  choices = c("Recency", "Frequency", "Monetary", "Time"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "density")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$density <- renderPlot({
    transfusion <- read.csv(here::here("transfusion.data"), header = TRUE)
    colnames(transfusion) <- c("Recency", "Frequency", "Monetary", "Time", "Donated_March_2007")
    transfusion$Donated_March_2007 <- ifelse(transfusion$Donated_March_2007 == 1, "Yes", "No")
    transfusion$Donated_March_2007 <- factor(transfusion$Donated_March_2007, levels = c("Yes","No"))
    
  
    title_of_plot <- switch(input$dropdown,
                            "Recency" = "Months Since Last Donation Based on March 2007 Donation",
                            "Frequency" = "Frequency of Donations Based on March 2007 Donation",
                            "Monetary" = "Monetary Frequency Based on March 2007 Donation",
                            "Time" = "Months Since First Donation Based on March 2007 Donation")
    
    
    
    
    ggplot(data = transfusion, aes_string(x = input$dropdown, group = "Donated_March_2007", fill= "Donated_March_2007")) +
      geom_density(adjust=1.5, alpha=.5) +
      scale_fill_manual(values = c("red", "pink")) +
      labs(x = input$dropdown, y = "Frequency", title = title_of_plot, legend = "Whether They Donated in March 2007") +
      theme_classic() +
      theme(axis.text = element_text(size = 14, family = "Times New Roman"),
           axis.title = element_text(size = 18, family = "Times New Roman"),
            plot.title = element_text(size = 18, family = "Times New Roman"),
            plot.subtitle = element_text(size = 13, face = "italic", family = "Times New Roman"),
            legend.text = element_text(size = 13, family = "Times New Roman"),
            legend.title = element_text(size = 15, family = "Times New Roman"))
    

    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)