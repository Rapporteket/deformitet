library(shiny)
library(ggplot2)
library(dplyr)

# Neste steg:
# Legge inn kommentarer og forklaringer på funksjonene
# Få til andelstabeller og vise dem
# Legge inn moduler
# Vi må lage samlerapport - tekst til dette
# Kort forklaring om deformitetsregisteret

regdata <- deformitet::les_og_flate_ut()

ui <- fluidPage(

  sidebarLayout(

    # Inputs: Select variables to plot
    sidebarPanel(

      # Select variable for x-axis
      selectInput(
        inputId = "x_var",
        label = "Variabel:",
        choices = c("BMI-kategori" = "BMI_CATEGORY"),
        selected = "BMI_CATEGORY")),

      # # Select color
      # selectInput(
      #   inputId = "color",
      #   label = "Color by:",
      #   choices = c("Title type" = "title_type",
      #               "Genre" = "genre",
      #               "MPAA rating" = "mpaa_rating",
      #               "Critics rating" = "critics_rating",
      #               "Audience rating" = "audience_rating"),
      #   selected = "audience_rating"
      # ),

    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "barplot"))
    )
  )
# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$barplot <- renderPlot({
    deformitet::gg_makeHist(regdata, input$x_var, input$x_var, input$x_var, input$x_var)
  })
}
#
# navn = gg_makeHist2(regdata, BMI_CATEGORY)
# navn + xlab("BMI")+
#   ggtitle("Fordeling av")+
#   theme(plot.title = element_text(size = 14, face ="bold", hjust = 0.5 , vjust = 1.5))

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
