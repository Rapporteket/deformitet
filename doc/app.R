library(shiny)
library(ggplot2)
library(dplyr)

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
        selected = "BMI_CATEGORY"
      ),
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
)
# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$barplot <- renderPlot({
    gg_makeHist2()################### hit er jeg kommet! Jeg legger in funksjonen her :)
    ggplot(data = movies, aes_string(x = input$x, y = input$y, color = input$z)) + # input list from the UI!
      geom_point(alpha = input$alpha)
  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
