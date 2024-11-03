library(shiny)
library(sf)
library(ggplot2)
library(bslib)

# Load the pre-downloaded cycling networks
load(file = "data/road_networks.RData")

# Initialize with a random city
current_city <- sample(names(road_networks), 1)

ui <- page(
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Itim"),
    "primary" = "#6C63FF"
  ),
  
  tags$head(
    tags$style(HTML("
      .game-title { 
        font-size: 2.5rem;
        text-align: center;
        margin-bottom: 1rem;
      }
      .choice-btn {
        font-size: 1.2rem !important;
        min-width: 150px;
      }
      .next-btn {
        position: absolute;
        right: 20px;
        top: 20px;
        z-index: 1000;
      }
      .score-box {
        position: absolute;
        left: 20px;
        top: 20px;
        z-index: 1000;
        background: white;
        padding: 10px 20px;
        border-radius: 10px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
    "))
  ),
  
  layout_column_wrap(
    width = "100%",
    height = "100%",
    fillable = FALSE,
    
    card(
      max_height = "100%",
      htmlOutput("gameTitle", class = "game-title"),
      
      # Score display
      div(
        class = "score-box",
        span("Score: "), 
        textOutput("score", inline = TRUE)
      ),
      
      # Next button
      input_task_button("nextCity", "Next Challenge →", 
                   class = "btn-secondary next-btn"),
      
      # Main map
      plotOutput("map", height = "500px"),
      
      # Choices as buttons
      layout_column_wrap(
        # width = 1/5,  # Changed to 1/5 for 5 cities
        style = "margin-top: 20px; text-align: center;",
        input_task_button("guess_1", "New York", 
                     class = "btn-primary choice-btn"),
        input_task_button("guess_2", "Amsterdam", 
                     class = "btn-primary choice-btn"),
        input_task_button("guess_3", "Toronto", 
                     class = "btn-primary choice-btn"),
        input_task_button("guess_4", "Tokyo", 
                     class = "btn-primary choice-btn"),
        input_task_button("guess_5", "Windsor", 
                     class = "btn-primary choice-btn")
      )
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(
    current_city = current_city,
    score = 0
  )
  
  # Game title with animation
  output$gameTitle <- renderUI({
    HTML('<div style="color: #6C63FF;">Road Network Detective</div>')
  })
  
  # Render the map
  output$map <- renderPlot({
    ggplot() +
      geom_sf(data = road_networks[[values$current_city]]$osm_lines, 
              color = "#6C63FF", size = 0.5) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#F8F9FA"),
        panel.background = element_rect(fill = "#F8F9FA")
      )
  })
  
  # Handle all guess buttons
  observeEvent(input$guess_1, { checkGuess("New York") })
  observeEvent(input$guess_2, { checkGuess("Amsterdam") })
  observeEvent(input$guess_3, { checkGuess("Toronto") })
  observeEvent(input$guess_4, { checkGuess("Tokyo") })
  observeEvent(input$guess_5, { checkGuess("Windsor") })
  
  # Check guess function
  checkGuess <- function(guess) {
    if (guess == values$current_city) {
      values$score <- values$score + 1
      showModal(modalDialog(
        title = "🎉 Correct!",
        "You've got a keen eye for transportation infrastructure!",
        footer = modalButton("Continue"),
        size = "s"
      ))
    } else {
      showModal(modalDialog(
        title = "❌ Not quite!",
        paste("Try again"),
        footer = modalButton("Try Again"),
        size = "s"
      ))
    }
  }
  
  # Next city
  observeEvent(input$nextCity, {
    values$current_city <- sample(names(road_networks), 1)
  })
  
  # Display score
  output$score <- renderText({
    values$score
  })
}

shinyApp(ui, server)
