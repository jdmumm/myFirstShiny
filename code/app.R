library(shiny)
library(tidyverse)

raw <- read.csv("../data/moose2021.csv")
raw %>% mutate(
  date = as.Date(paste0("2021/", dok), format = "%Y/%m/%d"))  %>%
  filter (killed == "Y", !is.na(dok), dok != "") -> dat

# Define UI
ui <- fluidPage(
  titlePanel("Moose Harvest by Date"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gmu", "GMU:", choices = sort(unique(dat$gmu)), selected = "15C")),
      selectInput("hunt", "Hunt:", choices = sort(unique(dat$hunt)), selected = "GM000")
      ),
    mainPanel(
      plotOutput("plot")
    )
  )


# Define server
server <- function(input, output) {
  
  # Filter data based on user input
  filt <- reactive({dat %>% filter(gmu == input$gmu & hunt == input$hunt) %>% 
    group_by(date) %>% summarize(
      nKills = n())
    }) 
  
  
  # Plot data
  output$plot <- renderPlot({
    filt_dat <- filt()
    p1 <- ggplot(data = filt_dat, aes(date,nKills))
    p1 + geom_smooth() + geom_point()
  })
}

# Run the app
shinyApp(ui = ui, server = server)