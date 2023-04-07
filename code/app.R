library(shiny)
library(tidyverse)

#raw <- read.csv("../data/moose2021.csv")
m21 <- read.csv("../data/moose2021.csv")
g21 <- read.csv("../data/goat2021.csv")
m21 %>% rbind(g21) -> raw

raw %>% mutate(
  date = as.Date(paste0("2021/", dok), format = "%Y/%m/%d"), 
  horn = horn * .01)  %>%
  filter (killed == "Y", !is.na(dok), dok != "") -> dat

# Define UI
ui <- fluidPage(
  titlePanel("2021 Alaska Moose and Goat Harvest Data"),
  sidebarLayout( 
    sidebarPanel(
      selectInput("species", "Species:", choices = sort(unique(dat$species)), selected = "Moose"),
      selectInput("gmu", "GMU:", choices = sort(unique(dat$gmu)), selected = "15C"),
      selectInput("hunt", "Hunt:", choices = NULL, selected = "GM000"), 
      checkboxInput("gmu_filter", "Filter By GMU", value = TRUE), 
      helpText("Note: Due to missing values, filtering by GMU may not always provide all harvests, especially for goats.
               However it is useful for general hunts such as GM000 because those hunts spans many GMUs."),
      sliderInput("bin",
                  "Bin Width:",
                  min = .1,
                  max = 4,
                  value = 1),
      HTML("<br> <a href='https://secure.wildlife.alaska.gov/index.cfm?fuseaction=harvest.lookup&_ga=2.240033411.107657813.1680839515-2080331412.1638856448'>Data from ADF&G</a>"),
    ),
    mainPanel(
      plotOutput("plot"),
      plotOutput("plot2")
    )
  ))


# Define server
server <- function(input, output, session) {
  
  # Filter data based on user input
  filt <- reactive({
    if(input$gmu_filter){
      dat %>% filter(gmu == input$gmu & hunt == input$hunt & species == input$species) %>% 
        group_by(date) %>% summarize(
          Kills = n())}
    else {
      dat %>% filter(hunt == input$hunt & species == input$species) %>% 
        group_by(date) %>% summarize(
          Kills = n())
    }
  }) 
  
  filt2 <- reactive({
    if(input$gmu_filter){
      dat %>% filter(gmu == input$gmu & hunt == input$hunt & species == input$species & horn > 1 & !is.na(horn))}
    else {
      dat %>% filter(hunt == input$hunt & species == input$species & horn > 1 & !is.na(horn))}
  }) 
  
  # Update "hunt" dropdown based on "species" selection
  observeEvent(input$species, {
    updateSelectInput(session, "hunt", 
                      choices = sort(unique(dat %>% 
                                              filter(species == input$species) %>% 
                                              pull(hunt))), 
                      selected = c("GM000", "DG360"))
  })
  
  # Plot kills vs date
  output$plot <- renderPlot({
    filt_dat <- filt()
    p1 <- ggplot(data = filt_dat, aes(date,Kills))
    p1 + geom_smooth() + geom_point() + labs(x = "Date", title = "Hunt Timing")
  })
  
  #plot Antler Spread / Horn Length
  output$plot2 <- renderPlot({
    filt_dat <- filt2()
    p2 <- ggplot(data = filt_dat, aes(x = horn))
    p2 + geom_histogram(binwidth = input$bin, aes (y = ..count..)) + 
      geom_density(bw = input$bin, color = "red", lwd = 1, aes( y = ..count..)) + 
      labs(title = "Antler Spread or Horn Length", x = "Inches", y = "Count")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
