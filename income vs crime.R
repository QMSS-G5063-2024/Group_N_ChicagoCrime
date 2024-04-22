library(shiny)
library(ggplot2)
library(dplyr)

# Load your data here
income_data <- read.csv("/Users/caraxu/Downloads/Per_Capita_Income.csv")
crime_data <- read.csv("/Users/caraxu/Downloads/Crimes_-_2022.csv")

# Print column names to verify
names(crime_data)
names(income_data)

# Merge data based on Community Area
merged_data <- merge(x = crime_data, y = income_data, by.x = "Community.Area", by.y = "Community.Area.Number", all = TRUE)

# Define UI
ui <- fluidPage(
  titlePanel("Crime and Socio-economic Analysis in Chicago"),
  sidebarLayout(
    sidebarPanel(
      selectInput("crimeType", "Choose a Crime Type:", choices = unique(merged_data$`Primary Type`))
    ),
    mainPanel(
      plotOutput("incomeCrimePlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$incomeCrimePlot <- renderPlot({
    # Filter and prepare the data
    filtered_data <- merged_data %>%
      filter(`Primary.Type` == input$crimeType) %>%
      group_by(`COMMUNITY.AREA.NAME`) %>%
      summarise(Crime_Count = n(), Avg_Income = mean(`PER.CAPITA.INCOME`, na.rm = TRUE)) %>%
      arrange(desc(Crime_Count)) %>%  # Order by descending crime count
      slice_head(n = 5) %>%  # Select the top 5 entries
      arrange(desc(Avg_Income))  # Now also order by descending average income
    
    # Generate the bar plot with income ordered from highest to lowest
    ggplot(filtered_data, aes(x = reorder(`COMMUNITY.AREA.NAME`, Avg_Income), y = Crime_Count, fill = Avg_Income)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "blue", high = "red") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = "Top 5 Community Areas by Crime Frequency", x = "Community Area", y = "Number of Crimes", fill = "Average Income")
  })
}




ui <- fluidPage(
  titlePanel("Crime and Socio-economic Analysis in Chicago"),
  sidebarLayout(
    sidebarPanel(
      selectInput("crimeType", "Choose a Crime Type:", choices = unique(merged_data$`Primary.Type`))
    ),
    mainPanel(
      plotOutput("incomeCrimePlot")
    )
  )
)


# Run the application 
shinyApp(ui = ui, server = server)
