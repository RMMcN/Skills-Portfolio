library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(scales)

# Load the data
emissions_file <- file.choose()  # Interactively choose the Carbon Emissions file
temperature_file <- file.choose()  # Interactively choose the Yearly Temp file

# Check if files exist
#if (!file.exists(emissions_file) || !file.exists(temperature_file)) {
  #stop("One or both files do not exist. Please check the file paths.")
#}

emissions <- read_excel(emissions_file)
temperature <- read_excel(temperature_file)

# Clean the emissions data to keep only the relevant records and sum by year
emissions_filtered <- emissions %>%
  filter(Record == "PBA_GgCO2") %>%
  gather(key = "Year", value = "Total_Emissions", -Country, -Record) %>%
  mutate(Total_Emissions = as.numeric(Total_Emissions), Year = as.integer(Year)) %>%
  group_by(Year) %>%
  summarize(Total_Emissions = sum(Total_Emissions, na.rm = TRUE))

# Clean the temperature data
temperature_clean <- temperature %>%
  rename(Year = Years, Average_Global_Temp = Temps) %>%
  mutate(Average_Global_Temp = as.numeric(Average_Global_Temp), Year = as.integer(Year))

# Merge the data on Year
df <- merge(emissions_filtered, temperature_clean, by = "Year")

# Print df to debug
print("Data Frame df:")
print(head(df))

# Calculate 5-year averages
df_avg <- df %>%
  mutate(Year_Group = (Year - 1970) %/% 5) %>%
  group_by(Year_Group) %>%
  summarize(
    Avg_Total_Emissions = mean(Total_Emissions, na.rm = TRUE),
    Avg_Global_Temp = mean(Average_Global_Temp, na.rm = TRUE)
  ) %>%
  mutate(Year = Year_Group * 5 + 1970)

# Print df_avg to debug
print("Data Frame df_avg:")
print(head(df_avg))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Carbon Emissions and Temperature Analysis"),
  dashboardSidebar(),
  dashboardBody(
    tabsetPanel(
      tabPanel("Research", 
               h3("Research"),
               p("This section provides an introduction to the data and analysis."),
               p("We analyze the relationship between carbon emissions and global temperature from 1970 to 2015.")
      ),
      tabPanel("Requirements", 
               h3("Requirements"),
               p("This section describes the methodology used to clean and analyze the data."),
               p("Data from two sources were merged, and calculations such as yearly totals and 5-year averages were performed.")
      ),
      tabPanel("Total Emissions by Year", 
               plotOutput("total_emissions_by_year")
      ),
      tabPanel("Average Global Temperature by Year", 
               plotOutput("avg_temp_by_year")
      ),
      tabPanel("Correlation Plot", 
               plotOutput("correlation_plot")
      ),
      tabPanel("5-Year Average Correlation Plot", 
               plotOutput("five_year_avg_correlation_plot")
      ),
      tabPanel("Backlog", 
               h3("Backlog"),
               p("This section summarizes the findings and implications of the analysis."),
               p("The correlation between carbon emissions and global temperature over the years was explored, providing insights into their relationship.")
      ),
      tabPanel("Conclusion", 
               h3("Conclusion"),
               p("This section suggests areas for further research based on the analysis."),
               p("Additional factors influencing global temperature and more granular data analysis can be considered in future studies.")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$total_emissions_by_year <- renderPlot({
    ggplot(data = df, aes(x = Year, y = Total_Emissions)) +
      geom_bar(stat = "identity", position = "dodge", fill = "#ffd700") +
      labs(title = "Total Emissions by Year", x = "Year", y = "Total Emissions (Gg CO2)") +
      scale_y_continuous(labels = comma)
  })
  
  output$avg_temp_by_year <- renderPlot({
    ggplot(data = df, aes(x = Year, y = Average_Global_Temp)) +
      geom_line(color = "#ff0800") +
      labs(title = "Average Global Temperature by Year", x = "Year", y = "Average Global Temperature (°F)") +
      scale_y_continuous(labels = comma)
  })
  
  output$correlation_plot <- renderPlot({
    ggplot(data = df, aes(x = Total_Emissions, y = Average_Global_Temp)) +
      geom_point(color = "#107c10") +
      geom_smooth(method = "lm", se = FALSE, color = "#65350f") +
      labs(title = "Correlation between Total Emissions and Average Global Temperature", x = "Total Emissions (Gg CO2)", y = "Average Global Temperature (°F)") +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma)
  })
  
  output$five_year_avg_correlation_plot <- renderPlot({
    ggplot(data = df_avg, aes(x = Avg_Total_Emissions, y = Avg_Global_Temp)) +
      geom_point(color = "#107c10") +
      geom_smooth(method = "lm", se = FALSE, color = "#65350f") +
      labs(title = "5-Year Average Correlation between Emissions and Temperature", x = "Average Total Emissions (Gg CO2)", y = "Average Global Temperature (°F)") +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma)
  })
  
  output$correlation_coefficient <- renderPrint({
    correlation <- cor(df$Total_Emissions, df$Average_Global_Temp, use = "complete.obs")
    paste("Correlation Coefficient: ", round(correlation, 2))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
