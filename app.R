### Setup ###
library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)

students <- read_csv("International_Students.csv")

### Preprocess Dataset ###
str(students)
# Filter to remove NAT state and the year 2019. Also rename South Korea and the USA:
students <- students %>%
  filter(State != "NAT" & Year != "2019") %>%
  mutate(Nationality = ifelse(Nationality == "Korea, Republic of (South)", "South Korea", Nationality),
         Nationality = ifelse(Nationality == "United States of America", "USA", Nationality))

# Convert variable types:
students$Year <- factor(students$Year, ordered = T)
levels(students$Year)
students$Month <- factor(students$Month,
                         levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                         ordered = T)
levels(students$Month)
students$Nationality <- factor(students$Nationality)
levels(students$Nationality)
students$State <- factor(students$State)
levels(students$State)
students$Sector <- factor(students$Sector,
                          levels = c("Schools", "Non-award", "ELICOS", "VET", "Higher Education"))
levels(students$Sector)

### Prepare Data For Plot 1 ###
# Create dataset with total enrolments at December each year:
students_total <- students %>%
  filter(Month == "Dec") %>%
  group_by(Year) %>%
  summarise(Total_Enrolments = sum(`DATA YTD Enrolments`))

### Prepare Data For Plot 2 ###
# Find which 10 countries have the most students in Australia as of December 2018, and create dataset based on this:
students %>%
  group_by(Year, Month, Nationality) %>%
  summarise(Sum_Enrolments = sum(`DATA YTD Enrolments`)) %>%
  filter(Year == "2018" & Month == "Dec") %>%
  arrange(desc(Sum_Enrolments)) %>%
  slice(1:10)
students_top_AUS <- students %>%
  filter(Nationality == "China" |
           Nationality == "India" |
           Nationality == "Nepal" |
           Nationality == "Brazil" |
           Nationality == "Malaysia" |
           Nationality == "South Korea" |
           Nationality == "Vietnam" |
           Nationality == "Thailand" |
           Nationality == "Colombia" |
           Nationality == "Indonesia") %>%
  filter(Month == "Dec") %>%
  group_by(Nationality, Year) %>%
  summarise(Sum_Enrolments = sum(`DATA YTD Enrolments`)) %>%
  mutate(State = "AUS")

# Find which 10 countries have the most students in each state as of December 2018 and create datasets based on this:
students %>%
  group_by(Year, Month, State, Nationality) %>%
  summarise(Sum_Enrolments = sum(`DATA YTD Enrolments`)) %>%
  filter(Year == "2018" & Month == "Dec") %>%
  arrange(desc(Sum_Enrolments)) %>%
  group_by(State) %>%
  slice(1:10) %>%
  print(n = 100)
students_top_ACT <- students %>%
  filter(Nationality == "China" |
           Nationality == "India" |
           Nationality == "Bhutan" |
           Nationality == "South Korea" |
           Nationality == "Nepal" |
           Nationality == "Vietnam" |
           Nationality == "Indonesia" |
           Nationality == "Hong Kong" |
           Nationality == "Malaysia" |
           Nationality == "Pakistan") %>%
  filter(Month == "Dec" & State == "ACT") %>%
  group_by(Nationality, Year) %>%
  summarise(Sum_Enrolments = sum(`DATA YTD Enrolments`)) %>%
  mutate(State = "ACT")
students_top_NSW <- students %>%
  filter(Nationality == "China" |
           Nationality == "Nepal" |
           Nationality == "India" |
           Nationality == "Brazil" |
           Nationality == "Thailand" |
           Nationality == "South Korea" |
           Nationality == "Indonesia" |
           Nationality == "Vietnam" |
           Nationality == "Colombia" |
           Nationality == "Malaysia") %>%
  filter(Month == "Dec" & State == "NSW") %>%
  group_by(Nationality, Year) %>%
  summarise(Sum_Enrolments = sum(`DATA YTD Enrolments`)) %>%
  mutate(State = "NSW")
students_top_NT <- students %>%
  filter(Nationality == "Nepal" |
           Nationality == "India" |
           Nationality == "China" |
           Nationality == "Philippines" |
           Nationality == "Taiwan" |
           Nationality == "Indonesia" |
           Nationality == "Bangladesh" |
           Nationality == "Vietnam" |
           Nationality == "Pakistan" |
           Nationality == "Nigeria") %>%
  filter(Month == "Dec" & State == "NT") %>%
  group_by(Nationality, Year) %>%
  summarise(Sum_Enrolments = sum(`DATA YTD Enrolments`)) %>%
  mutate(State = "NT")
students_top_QLD <- students %>%
  filter(Nationality == "China" |
           Nationality == "India" |
           Nationality == "Brazil" |
           Nationality == "South Korea" |
           Nationality == "Colombia" |
           Nationality == "Taiwan" |
           Nationality == "Japan" |
           Nationality == "Nepal" |
           Nationality == "USA" |
           Nationality == "Hong Kong") %>%
  filter(Month == "Dec" & State == "QLD") %>%
  group_by(Nationality, Year) %>%
  summarise(Sum_Enrolments = sum(`DATA YTD Enrolments`)) %>%
  mutate(State = "QLD")
students_top_SA <- students %>%
  filter(Nationality == "China" |
           Nationality == "India" |
           Nationality == "Hong Kong" |
           Nationality == "Vietnam" |
           Nationality == "Nepal" |
           Nationality == "Malaysia" |
           Nationality == "South Korea" |
           Nationality == "Kenya" |
           Nationality == "Taiwan" |
           Nationality == "Japan") %>%
  filter(Month == "Dec" & State == "SA") %>%
  group_by(Nationality, Year) %>%
  summarise(Sum_Enrolments = sum(`DATA YTD Enrolments`)) %>%
  mutate(State = "SA")
students_top_TAS <- students %>%
  filter(Nationality == "China" |
           Nationality == "India" |
           Nationality == "Nepal" |
           Nationality == "Malaysia" |
           Nationality == "Pakistan" |
           Nationality == "Vietnam" |
           Nationality == "South Korea" |
           Nationality == "Hong Kong" |
           Nationality == "Singapore" |
           Nationality == "Sri Lanka") %>%
  filter(Month == "Dec" & State == "TAS") %>%
  group_by(Nationality, Year) %>%
  summarise(Sum_Enrolments = sum(`DATA YTD Enrolments`)) %>%
  mutate(State = "TAS")
students_top_VIC <- students %>%
  filter(Nationality == "China" |
           Nationality == "India" |
           Nationality == "Malaysia" |
           Nationality == "Vietnam" | 
           Nationality == "Sri Lanka" |
           Nationality == "Colombia" |
           Nationality == "Nepal" |
           Nationality == "Thailand" |
           Nationality == "Indonesia" |
           Nationality == "Pakistan") %>%
  filter(Month == "Dec" & State == "VIC") %>%
  group_by(Nationality, Year) %>%
  summarise(Sum_Enrolments = sum(`DATA YTD Enrolments`)) %>%
  mutate(State = "VIC")
students_top_WA <- students %>%
  filter(Nationality == "China" |
           Nationality == "India" |
           Nationality == "Malaysia" |
           Nationality == "Brazil" |
           Nationality == "Bhutan" |
           Nationality == "Taiwan" |
           Nationality == "Vietnam" |
           Nationality == "Hong Kong" |
           Nationality == "South Korea" |
           Nationality == "Kenya") %>%
  filter(Month == "Dec" & State == "WA") %>%
  group_by(Nationality, Year) %>%
  summarise(Sum_Enrolments = sum(`DATA YTD Enrolments`)) %>%
  mutate(State = "WA")

# Combine Australia and state datasets and convert "State" to factor:
students_comb <- bind_rows(students_top_AUS,students_top_ACT,students_top_NSW,students_top_NT,students_top_QLD,
                           students_top_SA,students_top_TAS,students_top_VIC,students_top_WA)
students_comb$State <- factor(students_comb$State)

### Prepare Data For Plot 3 ###

# Create dataset:
students_hm <- students %>%
  filter(Year == "2018" & Month == "Dec") %>%
  group_by(Sector, State) %>%
  summarise(Sum = sum(`DATA YTD Enrolments`))

### Dashboard ###

# User Interface
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "International Students In Australia", titleWidth = 320),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      
                      tags$head(tags$style(HTML('.main-header .logo {
 font-weight: bold;
 font-size: 16.5px;}'))),
                      tags$head(tags$style(HTML('.box {margin: 5px;}'))),
                      tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
 background: #667596;
 border-top: 1px solid #667596 ;
 border-bottom: 1px solid #667596 ;}
 .irs-from, .irs-to, .irs-single { background: #667596 }'))),
                      
                      fluidRow(
                        box(title = "Total International Student Enrolments", width = 7, plotlyOutput("p1"), solidHeader = TRUE),
                        box(title = "Enrolments By Sector & State in 2018", width = 5, plotlyOutput("p3"), solidHeader = TRUE)
                      ),
                      fluidRow(
                        box(title = "Top 10 Nationalities By Enrolments", width = 12, solidHeader = TRUE,
                            sidebarPanel(
                              HTML(
                                paste("This bar chart displays the top ten nationalities as measured in 2018 for each individual state
 and Australia. Use the 'State' selector to choose a state or the entire country.
 The 'Year' slider can also be used to see previous years for the ten nationalities.
 Note that the chart will scale its x-axis accordingly for each state.")),
                              
                              HTML("<br><br>"),
                              
                              sliderInput("year", label = "Year", min = 2002, max = 2018, value = 2018, sep = ""),
                              selectInput("state", label = "State", list(Australia = "AUS", ACT = "ACT", NSW = "NSW", NT = "NT", QLD = "QLD",
                                                                         SA = "SA", TAS = "TAS", VIC = "VIC", WA = "WA"))),
                            mainPanel(plotlyOutput("p2")),
                            mainPanel(HTML(paste("Data obtained from the Australian Government Department of Education:
 <br><em>https://internationaleducation.gov.au/research/International-StudentData/Pages/InternationalStudentData2019.aspx</em>"
                            ))))
                      )
                    )
)

# Server

server <- function(input, output, session) {
  
  output$p1 <- renderPlotly({
    
    titlefont <- list(size = 12, color = "#4f4f4f")
    tickfont <- list(size = 8, color = "#696969")
    margin <- list(l = 45, r = 20, b = 0, t = 0)
    
    p1 <- plot_ly(students_total, x = ~Year, y = ~Total_Enrolments, type = "scatter", mode = "lines", name = "Enrolments",
                  line = list(color = "#667596", width = 4), hoverinfo = 'text', hoverlabel = list(bgcolor = "white"),
                  text = ~paste0("Year: ", Year, "\nEnrolments: ", Total_Enrolments)) %>%
      layout(yaxis = list(title = "Total Number of Students", titlefont = titlefont, tickfont = tickfont),
             xaxis = list(tickangle = 35, title = "Year", titlefont = titlefont, tickfont = tickfont),
             margin = margin)
  })
  
  
  output$p2 <- renderPlotly({
    
    if(input$state == "AUS") {
      x=260000
      y=20000
    } else if(input$state == "ACT") {
      x=12000
      y=1000
    } else if(input$state == "NSW") {
      x=100000
      y=10000
    } else if(input$state == "NT") {
      x=500
      y=50
    } else if(input$state == "QLD") {
      x=35000
      y=5000
    } else if(input$state == "SA") {
      x=16000
      y=2000
    } else if(input$state == "TAS") {
      x=5000
      y=500
    } else if(input$state == "VIC") {
      x=90000
      y=10000
    } else if(input$state == "WA") {
      x=9000
      y=1000
    }
    
    p2 <- ggplot(students_comb, aes(x = fct_reorder(Nationality, Sum_Enrolments), y = Sum_Enrolments,
                                    text = Sum_Enrolments)) +
      geom_bar(data = filter(students_comb, State == input$state & Year == input$year),
               stat = "identity", fill = "#667596") +
      scale_y_continuous(breaks = seq(0,x,y),
                         limits = c(0, x),
                         labels = paste0(as.character(seq(0, x/1000, y/1000)),"K")) +
      labs(x = "", y = "Number of Enrolments") +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank(),
            axis.title.x = element_text(size = 9, color = "#4f4f4f"),
            axis.title.y = element_text(size = 9, color = "#4f4f4f"),
            axis.text.x = element_text(size = 6, color = "#696969", angle = -35),
            axis.text.y = element_text(size = 6, color = "#696969")) +
      coord_flip()
    
    ggplotly(p2, tooltip = "text") %>%
      style(hoverlabel = list(bgcolor = "white"))
  })
  
  output$p3 <- renderPlotly({
    
    titlefont <- list(size = 12, color = "#4f4f4f")
    tickfont <- list(size = 8, color = "#696969")
    my_palette <- colorRampPalette(c("#a0b6eb", "#7689b5", "#49546e"), bias=3.5)(n = 140)
    
    plot_ly(x = students_hm$State, y = students_hm$Sector,z = students_hm$Sum,
            type = "heatmap", source = "heatplot", colors = my_palette, hoverlabel = list(bgcolor = "white"),
            hoverinfo = 'text',
            text = ~paste0("Sector: ", students_hm$Sector, "\nState: ", students_hm$State, "\nEnrolements: ",
                           students_hm$Sum)) %>%
      layout(yaxis = list(title = paste0(c(rep("&nbsp;", 7),"Sector",rep("&nbsp;", 7),rep("\n&nbsp;", 2)), collapse = ""),
                          titlefont = titlefont , tickfont = list(size = 8, color = "#696969")),
             xaxis = list(title = "State", titlefont = titlefont , tickfont = list(size = 8, color = "#696969")))
  })
}

# Deploy The Dashboard
shinyApp(ui = ui, server = server)