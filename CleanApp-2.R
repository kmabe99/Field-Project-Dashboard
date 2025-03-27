library(dplyr)
library(readxl)
library(tidyr) 
library(ggplot2)
library(stringr)
library(janitor)
library(forcats)
library(shinythemes)
library(shiny)


ui = fluidPage(
  
  theme = shinytheme("yeti"),
  
  titlePanel("GoDaddy Assignment 1 Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(width = 3, 
                 numericInput("count.responses", "1. Please Select The Number Of Responses", 
                              value = 200, min = 1, max = 2007),
                 
                 radioButtons("question.answer", "2. Select the Information", 
                              choices = c("Employment Status" = "q25_text",
                                          "Gender" = "q17_text",
                                          "Age" = "age_text",
                                          "Race" = "q21_text")),
                 
                 selectInput("experience", "3. Select the Assisted Customer Experience",
                             choices = c("Excellent", "Good", "Fair", "Poor")),
                 
                 selectInput("cat.var", "4. Select the Category", choices = c("Commercial", "Civic", "Community","Personal", "Other")),
                            
                 
                 # Add Explanatory Text
                 h4("Explanation of Graphs"),
                 p("Graph 1: This graph represents the distribution of website visitors for GoDaddy users giving the user the option to choose how many responses they want to include."),
                 p("Graph 2: This table provides a summary of the basic information of GoDaddy users allowing the user to select the information of the customers."),
                 p("Graph 3: This graph shows the likleness a GoDaddy user would reccomend the site given its expereince with GoDaddy assistance. It allows the user to select the expereince the customer had when getting assistance."),
                 p("Graph 4: This graph illustrates the distribution of audience scope based on category selection.")
    ),
    
    mainPanel(
      fluidRow(
        column(6, plotOutput("websitecategorybusinesssize")),
        column(6, tableOutput("table"))
      ),
      
      fluidRow(
        column(6, plotOutput("experiencereccomendation")),
        column(6, plotOutput("scopeofaudience"))
      )
    )
  )
)







server = function(input, output, session){
  
  #IMPORT DATA
  df = readxl::read_xlsx('~/Documents/Spring Babson 2025/Busness Analytics Field Project/Week 2 Assignment/VF_US_National_JUL19_RawData-1.xlsx', sheet = 3)
  
  #DATA WRANGLING
  #select the questions we want
  df <- df %>%
    select(q5, q6, q9, q13, q14, q25, q17, age, q21)
  
  # merge the answers to corresponding text answer
  values.q5 <- read_excel('VF_US_National_JUL19_RawData-1.xlsx', sheet = "Values", range = "B24:C28", col_names = c("q5", "q5_text"))
  df=merge(df,values.q5, by="q5", all.x=T)
  
  values.q6 <- read_excel('VF_US_National_JUL19_RawData-1.xlsx', sheet = "Values", range = "B62:C66", col_names = c("q6", "q6_text"))
  df=merge(df,values.q6, by="q6", all.x=T)
  
  values.q9 <- read_excel('VF_US_National_JUL19_RawData-1.xlsx', sheet = "Values", range = "B107:C111", col_names = c("q9", "q9_text"))
  df=merge(df,values.q9, by="q9", all.x=T)
  
  values.q14 <- read_excel('VF_US_National_JUL19_RawData-1.xlsx', sheet = "Values", range = "B376:C386", col_names = c("q14", "q14_text"))
  df=merge(df,values.q14, by="q14", all.x=T)
  
  values.q13 <- read_excel('VF_US_National_JUL19_RawData-1.xlsx', sheet = "Values", range = "B387:C390", col_names = c("q13", "q13_text"))
  df=merge(df,values.q13, by="q13", all.x=T)
  
  values.q17 <- read_excel('VF_US_National_JUL19_RawData-1.xlsx', sheet = "Values", range = "B393:C395", col_names = c("q17", "q17_text"))
  df=merge(df,values.q17, by="q17", all.x=T)
  
  values.age <- read_excel('VF_US_National_JUL19_RawData-1.xlsx', sheet = "Values", range = "B396:C399", col_names = c("age", "age_text"))
  df=merge(df,values.age, by="age", all.x=T)
  
  values.q21 <- read_excel('VF_US_National_JUL19_RawData-1.xlsx', sheet = "Values", range = "B408:C412", col_names = c("q21", "q21_text"))
  df=merge(df,values.q21, by="q21", all.x=T)
  
  values.q25 <- read_excel('VF_US_National_JUL19_RawData-1.xlsx', sheet = "Values", range = "B425:C434", col_names = c("q25", "q25_text"))
  df=merge(df,values.q25, by="q25", all.x=T)
  
  # remove answers in numeric value
  df <- df %>%
    select(-q5, -q6, -q9, -q13, -q14, -q25, -q17, -age, -q21)
  
  # replace NaN with "unknown"
  # Define the columns to update
  text_columns <- c("q5_text","q6_text", "q9_text", "q13_text", "q14_text", "q25_text", "q17_text", "age_text", "q21_text")
  
  # Replace NA values with "Unknown" in the specified columns
  df <- df %>%
    mutate(across(all_of(text_columns), ~replace_na(.x, "Unknown")))
  
  #remove the (#) before the text answer
  df <- df %>% 
    mutate_all(~ str_replace_all(., "\\(\\d+\\)\\s*", ""))
  
  
  
  
  
  
  
  # VISUALIZATION 1: WEBSITE VISITORS
  output$websitecategorybusinesssize= renderPlot({
    
    #category_data <- df %>% filter(q5_text == input$cat.var)
    count.response <- df[1:input$count.responses, ]
    
    ggplot(count.response, aes(x = q6_text)) +
      geom_bar(fill = "orange", color = "black", alpha = 0.7) +
      labs(title = "1. Distribution of Website Visitors",
           x = "Website Visitors Range",
           y = "Count of Responses") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability
  })
  
  
  
  
  # VISUALIZATION 2: TABLE PERCENTAGE DISTRIBUTION OF IMPORTANT INFORMATION
  output$table = renderTable({
    
    mytable=tabyl(df, input$question.answer) 
    mytable=mytable[order(mytable$n, decreasing = TRUE),]
    mytable
  })
  
  
  
  
  # VISUALIZATION 3: GODADDY ASSISTED EXPEREINCE WITH LIKLENESS TO RECOMMEND GODADDY TO SOMEONE ELSE
  output$experiencereccomendation= renderPlot({
    custom_levels <- c("0 Not likely at all", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 Extremely likely")
    
    # Convert q14_text to a factor with specified levels
    experience_data <- df %>%
      filter(q13_text == input$experience) %>%
      mutate(q14_text = factor(q14_text, levels = custom_levels))
    
    # Create the bar plot ensuring all levels appear
    ggplot(experience_data, aes(x = q14_text)) +
      geom_bar(fill = "blue", color = "black", alpha = 0.7) +
      labs(title = "3. Likelihood to Recommend GoDaddy",
           x = "Recommendation Score",
           y = "Count of Responses") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate labels for readability
      scale_x_discrete(drop = FALSE)  # Ensure all factor levels appear
  })
  
  
  
  
  # VISUALIZATION 4: WHAT IS YOUR SCOPE OF BUSINESS PER EACH CATEGORY
  output$scopeofaudience= renderPlot({
    #count.response <- data[1:input$count.responses, ]
    
    category_data <- df %>% filter(q5_text == input$cat.var)
    
    # Create the stacked bar chart
    ggplot(category_data, aes(x = q5_text, fill = q9_text)) +  
      geom_bar(color = "black", alpha = 0.7) +  # Stack q9_text responses
      labs(title = "4. Audience Scope by Website Category",
           x = "Webiste Category",
           y = "Count of Responses",
           fill = "Audience Scope") +  # Legend title
      theme_minimal() 
  })
  
}

shinyApp(ui, server)
