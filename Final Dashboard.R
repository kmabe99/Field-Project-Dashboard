library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(treemapify)
library(tidyr)
library(forcats)
library(broom)
library(knitr)

# ------------------------------------------------------
# Set working directory
# ------------------------------------------------------
setwd("/Users/student/Documents/Spring Babson 2025/Busness Analytics Field Project/GoDaddy Project")

# ------------------------------------------------------
# Load and filter dataset
# ------------------------------------------------------
data <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Data")

# Map employee size
emp_map <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Values", range = "B57:C65", col_names = c("q3a", "q3a_text"))
emp_map$q3a_text <- gsub("^\\(\\d+\\)\\s*", "", emp_map$q3a_text)

# Map business category
category_map <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Values", range = "B5:C6", col_names = c("q4", "q4_text")) %>%
  mutate(Category = ifelse(grepl("Business", q4_text), "Business or commercial", "Personal or community"))

# Map hiring plan
hiring_map <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Values", range = "B66:C70", col_names = c("q3c", "q3c_text")) %>%
  mutate(q3c_text = gsub("^\\(\\d+\\)\\s*", "", q3c_text),
         Hiring_plans_new = case_when(
           q3c_text %in% c("Yes, I plan to hire up to an additional 3 employees",
                           "Yes, I plan to hire substantially more employees") ~ "Yes",
           q3c_text == "No, I do not plan to hire more employees" ~ "No",
           TRUE ~ "Unknown"
         ))

# Map aspirations
asp_map <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Values", range = "B1650:C1657", col_names = c("q17a", "q17a_text")) %>%
  mutate(q17a_text = gsub("^\\(\\d+\\)\\s*", "", q17a_text),
         aspirations = case_when(
           grepl("solo entrepreneur", q17a_text) ~ "Solo Entrepreneur",
           grepl("small business", q17a_text) ~ "Small Business",
           grepl("mid-size", q17a_text) ~ "Mid-Size",
           grepl("corporate", q17a_text) ~ "Corporate",
           TRUE ~ "Unknown"
         ))

# Map revenue
rev_map <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Values", range = "B1563:C1567", col_names = c("q11c", "q11c_text")) %>%
  mutate(q11c_text = case_when(
    q11c_text == "(1) My revenue has increased" ~ "Increased",
    q11c_text == "(2) My revenue has stayed constant" ~ "Constant",
    q11c_text == "(3) My revenue has decreased" ~ "Decreased",
    q11c_text %in% c("(98) Don't know", "(99) Prefer not to answer") ~ "Unknown",
    TRUE ~ q11c_text
  ))

# Map investments
inv_map <- function(range, col_name) {
  read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Values", range = range, col_names = c(col_name, "text")) %>%
    mutate(cat = case_when(
      text %in% c("(1) Marketing my business online (social media, ads, promotions, etc.)",
                  "(2) Marketing my business using traditional media (print, radio, tv, promotions, etc.)") ~ "Marketing",
      text %in% c("(3) Hiring employees", "(8) Hiring a consultant") ~ "Hiring",
      text %in% c("(4) Developing new products / Services") ~ "Product Development",
      text %in% c("(5) Finding office / Retail space") ~ "Office/Retail Space",
      text %in% c("(6) Creating my website") ~ "Website",
      text %in% c("(7) Obtaining licenses / Permits / Taxes") ~ "Legal & Permits",
      text %in% c("(9) Equipment") ~ "Equipment",
      text %in% c("(10) Inventory") ~ "Inventory",
      text %in% c("(98) Don't know", "(99) Prefer not to answer") ~ "Unknown",
      TRUE ~ "Other"
    ))
}
q72_map <- inv_map("B1476:C1488", "q72")
q73_map <- inv_map("B1489:C1501", "q73")

# Capital use mapping
q78_map <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Values",
                      range = "B1463:C1475", col_names = c("q78", "q78_text")) %>%
  mutate(q78_text = gsub("^\\(\\d+\\)\\s*", "", q78_text))

# Challenge label mapping
label_map2 <- c(
  "q21a_1" = "Access to capital",
  "q21a_2" = "Affordable space",
  "q21a_18" = "Marketing online",
  "q21a_19" = "Traditional media",
  "q21a_4" = "Affordable healthcare",
  "q21a_5" = "Finding employees",
  "q21a_6" = "Website",
  "q21a_20" = "Social media setup",
  "q21a_7" = "Business planning",
  "q21a_14" = "Rising wages",
  "q21a_8" = "Technology management",
  "q21a_9" = "Networking",
  "q21a_10" = "Licensing/permits",
  "q21a_11" = "Taxes",
  "q21a_23" = "Time management"
)

challenge_vars <- names(label_map2)

# Stressor labels
stressor_labels <- data.frame(
  q76_code = c("q76_1", "q76_2", "q76_3", "q76_4", "q76_5", "q76_6", "q76_10"),
  Stressor = c(
    "Financial", "Work-Life Balance", "Customers",
    "Employees", "Vendors/Partnerships",
    "Technology/Equipment", "Competition"
  )
)

# Merge all mappings
df <- data %>%
  left_join(emp_map, by = "q3a") %>%
  left_join(category_map, by = "q4") %>%
  left_join(hiring_map, by = "q3c") %>%
  left_join(asp_map[, c("q17a", "aspirations")], by = "q17a") %>%
  left_join(rev_map, by = "q11c") %>%
  left_join(q72_map[, c("q72", "cat")], by = "q72") %>%
  rename(Initial_investment = cat) %>%
  left_join(q73_map[, c("q73", "cat")], by = "q73") %>%
  rename(Recent_investment = cat) %>%
  left_join(q78_map, by = "q78") %>%
  filter(q3a_text %in% c("1 (just myself)", "2-4", "5-9"),
         Category == "Business or commercial")


# ------------------------------------------------------
# Persona data preparation (optimized for box plots)
# ------------------------------------------------------
# ------------------------------------------------------
# Load and clean additional variables needed for personas
# ------------------------------------------------------
persona_data <- df %>%
  mutate(
    age = ifelse(d12 %in% c(-8, -9), NA, 2024 - d12),
    Business_Age = ifelse(q20 %in% c(-8, -9), NA, 2024 - q20)
  )

# ------------------------------------------------------
# Load additional mappings
# ------------------------------------------------------
gender_map <- read_excel("VF_US_National_FEB24_RawData.xlsx", 
                         sheet = "Values", range = "B2036:C2039", col_names = c("d1", "Gender")) %>%
  mutate(Gender = gsub("^\\(\\d+\\)\\s*", "", Gender))

race_map <- read_excel("VF_US_National_FEB24_RawData.xlsx", 
                       sheet = "Values", range = "B2040:C2045", col_names = c("d3", "Race")) %>%
  mutate(Race = gsub("^\\(\\d+\\)\\s*", "", Race))

hours_map <- read_excel("VF_US_National_FEB24_RawData.xlsx", 
                        sheet = "Values", range = "B1574:C1582", col_names = c("q12", "Weekly_Hours")) %>%
  mutate(Weekly_Hours = gsub("^\\(\\d+\\)\\s*", "", Weekly_Hours))

income_map <- read_excel("VF_US_National_FEB24_RawData.xlsx", 
                         sheet = "Values", range = "B1557:C1562", col_names = c("q11b", "Income_Contribution")) %>%
  mutate(Income_Contribution = gsub("^\\(\\d+\\)\\s*", "", Income_Contribution))

# ------------------------------------------------------
# # Merge all mappings
# ------------------------------------------------------
persona_data <- persona_data %>%
  left_join(gender_map, by = "d1") %>%
  left_join(race_map, by = "d3") %>%
  left_join(hours_map, by = "q12") %>%
  left_join(income_map, by = "q11b")

# ------------------------------------------------------
# Define consistent color palette
# ------------------------------------------------------
vf_colors <- c(
  "Yes" = "#55DBD5", "No" = "#8647DC", "Unknown" = "#726EFF",
  "Male" = "#2B50C4", "Female" = "#843DC6", "Non-binary" = "#21ACDB",
  "White" = "#55DBD5", "Black or African American" = "#8647DC", 
  "Hispanic or Latino" = "#726EFF", "Asian" = "#843DC6",
  "Other" = "#21ACDB",
  "Increased" = "#55DBD5", "Constant" = "#726EFF", "Decreased" = "#8647DC"
)

# Colors
vf_colors <- c("Yes" = "#55DBD5", "No" = "#8647DC", "Unknown" = "#726EFF",
               "Financial" = "#843DC6", "Work-Life Balance" = "#55DBD5", "Customers" = "#2B50C4",
               "Employees" = "#21ACDB", "Vendors/Partnerships" = "#4742C3", "Technology/Equipment" = "#7B9CF5",
               "Competition" = "#6A3EC4",
               "Marketing my business online (social media, ads, promotions, etc.)" = "#843DC6",
               "Hiring employees" = "#55DBD5",
               "Developing new products / Services" = "#2B50C4",
               "Equipment" = "#4742C3",
               "Inventory" = "#21ACDB",
               "Marketing online" = "#843DC6",
               "Access to capital" = "#28C1DD",
               "Traditional media" = "#21ACDB",
               "Website" = "#2B50C4",
               "Taxes" = "#1C76D5",
               "Finding employees" = "#225FCF",
               "Networking" = "#D4AF37",
               "Licensing/permits" = "#4742C3",
               "Affordable space" = "#6A3EC4",
               "Rising wages" = "#39E0E0",
               "Time management" = "#7B9CF5",
               "Business planning" = "#1D92DA"
)

inv_colors <- c("Initial_count" = "#55DBD5", "Recent_count" = "#8647DC")

challenge_colors <- c(
  "Marketing online" = "#39E0E0",
  "Access to capital" = "#28C1DD",
  "Traditional media" = "#21ACDB",
  "Website" = "#1D92DA",
  "Taxes" = "#1C76D5",
  "Finding employees" = "#225FCF",
  "Networking" = "#2B50C4",
  "Licensing/permits" = "#4742C3",
  "Affordable space" = "#6A3EC4",
  "Rising wages" = "#843DC6"
)

# ------------------------------------------------------
# Predictive Insights data preparation
# ------------------------------------------------------
feb24 <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Data")

# ------------------------------------------------------
# Mapping for Predictive Insights
# ------------------------------------------------------

# Q4 - Business Category
map_q4 <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Values", range = "B5:C6", col_names = c("q4", "q4_text"))
map_q4$q4_text <- gsub("^\\(\\d+\\)\\s*", "", map_q4$q4_text)
map_q4 <- map_q4 %>% mutate(Category = case_when(
  q4_text == "Business or commercial (including for profit and non-profit entities)" ~ "Business or commercial",
  q4_text == "Personal or community (primarily for a hobby, idea, religious organization, community, or sports team/association)" ~ "Personal or community",
  TRUE ~ q4_text
)) %>% select(q4, Category)

# Q3A - Employee Size
map_q3a <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Values", range = "B57:C65", col_names = c("q3a", "q3a_text"))
map_q3a$q3a_text <- gsub("^\\(\\d+\\)\\s*", "", map_q3a$q3a_text)

# Q3C - Hiring Plan
map_q3c <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Values", range = "B66:C70", col_names = c("q3c", "q3c_text"))
map_q3c <- map_q3c %>% mutate(Hiring_plan = case_when(
  q3c_text %in% c("(1) Yes, I plan to hire up to an additional 3 employees",
                  "(2) Yes, I plan to hire substantially more employees") ~ "Yes",
  q3c_text == "(3) No, I do not plan to hire more employees" ~ "No",
  TRUE ~ NA_character_
)) %>% select(q3c, Hiring_plan)

# Q86_1 - ML
map_ml <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Values", range = "B76:C80", col_names = c("q86_1", "q86_1_text"))
map_ml$q86_1_text <- gsub("^\\(\\d+\\)\\s*", "", map_ml$q86_1_text)



# Q86_3 - AI
map_ai <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Values", range = "B86:C90", col_names = c("q86_3", "q86_3_text"))
map_ai$q86_3_text <- gsub("^\\(\\d+\\)\\s*", "", map_ai$q86_3_text)


# Coerce all keys used in joins to character to match mapping tables
feb24$q4     <- as.character(feb24$q4)
feb24$q3a    <- as.character(feb24$q3a)
feb24$q3c    <- as.character(feb24$q3c)
feb24$q86_1  <- as.character(feb24$q86_1)
feb24$q86_3  <- as.character(feb24$q86_3)
map_q4$q4 <- as.character(map_q4$q4)
map_q3a$q3a <- as.character(map_q3a$q3a)
map_q3c$q3c <- as.character(map_q3c$q3c)
map_ml$q86_1 <- as.character(map_ml$q86_1)
map_ai$q86_3 <- as.character(map_ai$q86_3)


# ------------------------------------------------------
# Merge Mappings
# ------------------------------------------------------
feb24_pi <- feb24 %>%
  left_join(map_q4, by = "q4") %>%
  left_join(map_q3a, by = "q3a") %>%
  left_join(map_q3c, by = "q3c") %>%
  left_join(map_ml, by = "q86_1") %>%
  left_join(map_ai, by = "q86_3") %>%
  filter(Hiring_plan %in% c("Yes", "No"),
         q3a_text %in% c("1 (just myself)", "2-4", "5-9"),
         Category == "Business or commercial") %>%
  mutate(
    AccessCapital = ifelse(q21a_1 > 0, "Yes", "No"),
    AI_Optimism = case_when(
      q93 %in% c(1, 2) ~ "Agree",
      q93 %in% c(4, 5) ~ "Disagree",
      TRUE ~ "Unknown"
    ),
    Aspirations = case_when(
      q17a == 1 ~ "Solo",
      q17a %in% 2:5 ~ "Growth",
      TRUE ~ "Unknown"
    ),
    RevenueOutlook = case_when(
      q7 == 1 ~ "Positive",
      q7 == 2 ~ "Negative",
      TRUE ~ "Unknown"
    )
  ) %>%
  select(Hiring_plan, q3a_text, Aspirations, RevenueOutlook,
         q86_1_text, q86_3_text, AccessCapital, AI_Optimism) %>%
  rename(
    Employees = q3a_text,
    Uses_ML = q86_1_text,
    Uses_AI = q86_3_text
  ) %>%
  mutate(HiringPlan = factor(Hiring_plan)) %>%
  select(-Hiring_plan) %>%
  mutate(across(everything(), ~ replace_na(as.character(.), "Unknown")),
         across(everything(), as.factor))

# ------------------------------------------------------
# Logistic Regression
# ------------------------------------------------------
model <- glm(HiringPlan ~ ., data = feb24_pi, family = binomial)
model_summary <- tidy(model)

label_map <- c(
  "Employees2-4" = "Employees: 2–4",
  "Employees5-9" = "Employees: 5–9",
  "AspirationsSolo" = "Aspiration: Solo",
  "AspirationsUnknown" = "Aspiration: Unknown",
  "RevenueOutlookPositive" = "Revenue Outlook: Positive",
  "RevenueOutlookUnknown" = "Revenue Outlook: Unknown",
  "Uses_MLSelected" = "ML Use: Yes",
  "Uses_AISelected" = "AI Use: Yes",
  "AccessCapitalYes" = "Challenge: Access to Capital",
  "AI_OptimismDisagree" = "AI Optimism: Disagree",
  "AI_OptimismUnknown" = "AI Optimism: Unknown"
)

summary_table <- model_summary %>%
  mutate(
    Predictor = gsub("`", "", term),
    Estimate = round(estimate, 3),
    `Std. Error` = round(std.error, 3),
    `z value` = round(statistic, 2),
    `P-value` = ifelse(p.value < 0.001, "< 0.001", round(p.value, 5)),
    Display = recode(Predictor, !!!label_map)
  ) %>%
  select(Predictor, Display, Estimate, `Std. Error`, `z value`, `P-value`)

interpretations <- summary_table %>%
  filter(Predictor != "(Intercept)") %>%
  mutate(OddsRatio = exp(Estimate)) %>%
  select(Display, OddsRatio)


# ------------------------------------------------------
# Shiny app
# ------------------------------------------------------

# ------------------------------------------------------
# Shiny app
# ------------------------------------------------------

ui <- fluidPage(
  titlePanel("Microbusiness Growth Dashboard"),
  tabsetPanel(
    
    # Tab 1: Guidelines tab
    tabPanel("Guidelines",
             h2("Dashboard Usage Guide"),
             uiOutput("guidelines_content")),
    
    # Tab 2: Situation Overview
    tabPanel("Situation Overview",
             sidebarLayout(
               sidebarPanel(
                 style = "position:sticky;top:20px;z-index:1;",
                 selectInput("emp_size", "Select Employee Size:",
                             choices = c("All", "1 (just myself)", "2-4", "5-9")),
                 helpText("Use the dropdown to explore how microbusiness behavior varies by employee size.")
               ),
               mainPanel(
                 fluidRow(
                   column(12,
                          h3("Microbusiness Hiring: An Entry Point to Understanding Growth"),
                          p("Most microbusinesses don’t hire employees. But for the few that do, hiring often connects to deeper indicators of growth. This tab explores who is hiring and what that means.")
                   )
                 ),
                 fluidRow(
                   column(12,
                          h3("Current Hiring Plans: A Snapshot"),
                          p(strong("Subtitle:"), "What percentage of microbusinesses plan to hire more people?"),
                          p("This treemap highlights the distribution of current hiring intentions across microbusinesses."),
                          plotOutput("hiring_treemap")
                   )
                 ),
                 fluidRow(
                   column(12,
                          br(),
                          p("To better understand the mindset of businesses that are hiring versus those that are not, we looked at three additional indicators: investment patterns, revenue performance, and long-term aspirations.")
                   )
                 ),
                 fluidRow(
                   column(12,
                          h3("Investment Priorities: Investing in Future Growth"),
                          p(strong("Subtitle:"), "Which areas are businesses investing in now vs. earlier?"),
                          tags$p(em(textOutput("filter_investment")), style = "color: #555; font-style: italic;"),
                          p("This bar chart compares the types of investments businesses made initially versus more recently."),
                          plotOutput("investment_bar")
                   )
                 ),
                 fluidRow(
                   column(6,
                          h3("Revenue Performance: The Engine of Growth"),
                          p(strong("Subtitle:"), "Are businesses with higher revenue growth hiring more?"),
                          tags$p(em(textOutput("filter_revenue")), style = "color: #555; font-style: italic;"),
                          p("This chart explores how recent revenue performance relates to hiring plans."),
                          plotOutput("revenue_bar")
                   ),
                   column(6,
                          h3("Business Aspirations: The Vision for Expansion"),
                          p(strong("Subtitle:"), "Do long-term goals affect hiring decisions?"),
                          tags$p(em(textOutput("filter_aspiration")), style = "color: #555; font-style: italic;"),
                          p("This bar chart shows how aspirations, such as staying solo vs. becoming a small company, align with hiring."),
                          plotOutput("aspiration_bar")
                   )
                 )
               )
             )),
    
    # Tab 3: Business Challenges
    tabPanel("Business Challenges",
             fluidPage(
               h2("Business Challenges: The Financial and Emotional Landscape of Microbusinesses"),
               p("Capital constraints and operational stress are recurring themes in the world of microbusinesses."),
               br(),
               fluidRow(
                 column(12,
                        h3("Capital Use by Employee Size"),
                        p(strong("Subtitle:"), "How do businesses of different sizes spend their capital?"),
                        p("This line chart illustrates the top 5 ways microbusinesses are using capital. Compare the top 5 uses of capital across different employee groups to uncover key priorities."),
                        plotOutput("capital_plot")
                 )
               ),
               fluidRow(
                 column(6),
                 column(6,
                        selectInput("emp_filter_challenges", "Filter by Employee Size:",
                                    choices = c("All", "1 (just myself)", "2-4", "5-9")),
                        helpText("Use this filter to explore how challenges and stressors vary by employee size.")
                 )
               ),
               fluidRow(
                 column(12,
                        HTML('<p style="font-size:14px; margin-top:20px; margin-bottom:10px;">
                      Beyond capital spending, we looked at the pain points microbusinesses face, both when starting up and running day to day.
                    </p>')
                 )
               ),
               fluidRow(
                 column(12,
                        h3("Top Startup Challenges by Employee Size"),
                        p(strong("Subtitle:"), "What are the top hurdles when starting a business?"),
                        plotOutput("challenge_plot")
                 )
               ),
               fluidRow(
                 column(12,
                        h3("Top Business Stressors by Employee Size"),
                        p(strong("Subtitle:"), "What are the biggest stressors day-to-day?"),
                        plotOutput("stressor_plot")
                 )
               )
             )
    ),
    
    # Tab 4: Hiring Personas
    tabPanel("Hiring Personas",
             fluidPage(
               fluidRow(
                 column(12,
                        h2("Hiring Personas: Who is Behind the Business?"),
                        p("This section presents a demographic and operational profile of microbusiness owners, organized by their hiring intentions. 
                      Use the filter to compare characteristics such as age, gender, revenue trends, and business aspirations
                      between those planning to hire and those who are not.")
                 )
               ),
               
               # Row 1: filter + summary table + stats
               fluidRow(
                 column(4,
                        selectInput("hiring_filter", "Filter by Hiring Plan:",
                                    choices = c("All", "Yes", "No", "Unknown"),
                                    selected = "All"),
                        helpText("Select a hiring plan category to view specific persona profiles.")
                 ),
                 column(8,
                        h4("Key Statistics"),
                        uiOutput("persona_stats")
                 )
               ),
               
               hr(),
               
               # Row 2: age + business age
               fluidRow(
                 column(6,
                        h3("Age Distribution"),
                        p(strong("Subtitle:"), "Are younger business owners more likely to hire?"),
                        p("This box plot compares the age of business owners across different hiring plans. 
                      Median values highlight where hiring business owners fall on the age spectrum."),
                        plotOutput("age_boxplot")
                 ),
                 column(6,
                        h3("Business Age Distribution"),
                        p(strong("Subtitle:"), "Does business age influence hiring plans?"),
                        p("This chart visualizes how long businesses have been operating across hiring groups. 
                      It helps highlight whether newer businesses are more likely to hire."),
                        plotOutput("business_age_boxplot")
                 )
               ),
               
               # Row 3: gender + race
               fluidRow(
                 column(6,
                        h3("Gender Distribution"),
                        p(strong("Subtitle:"), "What does gender breakdown look like across hiring plans?"),
                        p("This pie chart shows the distribution of gender identities among business owners by hiring intention."),
                        plotOutput("gender_pie")
                 ),
                 column(6,
                        h3("Race Distribution"),
                        p(strong("Subtitle:"), "Are there differences in hiring across racial identities?"),
                        p("This bar chart explores racial representation across hiring categories to highlight disparities."),
                        plotOutput("race_bar")
                 )
               ),
               
               # Row 4: revenue + hours
               fluidRow(
                 column(6,
                        h3("Revenue Trend (Past 6 Months)"),
                        p(strong("Subtitle:"), "Do businesses with rising revenue hire more?"),
                        p("This chart compares recent revenue performance of businesses based on their hiring intentions."),
                        plotOutput("revenue_trend")
                 ),
                 column(6,
                        h3("Weekly Hours Worked"),
                        p(strong("Subtitle:"), "Are hiring business owners working more hours?"),
                        p("This plot compares weekly time commitment between hiring and non-hiring business owners."),
                        plotOutput("hours_worked")
                 )
               ),
               
               # Row 5: income
               fluidRow(
                 column(12,
                        h3("Household Income Contribution from Business"),
                        p(strong("Subtitle:"), "How much of household income comes from the business?"),
                        p("This plot shows whether hiring decisions are associated with greater reliance on business income."),
                        plotOutput("income_contribution")
                 )
               ),
               
               # Row 6: aspirations
               fluidRow(
                 column(12,
                        h3("Business Aspirations"),
                        p(strong("Subtitle:"), "Do long-term goals impact hiring intent?"),
                        p("This chart explores how different business aspirations such as remaining solo versus scaling
                      align with the decision to hire."),
                        plotOutput("aspirations_plot")
                 )
               )
             )
    ),
    
    # Tab 5: Predictive Insights
    tabPanel("Predictive Insights",
             fluidPage(
               h3("Predictive Insights: What Influences Microbusiness Hiring?"),
               p("This tab highlights key predictors of hiring behavior among microbusinesses using a logistic regression model. 
                 The model estimates how different characteristics are associated with the likelihood of hiring, while holding other factors constant."),
               fluidRow(
                 column(4,
                        wellPanel(
                          h5("Choose Predictors to Interpret:"),
                          checkboxGroupInput("predictors", "", choices = interpretations$Display)
                        ),
                        div(style = "padding-left: 20px;",
                            HTML("<b><i>Reference Groups (used as baselines in the model):</i></b><br>
         • <b>Employee Size</b>: Businesses with <i>just one person (solo operator)</i><br>
         • <b>Business Aspirations</b>: Businesses that <i>aspire to grow beyond solo status</i><br>
         • <b>Revenue Performance</b>: Businesses reporting <i>declining revenue</i><br>
         • <b>Use of ML/AI Tools</b>: Businesses that <i>do not use machine learning or AI</i><br>
         • <b>Access to Capital</b>: Businesses that <i>do not report 'access to capital' as a challenge</i><br>
         • <b>AI Outlook</b>: Businesses that <i>agree AI helps them compete</i>"))
                 ),
                 column(8,
                        h4("Logistic Regression Output (Summary)"),
                        verbatimTextOutput("reg_summary"),
                        hr(),
                        h4("Interpretation"),
                        uiOutput("interpretation")
                 )
               )
             )
    )
    
  ) 
) 



# ------------------------------------------------------
# Server 
# ------------------------------------------------------
server <- function(input, output) {
  
  # Render dashboard guidelines for the guidelines tab
  # Purpose: introduce users to the dashboard structure and data source
  
  output$guidelines_content <- renderUI({
    HTML("
    <h3>Welcome to the Microbusiness Growth Dashboard</h3>
    <p>This dashboard explores the growth potential of microbusinesses using data from the <strong>February 2024 survey</strong> conducted by <strong>Venture Forward, a GoDaddy initiative</strong>.</p>

    <p>Use the following tabs to explore different themes:</p>
    <ul>
      <li><strong>Situation Overview:</strong> Insights on hiring, investment, revenue, and growth goals</li>
      <li><strong>Business Challenges:</strong> Top challenges and day-to-day stressors</li>
      <li><strong>Hiring Personas:</strong> Profiles based on hiring behavior and business characteristics</li>
      <li><strong>Predictive Insights:</strong> Uncovering Growth Signals Through Hiring Behavior</li>
    </ul>

    <p>Use the <strong>interactive filters</strong> in each tab to refine your analysis.</p>
  ")
  })
  
  # ------------------------------------------------------
  # Reactive data
  # ------------------------------------------------------
  
  # Reactive data for overview tab
  overview_data <- reactive({
    if (input$emp_size == "All") df else df %>% filter(q3a_text == input$emp_size)
  })
  
  # Reactive data for challenges tab
  challenges_data <- reactive({
    if (input$emp_filter_challenges == "All") df else df %>% filter(q3a_text == input$emp_filter_challenges)
  })
  
  # Reactive data for personas tab
  persona_filtered <- reactive({
    if (input$hiring_filter == "All") {
      persona_data
    } else {
      persona_data %>% filter(Hiring_plans_new == input$hiring_filter)
    }
  })
  
  # ------------------------------------------------------
  # Output situation overview
  # ------------------------------------------------------
  
  # Hiring plans plot
  output$hiring_treemap <- renderPlot({
    data <- overview_data() %>%
      filter(!is.na(Hiring_plans_new)) %>%
      count(Hiring_plans_new) %>%
      mutate(percent = n / sum(n) * 100,
             label = paste0(Hiring_plans_new, "\n", n, " (", round(percent, 1), "%)"))
    
    ggplot(data, aes(area = n, fill = Hiring_plans_new, label = label)) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", grow = FALSE, fontface = "bold") +
      scale_fill_manual(values = vf_colors) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Investment plot   
  output$investment_bar <- renderPlot({
    data <- overview_data()
    
    initial <- data %>% count(Initial_investment, name = "Initial_count")
    recent  <- data %>% count(Recent_investment, name = "Recent_count")
    
    merged <- full_join(initial, recent, by = c("Initial_investment" = "Recent_investment")) %>%
      replace_na(list(Initial_count = 0, Recent_count = 0)) %>%
      rename(Category = Initial_investment) %>%
      mutate(pct_change = (Recent_count - Initial_count) / pmax(Initial_count, 1) * 100,
             pct_label = paste0("Δ ", round(pct_change, 1), "%")) %>%
      pivot_longer(cols = c("Initial_count", "Recent_count"),
                   names_to = "Investment_type", values_to = "Count")
    
    # Sort by highest abs % change
    merged$Category <- factor(merged$Category,
                              levels = merged %>%
                                distinct(Category, pct_change) %>%
                                arrange(-abs(pct_change)) %>% pull(Category))
    
    ggplot(merged, aes(x = Category, y = Count, fill = Investment_type)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.8) +
      geom_text(data = merged %>% distinct(Category, pct_label),
                aes(x = Category, y = max(merged$Count), label = pct_label),
                inherit.aes = FALSE, vjust = -0.5, fontface = "bold") +
      scale_fill_manual(values = inv_colors,
                        labels = c("Initial Investment", "Recent Investment"),
                        name = "Investment Period") +
      labs(x = "Investment Category", y = "Number of Businesses") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))
  })
  # Revenue plot    
  output$revenue_bar <- renderPlot({
    df_revenue <- overview_data() %>%
      filter(!is.na(q11c_text), q11c_text != "Unknown", !is.na(Hiring_plans_new)) %>%
      count(q11c_text, Hiring_plans_new, name = "Count") %>%
      group_by(q11c_text) %>%
      mutate(Percent = Count / sum(Count) * 100)
    
    totals <- df_revenue %>% group_by(q11c_text) %>% summarise(Total = sum(Count))
    
    ggplot(df_revenue, aes(x = q11c_text, y = Count, fill = Hiring_plans_new)) +
      geom_bar(stat = "identity", position = "fill") +
      geom_text(aes(label = paste0(round(Percent, 1), "%")),
                position = position_fill(vjust = 0.5), color = "white", fontface = "bold", size = 4) +
      geom_text(data = totals, aes(x = q11c_text, y = 1, label = Total),
                inherit.aes = FALSE, vjust = -0.5, fontface = "bold", size = 5) +
      scale_fill_manual(values = vf_colors, name = "Hiring Plans") +
      labs(x = "Recent Revenue Performance", y = "Proportion of Businesses") +
      theme_minimal()
  })
  
  # Aspiration plot  
  output$aspiration_bar <- renderPlot({
    df_asp <- overview_data() %>%
      filter(!is.na(aspirations), aspirations != "Unknown", !is.na(Hiring_plans_new)) %>%
      count(aspirations, Hiring_plans_new, name = "Count") %>%
      group_by(aspirations) %>%
      mutate(Percent = Count / sum(Count) * 100)
    
    totals <- df_asp %>% group_by(aspirations) %>% summarise(Total = sum(Count))
    
    ggplot(df_asp, aes(x = aspirations, y = Count, fill = Hiring_plans_new)) +
      geom_bar(stat = "identity", position = "fill") +
      geom_text(aes(label = paste0(round(Percent, 1), "%")),
                position = position_fill(vjust = 0.5), color = "white", fontface = "bold", size = 4) +
      geom_text(data = totals, aes(x = aspirations, y = 1, label = Total),
                inherit.aes = FALSE, vjust = -0.5, fontface = "bold", size = 5) +
      scale_fill_manual(values = vf_colors, name = "Hiring Plans") +
      labs(x = "Business Aspiration", y = "Proportion of Businesses") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # ------------------------------------------------------
  # Show active filter
  # ------------------------------------------------------
  
  output$filter_investment <- renderText({ paste("Showing results for:", input$emp_size) })
  output$filter_revenue <- renderText({ paste("Showing results for:", input$emp_size) })
  output$filter_aspiration <- renderText({ paste("Showing results for:", input$emp_size) })
  
  
  # --------------------------------------------------
  # Output business challenges
  # --------------------------------------------------
  
  # Data preparation for capital use plot
  capital_plot_data <- df %>%
    filter(!is.na(q78_text), !q78_text %in% c("Don't know", "Prefer not to answer")) %>%
    count(q3a_text, q78_text, sort = FALSE) %>%
    group_by(q3a_text) %>%
    mutate(percent = 100 * n / sum(n)) %>%
    ungroup()
  
  capital_totals <- capital_plot_data %>%
    group_by(q3a_text) %>%
    summarise(total = sum(n), max_percent = max(percent))
  
  # Capital use plot
  top5_capital <- df %>%
    filter(!is.na(q78_text), !q78_text %in% c("Don't know", "Prefer not to answer")) %>%
    count(q78_text, sort = TRUE) %>%
    slice_max(n, n = 5) %>%
    pull(q78_text)
  
  capital_plot_data <- df %>%
    filter(q78_text %in% top5_capital) %>%
    count(q3a_text, q78_text, sort = FALSE) %>%
    group_by(q3a_text) %>%
    mutate(percent = 100 * n / sum(n)) %>%
    ungroup()
  
  capital_totals <- capital_plot_data %>%
    group_by(q3a_text) %>%
    summarise(total = sum(n), max_percent = max(percent))
  
  output$capital_plot <- renderPlot({
    capital_colors <- vf_colors[names(vf_colors) %in% unique(capital_plot_data$q78_text)]
    
    ggplot(capital_plot_data, aes(x = q3a_text, y = percent, group = q78_text, color = q78_text)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      geom_text(data = capital_totals,
                aes(x = q3a_text, y = max_percent + 3, label = paste0("n = ", total)),
                inherit.aes = FALSE, size = 4, fontface = "bold") +
      labs(
        x = "Employee Size",
        y = "Percentage of Responses",
        color = "Capital Use"
      ) +
      scale_color_manual(values = capital_colors, na.translate = FALSE) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Top startup challenges plot
  output$challenge_plot <- renderPlot({
    filtered <- challenges_data()
    
    top_challenges <- filtered %>%
      summarise(across(all_of(challenge_vars), ~ sum(. %in% 1:3, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "count") %>%
      arrange(desc(count)) %>%
      slice_head(n = 6) %>%
      pull(variable)
    
    challenge_data <- filtered %>%
      select(q3a_text, all_of(top_challenges)) %>%
      pivot_longer(cols = -q3a_text, names_to = "variable", values_to = "value") %>%
      filter(value %in% 1:3) %>%
      mutate(label = label_map2[variable]) %>%
      group_by(label) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(pct = round(100 * count / sum(count), 1),
             label_text = paste0(count, " (", pct, "%)"))
    
    ggplot(challenge_data, aes(x = reorder(label, -count), y = count, fill = label)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label_text), vjust = -0.5, size = 4, fontface = "bold") +
      scale_fill_manual(values = vf_colors[names(vf_colors) %in% challenge_data$label], guide = "none") +
      labs(
        x = "Challenge",
        y = "Number of Responses"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Top business stressors plot
  output$stressor_plot <- renderPlot({
    stress_data <- challenges_data() %>%
      pivot_longer(cols = q76_1:q76_10, names_to = "q76_code", values_to = "rank") %>%
      filter(rank %in% c(1, 2)) %>%
      inner_join(stressor_labels, by = "q76_code") %>%
      count(Stressor, sort = TRUE) %>%
      slice_head(n = 3)
    
    ggplot(stress_data, aes(x = reorder(Stressor, n), y = n, fill = Stressor)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_text(aes(label = n), hjust = -0.1, fontface = "bold") +
      scale_fill_manual(values = vf_colors[names(vf_colors) %in% stress_data$Stressor], guide = "none") +
      labs(
        x = "Stressor",
        y = "Number of Selections"
      ) +
      theme_minimal()
  })
  
  
  # --------------------------------------------------
  # Output personas plots
  # --------------------------------------------------
  
  # Persona summary table
  output$persona_table <- renderTable({
    data <- persona_filtered()
    total <- nrow(data)
    
    if (total == 0) {
      return(tibble(
        `Persona Type` = "No Data Available",
        `Description` = "There are no respondents matching this filter.",
        `% of Group` = "-"
      ))
    }
    
    if (input$hiring_filter == "Yes") {
      return(tibble(
        `Persona Type` = "Growth-Oriented Hires",
        `Description` = "Strategic scalers with increasing revenue",
        `% of Group` = "100%"
      ))
      
    } else if (input$hiring_filter == "No") {
      # Group the first three aspiration types as "aspirational"
      aspirational <- data %>% 
        filter(aspirations %in% c("Corporate", "Mid-Size", "Small Business")) %>% 
        nrow()
      
      solo <- data %>% filter(aspirations == "Solo Entrepreneur") %>% nrow()
      known_total <- aspirational + solo
      
      aspirational_pct <- round(aspirational / known_total * 100)
      solo_pct <- 100 - aspirational_pct
      
      return(tibble(
        `Persona Type` = c("Aspirational Hires", "Solo Sustainers"),
        `Description` = c("Want to grow, but no hiring plan", 
                          "Prefer to stay as Solo Entrepreneur"),
        `% of Group` = c(paste0(aspirational_pct, "%"), paste0(solo_pct, "%"))
      ))
      
    } else if (input$hiring_filter == "Unknown") {
      return(tibble(
        `Persona Type` = "Unknown Plan",
        `Description` = "Unsure about planning to hire in the next 12 months",
        `% of Group` = "100%"
      ))
    } else {
      return(tibble(
        `Persona Type` = "All Respondents",
        `Description` = "Complete profile of all microbusinesses",
        `% of Group` = "100%"
      ))
    }
  })
  
  # --------------------------------------------------
  # Plots
  # --------------------------------------------------
  
  # Age boxplot
  output$age_boxplot <- renderPlot({
    ggplot(persona_filtered(), aes(x = Hiring_plans_new, y = age, fill = Hiring_plans_new)) +
      geom_boxplot() +
      stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)), 
                   vjust = -0.5, color = "black", fontface = "bold") +
      scale_fill_manual(values = vf_colors) +
      labs(x = "", y = "Age") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Business age boxplot
  output$business_age_boxplot <- renderPlot({
    ggplot(persona_filtered(), aes(x = Hiring_plans_new, y = Business_Age, fill = Hiring_plans_new)) +
      geom_boxplot() +
      stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)), 
                   vjust = -0.5, color = "black", fontface = "bold") +
      scale_fill_manual(values = vf_colors) +
      labs(x = "", y = "Business Age (Years)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Gender distribution
  output$gender_pie <- renderPlot({
    gender_data <- persona_filtered() %>%
      filter(!is.na(Gender), !Gender %in% c("Prefer not to answer", "Unknown")) %>%
      count(Hiring_plans_new, Gender) %>%
      group_by(Hiring_plans_new) %>%
      mutate(percent = n/sum(n)*100,
             label = paste0(n, "\n(", round(percent, 0), "%)"))
    
    gender_colors <- c("Male" = "#2B50C4", "Female" = "#843DC6", "Non-binary" = "#21ACDB")
    
    ggplot(gender_data, aes(x = "", y = n, fill = Gender)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      facet_wrap(~Hiring_plans_new) +
      geom_text(aes(label = label), 
                position = position_stack(vjust = 0.5), 
                size = 4, color = "white", fontface = "bold") +
      scale_fill_manual(values = gender_colors) +
      labs(fill = "Gender") +
      theme_void() +
      theme(strip.text = element_text(face = "bold", size = 12),
            legend.position = "bottom")
  })
  
  # Race distribution
  output$race_bar <- renderPlot({
    race_data <- persona_filtered() %>%
      filter(!is.na(Race), !Race %in% c("Unknown", "Prefer not to answer")) %>%
      count(Hiring_plans_new, Race) %>%
      group_by(Race) %>%
      mutate(percent = n/sum(n)*100)
    
    race_colors <- c("White" = "#55DBD5", "Black or African American" = "#8647DC",
                     "Hispanic or Latino" = "#726EFF", "Asian" = "#843DC6")
    
    ggplot(race_data, aes(x = Race, y = n, fill = Hiring_plans_new)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = ifelse(n > 100, paste0(n, "\n(", round(percent, 0), "%)"), "")),
                position = position_stack(vjust = 0.5), size = 3.5, fontface = "bold") +
      scale_fill_manual(values = vf_colors) +
      labs(x = "", y = "Count", fill = "Hiring Plan") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Revenue trend
  output$revenue_trend <- renderPlot({
    rev_data <- persona_filtered() %>%
      filter(!is.na(q11c_text), !q11c_text %in% c("Unknown", "Don't know")) %>%
      count(Hiring_plans_new, q11c_text) %>%
      group_by(q11c_text) %>%
      mutate(percent = n/sum(n)*100)
    
    ggplot(rev_data, aes(x = q11c_text, y = n, fill = Hiring_plans_new)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(n, "\n(", round(percent, 0), "%)")), 
                position = position_stack(vjust = 0.5), 
                size = 3.5, fontface = "bold") +
      scale_fill_manual(values = vf_colors) +
      labs(x = "Revenue Trend", y = "Count", fill = "Hiring Plan") +
      theme_minimal()
  })
  
  # Weekly hours worked
  output$hours_worked <- renderPlot({
    hours_data <- persona_filtered() %>%
      filter(!is.na(Weekly_Hours),
             !Weekly_Hours %in% c("Don't know", "Prefer not to answer")) %>%
      count(Hiring_plans_new, Weekly_Hours)
    
    hours_order <- c("0 hours", "1-10 hours", "11-20 hours", "21-30 hours",
                     "31-40 hours", "41-50 hours", "51 hours or more")
    hours_data$Weekly_Hours <- factor(hours_data$Weekly_Hours, levels = hours_order)
    
    ggplot(hours_data, aes(x = Weekly_Hours, y = n, fill = Hiring_plans_new)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = ifelse(n > 30, n, "")),
                position = position_stack(vjust = 0.5), size = 3.5, fontface = "bold") +
      scale_fill_manual(values = vf_colors) +
      labs(x = "Hours Worked", y = "Count", fill = "Hiring Plan") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Income contribution
  output$income_contribution <- renderPlot({
    income_data <- persona_filtered() %>%
      filter(!is.na(Income_Contribution)) %>%
      mutate(Income_Group = case_when(
        grepl("0-25", Income_Contribution) ~ "0-25%",
        grepl("26-50", Income_Contribution) ~ "26-50%",
        grepl("51-75", Income_Contribution) ~ "51-75%",
        grepl("76-100", Income_Contribution) ~ "76-100%",
        TRUE ~ Income_Contribution
      )) %>%
      count(Hiring_plans_new, Income_Group) %>%
      mutate(label = ifelse(n >= 50, as.character(n), ""))
    
    ggplot(income_data, aes(x = Income_Group, y = n, fill = Hiring_plans_new)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = label),
                position = position_stack(vjust = 0.5),
                size = 3.5, fontface = "bold") +
      scale_fill_manual(values = vf_colors) +
      labs(x = "Income from Business", y = "Count", fill = "Hiring Plan") +
      theme_minimal()
  })
  
  # Business aspirations
  output$aspirations_plot <- renderPlot({
    asp_data <- persona_filtered() %>%
      filter(!is.na(aspirations), !aspirations %in% c("Unknown", "Prefer not to answer")) %>%
      count(Hiring_plans_new, aspirations) %>%
      group_by(aspirations) %>%
      mutate(percent = n / sum(n) * 100) %>%
      ungroup() %>%
      mutate(label = ifelse(n >= 50, paste0(n, "\n(", round(percent, 0), "%)"), ""))
    
    asp_order <- c("Solo Entrepreneur", "Small Business", "Mid-Size", "Corporate")
    asp_data$aspirations <- factor(asp_data$aspirations, levels = asp_order)
    
    ggplot(asp_data, aes(x = aspirations, y = n, fill = Hiring_plans_new)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = label), 
                position = position_stack(vjust = 0.5), 
                size = 3.5, fontface = "bold") +
      scale_fill_manual(values = vf_colors) +
      labs(x = "Aspiration Level", y = "Count", fill = "Hiring Plan") +
      theme_minimal() +
      theme(axis.text.x = element_text(hjust = 1))
  })
  
  # Key statistics display
  # Enhanced key statistics display with 2 additional variables
  output$persona_stats <- renderUI({
    req(persona_filtered())
    
    stats <- persona_filtered() %>%
      summarise(
        `Median Age` = median(age, na.rm = TRUE),
        `Median Business Age` = median(Business_Age, na.rm = TRUE),
        `Most Common Gender` = names(which.max(table(Gender))),
        `Most Common Race` = names(which.max(table(Race))),
        `Most Common Weekly Hours` = {
          hours <- na.omit(Weekly_Hours)
          if(length(hours) > 0) names(which.max(table(hours))) else NA
        },
        `Avg Revenue Trend` = {
          trends <- na.omit(q11c_text)
          if(length(trends) > 0) names(which.max(table(trends))) else NA
        },
        `Primary Income Source` = {
          income <- na.omit(Income_Contribution)
          if(length(income) > 0) names(which.max(table(income))) else NA
        },
        `Most Common Business Aspiration` = {
          asp <- na.omit(aspirations)
          if(length(asp) > 0) names(which.max(table(asp))) else NA
        }
      )
    
    # Format the statistics for display
    tagList(
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          p(style = "font-size: 16px;", 
            strong("Median Age: "), round(stats$`Median Age`, 1), " years"),
          p(style = "font-size: 16px;", 
            strong("Median Business Age: "), round(stats$`Median Business Age`, 1), " years"),
          p(style = "font-size: 16px;", 
            strong("Most Common Gender: "), stats$`Most Common Gender`),
          p(style = "font-size: 16px;", 
            strong("Most Common Race: "), stats$`Most Common Race`),
          p(style = "font-size: 16px;", 
            strong("Weekly Hours: "), stats$`Most Common Weekly Hours`),
          p(style = "font-size: 16px;", 
            strong("Average Revenue Trend: "), stats$`Avg Revenue Trend`),
          p(style = "font-size: 16px;", 
            strong("Business Income Distribution: "), stats$`Primary Income Source`),
          p(style = "font-size: 16px;", 
            strong("Business Aspiration: "), stats$`Most Common Business Aspiration`)
      ),
      br(),
      p(em("Note: Statistics calculated for currently filtered data only."),
        style = "font-size: 12px; color: #666;")
    )
  })
 
  
  # --------------------------------------------------
  # Output Predictive Insights
  # -------------------------------------------------- 
  output$reg_summary <- renderPrint({
    kable(summary_table %>% select(Display, Estimate, `Std. Error`, `z value`, `P-value`),
          format = "simple", align = "lcccc", row.names = FALSE)
  })
  
  output$interpretation <- renderUI({
    selected <- input$predictors
    if (length(selected) == 0) {
      HTML("<em>Select one or more predictors to view interpretation.</em>")
    } else {
      combined_odds <- prod(interpretations$OddsRatio[interpretations$Display %in% selected])
      HTML(paste0(
        "<b>Selected Predictors:</b> ", paste(selected, collapse = ", "), "<br><br>",
        "Holding other factors constant, businesses with these characteristics have ",
        round(combined_odds, 2), 
        ifelse(combined_odds > 1, " times higher", " times lower"),
        " odds of planning to hire compared to the reference group."
      ))
    }
  })
  
}

shinyApp(ui, server)
