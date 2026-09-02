
# Load libraries
library(shiny)
library(tidyverse)
library(ggthemes)
library(bslib)
library(DT)
library(stargazer)

# Load Data Frame
Res_Mobility = readRDS("./data/Res_Mobility_2020_2022.rds")

ui <- fluidPage(
  
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  
  # App title
  titlePanel("Determinants of Immigration Into US Counties"),
  
  
  # Tabs
  tabsetPanel(
    
    # Tab 1: Instructions
    tabPanel("Instructions",
             
             tags$div(
               tags$h2("About the App"),
               tags$p("The objective of this Shiny App is to give users the ability to browse US mobility data by county and by year. Users will be able to access county level information such as the proportion of the population that migrated into the county within the year and demographic and socioeconomic characteristics of the county, as well as perform basic statistical analysis to explore what could be driving migration patterns at the county level. Migration at the county level includes migration from other counties in the same state, other states, and other countries."),
               
               tags$h2("About the Usage"),
               tags$ol(
                 tags$li("Choose one of the tabs above to start interacting."),
                 tags$li("The \"Exploratory Data Analysis\" tab allows users to select a specific state and compare counties within that state by year and the selection of key characteristic variables on a summary table and a plot."),
                 tags$li("The \"Statistical Data Analysis\" tab allows users to select a state and run a simple or multiple linear regression analysis based on the users' selection of variables. The results can be interpreted by the observation of a bivariate plot and the model output table."),
                 tags$li("The \"Interactive Data Table\" tab allows users to browse through the entire data frame."),
                 tags$li("This Shiny App depends on several R packages such as {shiny} and {tidyverse}, for instance. Therefore, it's essential that those packages be isntalled on your environment in order for the App to run.")
               ),
               
               tags$h2("About the Variables"),
               tags$ol(
                tags$li("State (Not including Puerto Rico)"),
                tags$li("County"),
                tags$li("Year (2020, 2021, 2022)"),
                tags$li("Percent of Immigration"),
                tags$li("Median Age"),
                tags$li("Median Income"),
                tags$li("Proportion of Male"),
                tags$li("Proportion of Female"),
                tags$li("Proportion Below Poverty Line 100"),
                tags$li("Proportion With Less Than Bachelor's Degree"),
                tags$li("Proportion With Bachelor's Degree"),
                tags$li("Proportion with Graduate Degree")
               )
             )
    ),  #closes tabPanel 1
    
    
    
    # Tab 2: County Comparison
    tabPanel("Exploratory Data Analysis",
             
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("state", "Select State:", 
                             choices = unique(Res_Mobility$State), 
                             selected = unique(Res_Mobility$State)[1]),
                 
                 selectInput("county", "Select County(ies):", 
                             choices = NULL,
                             multiple = TRUE), # Updated dynamically
               
                 #helpText("Select Variables for Summary Table"),
                 checkboxGroupInput("variable2", "Select Variable(s) to Table:", 
                             choices = c(
                               "Percent of Immigration" = "pct_immigrated",
                               "Median Age" = "median_age",
                               "Median Income" = "median_income",
                               "Proportion of Male" = "rate_male",
                               "Proportion of Female" = "rate_female",
                               "Proportion Below Poverty Line 100" = "poverty_rate_100_less",
                               "Proportion with Graduate Degree" = "pop_rate_graduate_degree_higher",
                               "Proportion With Bachelor's Degree" = "pop_rate_bach_degree",
                               "Proportion With Less Than Bachelor's Degree" = "pop_rate_less_than_bach_degree"
                             ),
                             selected = "pct_immigrated"
                 ),
                
                 checkboxGroupInput("year", "Select Year(s)", 
                                    choices = unique(Res_Mobility$Year), 
                                    selected = unique(Res_Mobility$Year)[1]),
                 
                 selectInput("variable", "Select One Variable for Plotting:", 
                             choices = c(
                               "Percent of Immigration" = "pct_immigrated",
                               "Median Age" = "median_age",
                               "Median Income" = "median_income",
                               "Proportion of Male" = "rate_male",
                               "Proportion of Female" = "rate_female",
                               "Proportion Below Poverty Line 100" = "poverty_rate_100_less",
                               "Proportion with Graduate Degree" = "pop_rate_graduate_degree_higher",
                               "Proportion With Bachelor's Degree" = "pop_rate_bach_degree",
                               "Proportion With Less Than Bachelor's Degree" = "pop_rate_less_than_bach_degree"
                             ),
                             selected = "pct_immigrated"
                             ),

               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Summary Table", tableOutput("summary_table")),
                   tabPanel("Plot", plotOutput("variable_plot"))
                 )
               )
             )
             
    ), #closes tabPanel 2

    
    # Tab 3: Regression Analysis
    tabPanel("Statistical Data Analysis",
             
             sidebarLayout(
               sidebarPanel(

                 selectInput("state2", "Select State:", 
                             choices = unique(Res_Mobility$State), 
                             selected = unique(Res_Mobility$State)[1]),  
                 
                 checkboxGroupInput("year2", "Select Year:", 
                                    choices = unique(Res_Mobility$Year), 
                                    selected = unique(Res_Mobility$Year)[1]),
                 
                 selectInput("x_var_bi", "Select (X) Variable for Bivariate Plot:", 
                             choices = c(
                               "Median Age" = "median_age",
                               "Median Income" = "median_income",
                               "Proportion of Male" = "rate_male",
                               "Proportion of Female" = "rate_female",
                               "Proportion Below Poverty Line 100" = "poverty_rate_100_less",
                               "Proportion with Graduate Degree" = "pop_rate_graduate_degree_higher",
                               "Proportion With Bachelor's Degree" = "pop_rate_bach_degree",
                               "Proportion With Less Than Bachelor's Degree" = "pop_rate_less_than_bach_degree"
                             ),
                             selected = "median_income"),
                             
                 selectInput("y_var", "Select Dependent Variable (Y):", 
                             choices = c(
                               "Percent of Immigration" = "pct_immigrated",
                               "Median Age" = "median_age",
                               "Median Income" = "median_income",
                               "Proportion of Male" = "rate_male",
                               "Proportion of Female" = "rate_female",
                               "Proportion Below Poverty Line 100" = "poverty_rate_100_less",
                               "Proportion with Graduate Degree" = "pop_rate_graduate_degree_higher",
                               "Proportion With Bachelor's Degree" = "pop_rate_bach_degree",
                               "Proportion With Less Than Bachelor's Degree" = "pop_rate_less_than_bach_degree"
                             ),
                             selected = "pct_immigrated"
                 ),
                 
                 helpText("For the Regression Analysis"),
                 selectInput("x_vars", "Select Independent Variable(s) (X):", 
                             choices = c(
                               "Median Age" = "median_age",
                               "Median Income" = "median_income",
                               "Proportion of Male" = "rate_male",
                               "Proportion of Female" = "rate_female",
                               "Proportion Below Poverty Line 100" = "poverty_rate_100_less",
                               "Proportion with Graduate Degree" = "pop_rate_graduate_degree_higher",
                               "Proportion With Bachelor's Degree" = "pop_rate_bach_degree",
                               "Proportion With Less Than Bachelor's Degree" = "pop_rate_less_than_bach_degree"
                             ),
                             selected = "median_income",
                             multiple = TRUE),
                 
                 
                 checkboxInput("add_smoother", "Add Linear Smoother", value = TRUE)
                 
               ),

               mainPanel(
                 tabsetPanel(
                   tabPanel("Bivariate Plot", plotOutput("bivariate_plot")),
                   tabPanel("Linear Regression Analysis",
                   verbatimTextOutput(outputId = "regsum"),
                   verbatimTextOutput(outputId = "indprint"),
                   verbatimTextOutput(outputId = "depprint")
                   )
                   
                 )

             )
            
        )      
             
             
    ), #closes tabPanel 3

    
    # Tab 4: data table
    tabPanel("Interactive Data Table", DTOutput("data_table")
             
    ) #closes tabPanel 4
    
        
  ) #closes tabsetPanel
) #closes fluidPage



server <- function(input, output, session) {
  
  # Dynamically update county choices based on state selection
  observe({
    counties <- Res_Mobility %>% filter(State == input$state) %>% pull(County) %>% unique()
    updateSelectInput(session, "county", choices = counties, selected = counties[1])
  })

  
  # Filtered Data
  filtered_data_summ <- reactive({
    Res_Mobility %>% 
      filter(State == input$state,
             County %in% input$county,
             Year %in% input$year)
  })

  # Summary Table
  output$summary_table <- renderTable({
    filtered_data_summ()[c("State", "Year", "County", input$variable2)]
  })


  
  filtered_data_plot <- reactive({
    Res_Mobility %>%
      filter(State == input$state & County %in% input$county)
  })
  

  output$variable_plot <- renderPlot({

      filtered <- filtered_data_plot()
      y_data <- filtered_data_plot()[[input$variable]]

      ggplot( filtered_data_plot(), aes(x = County, y = y_data, fill = Year)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = y_data), size = 5, position = position_dodge(0.9), vjust = -0.5) +
        scale_fill_brewer(palette = "Set2") +  
        theme_classic() +
        labs(title = paste0("Variable: ", "'", input$variable, "'", " in Selected County(ies) for ", input$state, " State"),
             x = "County(ies)",
             y = input$variable) +
        theme(plot.title = element_text(hjust = 0.5))
      
  })
  
  
  # Filtered Data V2
  filtered_data_biv <- reactive({
    Res_Mobility %>% 
      filter(State == input$state2,
             Year %in% input$year2)
  })  
  
  
  # Bivariate Plot
  output$bivariate_plot <- renderPlot({
    
    filtered <- filtered_data_biv()
    x_data <- filtered_data_biv()[[input$x_var_bi]]
    y_data <- filtered_data_biv()[[input$y_var]]

    if (length(input$x_vars) > 0) {
      p <- ggplot(filtered, aes(x = x_data, y = y_data, color = Year)) +
        geom_point(size = 3) +
        theme_minimal() +
        labs(title = paste("Bivariate Plot:", input$y_var, "vs.", input$x_var_bi),
             x = input$x_var_bi,
             y = input$y_var) +
        theme(plot.title = element_text(hjust = 0.5))
      
        if (input$add_smoother) {
          p <- p + geom_smooth(method = "lm", se = FALSE, color = "red")
        }
      p
    }
    
  })
  

  lm1 <- reactive({
    
    filtered <- filtered_data_biv()
    
    lm(reformulate(input$x_vars, input$y_var), data = filtered)
    
    })
  
  output$depPrint <- renderPrint({input$y_var})
  output$indPrint <- renderPrint({input$x_vars})
  output$regsum <- renderPrint({stargazer(lm1(),  type = "text")})
  

   ## Interactive output table
   output$data_table <- renderDT({
    Res_Mobility
  },
  options = list(pageLength = 10)
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

  