####################################
# Statistical Learning & Data Mining Project #
# Morning Group 17 #
#   #
####################################

# 

# Load R packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
#install_github("timelyportfolio/sweetalertR")
#library(sweetalertR)

# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('patchwork') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation

# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('vroom') # input/output
library('skimr') # overview
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('purrr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('fuzzyjoin') # data wrangling

# specific visualisation
library('alluvial') # visualisation
library('ggrepel') # visualisation
library('ggforce') # visualisation
library('ggridges') # visualisation
library('gganimate') # animations
library('GGally') # visualisation
library('ggthemes') # visualisation
library('wesanderson') # visualisation
library('kableExtra') # display

# Date + forecast
library('lubridate') # date and time
library('forecast') # time series analysis
#library('prophet') # time series analysis
library('timetk') # time series analysis

# Interactivity
library('crosstalk')
library('plotly')

# parallel
library('foreach')
library('doParallel')
library('shiny')
library('shinydashboard')
library('shinydashboardPlus')
library('plotly')
library('ggplot2')

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                useShinyjs(),
                navbarPage(
                  theme = "cerulean",  
                  "Walmart Sales",
                  tabPanel("Select Data",
                           sidebarPanel(
                             tags$h3("Select the states, stores, categories and departments"),
                             br(),
                             br(),
                             pickerInput("stores", "Choose Stores:",
                                         list(`California` = list("CA_1", "CA_2", "CA_3", "CA_4"),
                                              `Texas` = list("TX_1", "TX_2", "TX_3"),
                                              `Wisconsin` = list("WI_1", "WI_2", "WI_3")),
                                         multiple = TRUE
                             ), # Select input
                             br(),
                             br(),
                             radioButtons("num_prods", "Product selection type",
                                          choiceNames = c("Enter single product","Select all products from a particular department"),
                                          choiceValues = c(1,2)),
                             #                             submitButton("Change", icon("")),
                             helpText("Change product selections?"),
                             submitButton("Change", icon("redo")),
                             br(),
                             br(),
                             br(),
                             br(),
                             conditionalPanel(condition = "input.num_prods == 1",
                                              textInput("product", "Enter Product:", "")
                             ),
                             conditionalPanel(condition = "input.num_prods == 2",
                                              pickerInput("depts", "Choose Departments:",
                                                          list(`Foods` = list("Foods_1", "Foods_2", "Foods_3"),
                                                               `Hobbies` = list("Hobbies_1", "Hobbies_2"),
                                                               `Household` = list("Household_1", "Household_2")),
                                                          multiple = TRUE)
                             ),
                             br(),
                             br(),
                             tags$h4("Confirm Selections?"),
                             submitButton("Submit", icon("sync")),
                           ), # sidebarPanel
                           mainPanel(
                             h1("Selections for Analysis"),
                             br(),
                             br(),
                             br(),
                             h4("States & Stores"),
                             textOutput("storeout"),
                             br(),
                             br(),
                             br(),
                             h4("Categories & Departments"),
                             textOutput("deptout"),
                             br(),
                             br(),
                             br(),
                             h4("Product"),
                             verbatimTextOutput("productout")
                             
                           ) # mainPanel
                           
                  ), # Select tabpanel
                  tabPanel("Data Visualizations",
                           h1("Aggregated sales over the timespan"),
                           plotlyOutput("plot", width="100%", height=700),
                           h1("Product Sales trend per category"),
                           bscols(plotlyOutput("productPlotP1", width="100%", height=600),plotlyOutput("productPlotP2", width="100%", height=600)),
                           h1("Product Sales trend per store"),
                           plotlyOutput("productPlotP3", width="100%", height=600),
                           h1("Product Sales headmap"),
                           plotlyOutput("productPlot2P2", width="100%", height=600)
                  ), # Visualizations tabPanel
                  
                  tabPanel("Sales Forecast",
                           h1("Net Sales Trend for Selected Filters"),
                           plotOutput("forcastplot_monthly", width="100%", height=600),
                           h1("Sales Forcasting for Selected Filters"),
                           plotOutput("forcastplot", width="100%", height=600)
                  ), # Forecast tabPanel
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  path <- "/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/"
  
  train <- vroom(str_c(path,'sales_train_validation.csv'), delim = ",", col_types = cols())
  prices <- vroom(str_c(path,'sell_prices.csv'), delim = ",", col_types = cols())
  calendar <- read_csv(str_c(path,'calendar.csv'), col_types = cols())
  sample_submit <- vroom(str_c(path,'sample_submission.csv'), delim = ",", col_types = cols())
  
  extract_ts <- function(df){
    min_date <- date("2011-01-29")
    df %>%
      select(id, starts_with("d_")) %>%  
      pivot_longer(starts_with("d_"), names_to = "dates", values_to = "sales") %>%
      mutate(dates = as.integer(str_remove(dates, "d_"))) %>% 
      mutate(dates = min_date + dates - 1) %>% 
      mutate(id = str_remove(id, "_validation"))
  }
  
  output$plot <- renderPlotly({
    store_ids = if(is.null(input$stores)) unique(train$store_id) else if(input$stores == "") unique(train$store_id) else input$stores
    dept_id = if(is.null(input$depts)) unique(train$dept_id) else if(input$depts == "") unique(train$dept_id) else input$depts
    product = if(is.null(input$product)) unique(train$id) else if(input$product == "") unique(train$id) else input$product

    train <- train %>% filter(store_id %in% c(store_ids) & dept_id %in% c(dept_id) & id %in% c(product))
    
    foo <- train %>% 
      summarise_at(vars(starts_with("d_")), sum) %>%
      mutate(id = 1)
    
    bar <- extract_ts(foo)
    
    gg <- bar %>% 
      ggplot(aes(dates, sales)) +
      geom_line(col = "blue") +
      theme_tufte() +
      labs(x = "Date", y = "Sales", title = "All aggregate sales")
    
    ggplotly(gg, dynamicTicks = TRUE)
  })
  
  output$productPlotP1 <- renderPlotly({
    store_ids = if(is.null(input$stores)) unique(train$store_id) else if(input$stores == "") unique(train$store_id) else input$stores
    dept_id = if(is.null(input$depts)) unique(train$dept_id) else if(input$depts == "") unique(train$dept_id) else input$depts
    product = if(is.null(input$product)) unique(train$id) else if(input$product == "") unique(train$id) else input$product

    train <- train %>% filter(store_id %in% c(store_ids) & dept_id %in% c(dept_id) & id %in% c(product))
    
    foo <- train %>%
      group_by(cat_id) %>% 
      summarise_at(vars(starts_with("d_")), sum) %>% 
      rename(id = cat_id)
    
    bar <- train %>%
      group_by(store_id) %>% 
      summarise_at(vars(starts_with("d_")), sum) %>% 
      rename(id = store_id)
    
    p1 <- extract_ts(foo) %>% 
      mutate(month = month(dates),
             year = year(dates)) %>% 
      group_by(month, year, id) %>% 
      summarise(sales = sum(sales),
                dates = min(dates)) %>% 
      ungroup() %>% 
      filter(str_detect(as.character(dates), "..-..-01")) %>% 
      filter(dates != max(dates)) %>% 
      ggplot(aes(dates, sales, col = id)) +
      geom_line() +
      theme_hc() +
      theme(legend.position = "none") +
      labs(title = "Sales Trend per Category", x = "Timespan", y = "Product Sales")
    
    p1
  })
  
  output$productPlotP2 <- renderPlotly({
    store_ids = if(is.null(input$stores)) unique(train$store_id) else if(input$stores == "") unique(train$store_id) else input$stores
    dept_id = if(is.null(input$depts)) unique(train$dept_id) else if(input$depts == "") unique(train$dept_id) else input$depts
    product = if(is.null(input$product)) unique(train$id) else if(input$product == "") unique(train$id) else input$product

    train <- train %>% filter(store_id %in% c(store_ids) & dept_id %in% c(dept_id) & id %in% c(product))
    
    foo <- train %>%
      group_by(cat_id) %>% 
      summarise_at(vars(starts_with("d_")), sum) %>% 
      rename(id = cat_id)
    
    bar <- train %>%
      group_by(store_id) %>% 
      summarise_at(vars(starts_with("d_")), sum) %>% 
      rename(id = store_id)
    
    p2 <- train %>% 
      count(cat_id) %>% 
      ggplot(aes(cat_id, n, fill = cat_id)) +
      geom_col() +
      theme_hc() +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(size = 7)) +
      labs(x = "", y = "", title = "Aggregated sales per Category")
    
    p2
  })
  
  output$productPlotP3 <- renderPlotly({
    store_ids = if(is.null(input$stores)) unique(train$store_id) else if(input$stores == "") unique(train$store_id) else input$stores
    dept_id = if(is.null(input$depts)) unique(train$dept_id) else if(input$depts == "") unique(train$dept_id) else input$depts
    product = if(is.null(input$product)) unique(train$id) else if(input$product == "") unique(train$id) else input$product

    train <- train %>% filter(store_id %in% c(store_ids) & dept_id %in% c(dept_id) & id %in% c(product))
    
    foo <- train %>%
      group_by(cat_id) %>% 
      summarise_at(vars(starts_with("d_")), sum) %>% 
      rename(id = cat_id)
    
    bar <- train %>%
      group_by(store_id) %>% 
      summarise_at(vars(starts_with("d_")), sum) %>% 
      rename(id = store_id)
    
    p3 <- extract_ts(bar) %>% 
      mutate(month = month(dates),
             year = year(dates)) %>% 
      group_by(month, year, id) %>% 
      summarise(sales = sum(sales),
                dates = min(dates)) %>% 
      ungroup() %>% 
      filter(str_detect(as.character(dates), "..-..-01")) %>% 
      filter(dates != max(dates)) %>% 
      mutate(state_id = str_sub(id, 1, 2)) %>% 
      ggplot(aes(dates, sales, col = id)) +
      geom_line() +
      theme_hc() +
      theme(legend.position = "bottom") +
      labs(title = "Sales trend per store", x = "Timespan", y = "Product Sales", col = "Store ID") +
      facet_wrap(~state_id)
    
    p3
  })
  
  output$productPlot2P2 <- renderPlotly({
    store_ids = if(is.null(input$stores)) unique(train$store_id) else if(input$stores == "") unique(train$store_id) else input$stores
    dept_id = if(is.null(input$depts)) unique(train$dept_id) else if(input$depts == "") unique(train$dept_id) else input$depts
    product = if(is.null(input$product)) unique(train$id) else if(input$product == "") unique(train$id) else input$product

    train <- train %>% filter(store_id %in% c(store_ids) & dept_id %in% c(dept_id) & id %in% c(product))
    
    foo <- train %>% 
      summarise_at(vars(starts_with("d_")), sum) %>% 
      mutate(id = 1)
    
    bar <- extract_ts(foo) %>% 
      filter(!str_detect(as.character(dates), "-12-25"))
    
    loess_all <- predict(loess(bar$sales ~ as.integer(bar$dates - min(bar$dates)) + 1, span = 1/2, degree = 1))
    
    bar <- bar %>% 
      mutate(loess = loess_all) %>% 
      mutate(sales_rel = sales - loess)
    
    p2 <- bar %>% 
      mutate(wday = wday(dates, label = TRUE, week_start = 1),
             month = month(dates, label = TRUE),
             year = year(dates)) %>% 
      group_by(wday, month, year) %>% 
      summarise(sales = sum(sales_rel)/1e3) %>%
      ggplot(aes(month, wday, fill = sales)) +
      geom_tile() +
      labs(x = "Months", y = "week day", fill = "Sales per 1k") +
      scale_fill_distiller(palette = "Spectral") +
      theme_hc()
    
    p2
  })
  
  output$forcastplot_monthly <- renderPlot({
    store_ids = if(is.null(input$stores)) unique(train$store_id) else if(input$stores == "") unique(train$store_id) else input$stores
    dept_id = if(is.null(input$depts)) unique(train$dept_id) else if(input$depts == "") unique(train$dept_id) else input$depts
    product = if(is.null(input$product)) unique(train$id) else if(input$product == "") unique(train$id) else input$product

    train <- train %>% filter(store_id %in% c(store_ids) & dept_id %in% c(dept_id) & id %in% c(product))
    
    foo <- train %>% 
      summarise_at(vars(starts_with("d_")), sum) %>%
      mutate(id = 1)
    
    bar <- extract_ts(foo)
    
    bar$month = lubridate::month(bar$dates)
    bar$year = lubridate::year(bar$dates)
    bar$week = lubridate::week(bar$dates)
    bar_Monthly <- bar %>% group_by(year,month) %>% summarize(MonthlySales = sum(sales)) %>% as.data.frame
    
    bar_Monthly = ts(bar_Monthly$MonthlySales,start = 2011,frequency = 12)
    
    autoplot(bar_Monthly, ylab = "Total Sales for selected filters", 
             xlab = "monthly for the period 2011 – June 2016", 
             main = "total net sales for selected filters")
    
  })
  
  output$forcastplot <- renderPlot({
    store_ids = if(is.null(input$stores)) unique(train$store_id) else if(input$stores == "") unique(train$store_id) else input$stores
    dept_id = if(is.null(input$depts)) unique(train$dept_id) else if(input$depts == "") unique(train$dept_id) else input$depts
    product = if(is.null(input$product)) unique(train$id) else if(input$product == "") unique(train$id) else input$product

    train <- train %>% filter(store_id %in% c(store_ids) & dept_id %in% c(dept_id) & id %in% c(product))
    
    foo <- train %>% 
      summarise_at(vars(starts_with("d_")), sum) %>%
      mutate(id = 1)
    
    bar <- extract_ts(foo)
    
    bar$month = lubridate::month(bar$dates)
    bar$year = lubridate::year(bar$dates)
    bar$week = lubridate::week(bar$dates)
    bar_Monthly <- bar %>% group_by(year,month) %>% summarize(MonthlySales = sum(sales)) %>% as.data.frame
    
    bar_Monthly = ts(bar_Monthly$MonthlySales,start = 2011,frequency = 12)
    
    bar_auto_arima <- auto.arima(bar_Monthly, seasonal = TRUE, stepwise = FALSE,approximation = FALSE, lambda = "auto")
    
    plot(forecast(bar_auto_arima, h = 180), ylab = "Total Sales for the selected filters")
  })
  
  output$storeout <- renderPrint(input$stores)
  output$deptout <- renderPrint(input$depts)
  output$productout <- renderText({
    paste(input$product)})
  observeEvent(input$num_prods, {
    reset("product")
    reset("depts")
  })
  observeEvent(input$Submit, {
    print("Submit Clicked")
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
