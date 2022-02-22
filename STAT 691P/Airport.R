library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)

not_sel <- "Not Selected"

about_page <- tabPanel(
    title = "About",
    titlePanel("About Us"),
    "UMass Amherst Fall 2021",
    br(),
    "STAT 691P with Erin Conlon",
    br(),
    "R Shiny App for Group 2: Yitian Fu, Connor Kennedy, Michaela LaCasse",
    br(),
    br(),
    "Designed for airport flights datasets used in assignments",
    br(),
    br(),
    "Last Update: 4 December 2021"
)

main_page <- tabPanel(
    title = "EDA",
    titlePanel("Exploratory Data Analysis"),
    sidebarLayout(
        sidebarPanel(
            title = "Inputs",
            fileInput("csv_input", "Select Flight Data File to Import", accept = ".csv"),
            selectInput("var_1", "Variable 1 (Numerical)", choices = c(not_sel)),
            selectInput("var_2", "Variable 2", choices = c(not_sel)),
            br(),
            actionButton("run_button", "Run Analysis", icon = icon("play"))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    title = "Visualization",
                    plotOutput("plot_1")
                ),
                tabPanel(
                    title = "Summary Statistics",
                    fluidRow(
                        column(width = 4, strong(textOutput("var_1_title"))),
                        column(width = 4, strong(textOutput("var_2_title")))
                    ),
                    fluidRow(
                        column(width = 4, tableOutput("var_1_summary_table")),
                        column(width = 4, tableOutput("var_2_summary_table"))
                    ),
                    fluidRow(
                        column(width = 12, strong("2-Variable Statistics"))
                    ),
                    fluidRow(
                        column(width = 12, tableOutput("combined_summary_table"))
                    )
                    
                )
            )
        )
    )
)

type <- function(var_2){
    if(var_2 == "carrier" | var_2 == "origin" | var_2 == "orstate" |
       var_2 == "dest" | var_2 == "deststate"){
        var_2_type = "character"
    }
    else if(var_2 == "month" | var_2 == "day" | var_2 == "depart" | 
            var_2 == "delay" | var_2 == "duration" | var_2 == "distance"){
        var_2_type = "integer"
    }
    else{
        var_2_type = NA
    }
}

draw_plot_1 <- function(data_input, var_1, var_2){
    var_2_type = type(var_2)
    if(var_2!=not_sel & var_2_type == "character"){
        data_input[,(var_2):= as.factor(data_input[,get(var_2)])]
    }
    # 2 variable plots
    if(var_1 != not_sel & var_2 != not_sel){
        if(var_2_type == "integer"){
            ggplot(data = data_input,
                   aes_string(x = var_1, y = var_2)) +
                geom_point() +
                theme_light() + 
                scale_x_continuous(expand = c(0.01, 0.01)) + 
                scale_y_continuous(expand = c(0.01, 0.01))
        }
        else if(var_2_type == "character"){
            ggplot(data = data_input,
                   aes_string(x = var_1, y = var_2)) +
                geom_boxplot() +
                theme_light()
        }
    }
    # 1 variable plots
    else if(var_1 != not_sel & var_2 == not_sel){
        ggplot(data = data_input,
               aes_string(x = var_1)) +
            geom_histogram(fill = "grey", color = "black") +
            theme_light() + 
            scale_x_continuous(expand = c(0.01, 0.01)) + 
            scale_y_continuous(expand = c(0.01, 0.01))

    }
    else if(var_1 == not_sel & var_2 != not_sel){
        if(var_2_type == "character"){
            ggplot(data = data_input,
                   aes_string(x = var_2)) +
                geom_bar(fill = "grey", color = "black") +
                theme_light() +
                scale_y_continuous(expand = c(0.01, 0.01)) +
                coord_flip()
        }
        else if(var_2_type == "integer"){
            ggplot(data = data_input,
                   aes_string(x = var_2)) +
                geom_histogram(fill = "grey", color = "black") +
                theme_light() + 
                scale_x_continuous(expand = c(0.01, 0.01)) + 
                scale_y_continuous(expand = c(0.01, 0.01))
        }
    }
}

create_var_1_table <- function(data_input, var_1){
    if(var_1 != not_sel){
        col <- data_input[,get(var_1)]
        statistic <- c("mean", "median", "standard deviation", 
                       "25th percentile", "75th percentile")
        value <- c(round(mean(col),2), round(median(col),2), round(sd(col),2),
                   round(quantile(col, 0.25),2), round(quantile(col, 0.75),2))
        data.table(statistic, value)
    }
}

create_var_2_table <- function(data_input, var_2){
    var_2_type = type(var_2)
    if(var_2 != not_sel & var_2_type == "character"){
        freq_tbl <- data_input[,.N, by = get(var_2)]
        freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
        freq_tbl
    }
    else if(var_2 != not_sel & var_2_type == "integer"){
        col <- data_input[,get(var_2)]
        statistic <- c("mean", "median", "standard deviation",
                       "25th percentile", "75th percentile")
        value <- c(round(mean(col),2), round(median(col),2), round(sd(col),2),
                   round(quantile(col, 0.25),2), round(quantile(col, 0.75),2))
        data.table(statistic, value)
    }
}

create_combined_table <- function(data_input, var_1, var_2){
    var_2_type = type(var_2)
    if(var_1 != not_sel & var_2 != not_sel & var_2_type == "character"){
        res_tbl <- data_input[,.(mean = mean(get(var_1))), by = var_2]
    }
    else if(var_1 != not_sel & var_2 != not_sel & var_2_type == "integer"){
        res_tbl <- data.table(
            statistic = c("correlation"),
            value = c(cor(
                data_input[,get(var_1)],
                data_input[,get(var_2)])))
    }
    else(return("Not Selected"))
    return(res_tbl)
}

ui <- navbarPage(
    title = "Airport Data Analysis",
    theme = shinytheme('superhero'),
    main_page,
    about_page
)

server <- function(input, output){
    
    options(shiny.maxRequestSize=10*1024^2) 
    
    data_input <- reactive({
        req(input$csv_input)
        fread(input$csv_input$datapath)
    })
    
    observeEvent(data_input(),{
        choices <- c(not_sel,names(data_input()))
        updateSelectInput(inputId = "var_1", choices = choices)
        updateSelectInput(inputId = "var_2", choices = choices)
    })
    
    var_1 <- eventReactive(input$run_button,input$var_1)
    var_2 <- eventReactive(input$run_button,input$var_2)
    
    # plot
    
    plot_1 <- eventReactive(input$run_button,{
        draw_plot_1(data_input(), var_1(), var_2())
    })
    
    output$plot_1 <- renderPlot(plot_1())
    
    # 1-d summary tables
    
    output$var_1_title <- renderText(paste("Var 1:", var_1()))
    
    var_1_summary_table <- eventReactive(input$run_button,{
        create_var_1_table(data_input(), var_1())
    })
    
    output$var_1_summary_table <- renderTable(var_1_summary_table(),
                                              colnames = F)
    
    output$var_2_title <- renderText(paste("Var 2:", var_2()))
    
    var_2_summary_table <- eventReactive(input$run_button,{
        create_var_2_table(data_input(), var_2())
    })
    
    output$var_2_summary_table <- renderTable(var_2_summary_table(),
                                              colnames = F)
    
    # 2-d summary tables
    
    combined_summary_table <- eventReactive(input$run_button,{
        create_combined_table(data_input(), var_1(), var_2())
    })
    
    output$combined_summary_table <- renderTable(combined_summary_table())
    
}

shinyApp(ui = ui, server = server)