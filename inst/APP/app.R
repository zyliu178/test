library(ggplot2)
library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(DT)
library(kableExtra)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("province", "What province do you want to look at?",
                choices = unique(cov_china$provinceEnglishName)),
            selectInput("city", "What city do you want to look at?",
                choices = unique(cov_china$cityEnglishName))),
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotOutput("plot_covid")),
                tabPanel("Plotly",plotlyOutput("plot"),
                    verbatimTextOutput("hover"),
                    verbatimTextOutput("click"),
                    verbatimTextOutput("selecting")),
                tabPanel("Table", DT::dataTableOutput("table"))),
        h3("You can select the province and city you want to know about in the selection box at the top. "),
        h3("Then  the first picture and table will automatically proccess the  coronavirus data of your selected city"),
        h3("On the second graph, you can use the mouse to click on the points in the graph to view the values."),
        h3("Or brush on the graph to show multiple points."),
        h3("You also can hover on the graph the see the value of each point."),
        h4("Thank you for using the app!")
)))

server <- function(input, output, session) {
    observeEvent(input$province,{
        updateSelectInput(session, "city",
                          choices = filter(cov_china, 
                                           provinceEnglishName == input$province)$cityEnglishName)
    })
    
    output$plot_covid <- renderPlot({
        cov_china %>% filter(provinceEnglishName == input$province,
                             cityEnglishName == input$city,
                             month <=3) %>%
            ggplot(aes(date, city_confirmedCount)) +
            geom_point(aes(color = "#60B0DD")) +
            geom_smooth(aes(color = "#21618C"), se = FALSE) +
            theme(legend.title = element_blank()) +
            xlab("Date") +
            theme_light() +
            ylab("Total confirmed cases") +
            theme(legend.position = "none")+
            ggtitle(input$city)
    })
    
    output$plot <- renderPlotly({
        p <- cov_total %>% filter(month <=2) %>%
            ggplot(aes(x = Date_reported, y = Cumulative_cases)) + 
            geom_point() +
            geom_smooth(se = FALSE) +
            theme(legend.title = element_blank()) +
            xlab("Date") +
            theme_light() +
            ylab("Total confirmed cases") +
            theme(legend.position = "none") +
            ggtitle("Trend of total confirmed cases in China")
        p <- ggplotly(p) 
        
        p %>% 
            layout(dragmode = "select") %>%
            event_register("plotly_selecting")
    })
    
    output$hover <- renderPrint({
        d <- event_data("plotly_hover")
        if (is.null(d)) "Hover events appear here (unhover to clear)" else d
    })
    
    output$click <- renderPrint({
        d <- event_data("plotly_click")
        if (is.null(d)) "Click to show one point (double-click to clear)" else d
    })
    
    output$selecting <- renderPrint({
        d <- event_data("plotly_selecting")
        if (is.null(d)) "Brush the points to show multiple points (double-click to clear)" else d
    })
    
    output$table <- DT::renderDataTable({
        cov_china %>% filter(provinceEnglishName == input$province,
                                             cityEnglishName == input$city)%>%
            select(date,city_confirmedCount, city_curedCount, city_deadCount) %>%
        DT::datatable(caption = "Accumulated COVID-19 Cases in selected city",
                      colnames = c("Date", "Confirmed", "Recovered", "Death"),
                      options = list(dom = 'Bfrtip',
                                     buttons = list(list(extend = 'colvis', columns = 1:6))),
                      filter = 'top')
           
    })
 }

shinyApp(ui, server)