#####################################################################################
#Global
library(shiny)
library(ggplot2)
library(dplyr)
library(shinyTime)
library(lubridate)
library(markdown)
library(wordcloud2)
library(tm)


#Get clean data
df10 <- readr::read_csv("./df3.csv")
df11 <- readr::read_csv("./df4.csv")
df15 <- readr::read_csv("./df3_wsentiment.csv")
df16 = left_join(df15,df11, by = 'week')
df.rcx.r1 <- readr::read_csv('df.rcx.r1.csv')
df.rcx.p1 <- readr::read_csv('df.rcx.p1.csv')
df.rcx.r2 <- readr::read_csv('df.rcx.r2.csv')
df.rcx.p2 <- readr::read_csv('df.rcx.p2.csv')


#####################################################################################
#UI
ui <- fluidPage(
  navbarPage(
    "The XRP/Reddit Correlation",
    tabPanel(
      title = 'Main Page',
      fluid = TRUE,
      fluidRow(column(6,
                     includeMarkdown("main_page.rmd")),
               column(6,
                 img(src = './xrp_logo_svg.svg', 
                     style = 'length:50%; width:50%; border:0px;
                            display: block; margin-left: auto; margin-right: auto;')
               )
               ),
      fluidRow(br(),
               column(6),
                column(
                  6,
                  img(src = './reddit.svg', 
                      style = 'length:50%; width:50%; border:0px;
                      display: block; margin-left: auto; margin-right: auto;')
                )
                      
                      )
    ),
    #tabPanel(
      #title = 'Definitions',
      #fluid = TRUE,
      #fluidRow(column(12,
                      #withMathJax(includeMarkdown("definitions.Rmd"))))
    #),
    tabPanel(
      tagList(shiny::icon('chart-line'), "Number of Post vs Price Over Time"),
      sidebarLayout(
        sidebarPanel(
          helpText('Select date range to view
                   number of posts per day and associated closing price.'),
          sliderInput("slider1", label = h3("Date Range"), min = min(df10$date), 
                      max = max(df10$date), value = c(min(df10$date), max(df10$date))),
          fluidRow(
          column(8, verbatimTextOutput("range1"))
          )
        ),
        mainPanel(
          fluidRow(
            column(12,
                   plotOutput("plot1")
            )
            )
          )
          )
        ),
    tabPanel(
      tagList(shiny::icon('chart-line'), "Previous Day's Insight"),
      fluidRow(
        column(12,
               helpText('The following plots and matrix visualize 
                        Reddit post volume and post sentiment with following day price change.')
               ),
        br(),
        fluidRow(
        column(12,
        mainPanel(
          tabsetPanel(
            tabPanel("Number of Posts", img(src = './Price_Change_v_Posts_day.png', 
                                 style = 'length:50%; width:50%; border:0px;
                                 display: block; margin-left: auto; margin-right: auto;')), 
            tabPanel("Sentiment", img(src = './Price_Change_v_Sentiment_day.png', 
                                    style = 'length:50%; width:50%; border:0px;
                                    display: block; margin-left: auto; margin-right: auto;')), 
            tabPanel("Correlation Matrix", img(src = './Rplot01.png', 
                         style = 'length:50%; width:50%; border:0px;
                       display: block; margin-left: auto; margin-right: auto;')),
            tabPanel("Correlation Table (r)", DT::dataTableOutput("table1")),
            tabPanel("Correlation Table (p)", DT::dataTableOutput("table2")),
            tabPanel("Positive Gain Word Cloud", img(src = './wc_day.png', 
                                       style = 'length:50%; width:50%; border:0px;
                                       display: block; margin-left: auto; margin-right: auto;')),
            tabPanel("Negative Gain Word Cloud", img(src = './wc_day_loss.png', 
                                                    style = 'length:50%; width:50%; border:0px;
                                                    display: block; margin-left: auto; margin-right: auto;'))
          )
        ))))),
    tabPanel(
      tagList(shiny::icon('chart-line'), "Previous Week's Insight"),
      fluidRow(
        column(12,
               helpText("The following plots and matrix visualize the preceding week's Reddit post volume 
                and post sentiment with the current week's price change.")
               ),
        br(),
        fluidRow(
          column(12,
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Number of Posts", img(src = './Price_Change_v_Posts_week.png', 
                                                     style = 'length:50%; width:50%; border:0px;
                                                     display: block; margin-left: auto; margin-right: auto;')), 
                     tabPanel("Sentiment", img(src = './Price_Change_v_Sentiment_week.png', 
                                               style = 'length:50%; width:50%; border:0px;
                                               display: block; margin-left: auto; margin-right: auto;')), 
                     tabPanel("Correlation Matrix", img(src = './Rplot02.png', 
                                                        style = 'length:50%; width:50%; border:0px;
                                                        display: block; margin-left: auto; margin-right: auto;')),
                     tabPanel("Correlation Table (r)", DT::dataTableOutput("table3")),
                     tabPanel("Correlation Table (p)", DT::dataTableOutput("table4")),
                     tabPanel("Positive Gain Word Cloud", img(src = './wc_week.png', 
                                                style = 'length:50%; width:50%; border:0px;
                                       display: block; margin-left: auto; margin-right: auto;')),
                     tabPanel("Negative Gain Word Cloud", img(src = './wc_week_loss.png', 
                                                             style = 'length:50%; width:50%; border:0px;
                                                             display: block; margin-left: auto; margin-right: auto;'))
                     )
                     )))))
    )   


)

#####################################################################################
#Server

server <- function(input, output, session) {
 
#################################################################################### 
  output$range1 <- renderPrint({ input$slider1 })

  
  df12 <- reactive({
    df10%>%
      filter(., between(date, input$slider1[1], input$slider1[2]))%>%
      group_by(., date)%>%
      summarise(.,count=n(), price = mean(close))
  })
  
  output$plot1 <- renderPlot({
      ggplot(df12(), aes(x = df12()$date)) +
      geom_line(aes(y = df12()$count, colour = "posts")) +
      geom_line(aes(y = df12()$price*800, colour = "price")) +
      scale_y_continuous(sec.axis = sec_axis(~./800, name = "Price, ($)")) +
      scale_colour_manual(values = c("blue", "red")) +
      labs(y = "Number of Posts, (n)",
           x = "Date",
           colour = "Legend")+
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = c(0.8, 0.9))+
      ggtitle('Number of Posts & Price Over Time')
  })
  #################################################################################### 
  output$range2 <- renderPrint({ input$slider2 })
  
  df13 <- reactive({
    df10%>%
      filter(., between(date, input$slider2[1], input$slider2[2]))
  })

  #################################################################################### 
  output$table1 = DT::renderDataTable({
    df.rcx.r1
  })
  output$table2 = DT::renderDataTable({
    df.rcx.p1
  })
  output$table3 = DT::renderDataTable({
    df.rcx.r2
  })
  output$table4 = DT::renderDataTable({
    df.rcx.p2
  })
}
#########################################################################
#App
shinyApp(ui, server)