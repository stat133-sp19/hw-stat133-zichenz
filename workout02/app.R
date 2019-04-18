#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Three Modalities of Saving-Investing"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(4,
         sliderInput("init",
                     "Initial Amount",
                     min = 0,
                     max = 100000,
                     step = 500,
                     value = 1000),
         sliderInput("annual",
                     "Annual Contribution",
                     min = 0,
                     max = 50000,
                     step = 500,
                     value = 2000)
     ),
     column(4,
         sliderInput("return",
                     "Return Rate (in %)",
                     min = 0,
                     max = 20,
                     step = 0.1,
                     value = 5),
         sliderInput("growth",
                     "Growth Rate (in %)",
                     min = 0,
                     max = 20,
                     step = 0.1,
                     value = 2)
     ),
     column(4,
         sliderInput("year",
                     "Years",
                     min = 0,
                     max = 50,
                     step = 1,
                     value = 20),
         selectInput('facet',
                     'Facet?',
                     c('Yes', 'No'))
     )
   ),
      
      
      # Show a plot of the generated distribution
         h4("Timelines"),
         plotOutput("distPlot"),
         h4("Balances"),
         verbatimTextOutput('displayTable')
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     #' @title future_value 
     #' @description calculate the future value of present money, accounting for interest rate
     #' @param amount amount of money
     #' @param rate interest rate
     #' @param years number of years in the future
     #' @return the future value
     future_value <- function(amount, rate, years){
       fv <- amount * (1 + rate) ^ years
       return(fv)
     }
    
     #' @title annuity
     #' @description calculate the future value of saving
     #' @param contrib contribution of saving
     #' @param rate interest rate
     #' @param years number of years in the future
     #' @return the future value of annuity
     annuity <- function(contrib, rate, years){
       fva <- contrib * (((1+rate)^years - 1)/rate)
       return(fva)
     }
    
     #' @title growing annuity
     #' @description calculate the future value of growing annuity
     #' @param contrib contribution of saving
     #' @param rate interest rate
     #' @param growth growth rate of savings
     #' @param years number of years in the future
     #' @return result
     growing_annuity <- function(contrib, rate, growth, years){
       fvga <- contrib * (((1+rate)^years - (1+growth)^years)/(rate - growth))
       return(fvga)
     }
    
     
     #constructing the data frame 'modalities' of three modes
     modalities <- data.frame(year = 0:input$year)
     no_contrib <- c()
     fixed_contrib <- c()       
     growing_contrib <- c()
     for(i in 0:input$year){
       no_contrib <- c(no_contrib, future_value(input$init, input$return/100, i))
       fixed_contrib <- c(fixed_contrib, future_value(input$init, input$return/100, i) + annuity(input$annual, input$return/100, i))
       growing_contrib <- c(growing_contrib, future_value(input$init, input$return/100, i) + growing_annuity(input$annual, input$return/100, input$growth/100, i))
     }       
     modalities$no_contrib = no_contrib
     modalities$fixed_contrib = fixed_contrib
     modalities$growing_contrib = growing_contrib
     
     
     no_df <- data.frame(year = rep(0:input$year))
     fixed_df <- data.frame(year = rep(0:input$year))
     growing_df <- data.frame(year = rep(0:input$year))
     no <- c()
     fixed <- c()
     growing <- c()
     for (i in 0:input$year){
       no <- c(no, future_value(input$init, input$return/100, i))
       fixed <- c(fixed, future_value(input$init, input$return/100, i) + annuity(input$annual, input$return/100, i))
       growing <- c(growing, future_value(input$init, input$return/100, i) + growing_annuity(input$annual, input$return/100, input$growth/100, i))
     }
     no_df$money = no
     fixed_df$money = fixed
     growing_df$money = growing
     no_df$variable <- rep('no_contrib', times = input$year+1)
     fixed_df$variable <- rep('fixed_contrib', times = input$year+1)
     growing_df$variable <- rep('growing_contrib', times = input$year+1)
     savings <- rbind(no_df, fixed_df, growing_df)
     savings$variable <- factor(savings$variable, levels = c('no_contrib', 'fixed_contrib', 'growing_contrib'))
     

     #plotting   
     if(input$facet == 'No'){
       ggplot(modalities)+
         geom_line(aes(x=year, y=no_contrib, color = 'no_contrib'))+
         geom_point(aes(x=year, y=no_contrib, color = 'no_contrib'),size=0.7)+
         geom_line(aes(x=year,  y=fixed_contrib, color = 'fixed_contrib'))+
         geom_point(aes(x=year, y=fixed_contrib, color = 'fixed_contrib'),size=0.7)+
         geom_line(aes(x=year, y=growing_contrib, color = 'growing_contrib'))+
         geom_point(aes(x=year, y=growing_contrib, color = 'growing_contrib'),size=0.7)+
         labs(x='year', y='value', title='Three modes of investing')+
         scale_color_manual(values = c('no_contrib'='red', 'fixed_contrib' = 'green', 'growing_contrib' ='blue'), 
                            name = 'variable', limits = c('no_contrib', 'fixed_contrib', 'growing_contrib'))
     }
     
     else{
       ggplot(savings, aes(x=year))+
         geom_line(aes(y=money, col = variable))+
         geom_point(aes(y=money, col = variable))+
         geom_area(aes(y=money, fill = variable, col = variable), alpha = 0.5)+
         theme_bw()+
         labs(x='year', y='value', title='Three modes of investing')+
         facet_wrap(~variable)
     }
     #displaying balance table
   })
   output$displayTable <- renderPrint({
     modalities
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

