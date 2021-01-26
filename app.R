#Shiny App : Investment Timeline Estimations
#Author: Najiyullah Sanee

#Inputs: 
#    initamt: Initial Amount
#    an_contrib: Annual Contribution
#    annual_grate: Annual Growth rate
#    high_rate: High Yield Rate
#    fixed_rate: Fixed Income rate
#    equity_rate: US Equity rate
#    high_volat: High Yield volatility
#    fixed_volat: Fixed Income volatility
#    equity_volat: US Equity volatility
#    years: Years of estimaiton
#    seed: Random seed
#    facet: enable/disable facet by investment type
#

#Outputs:
#    Investment plot(s)

library(shiny)
library(ggplot2)
library(reshape2)


#Define UI for application
ui <-  fluidPage(
  titlePanel("Investment Returns Estimations"),
  
  fluidRow(
    column(3, 
           sliderInput("initamount", 
                       label = "Initial Amount:", 
                       min = 1, max = 10000,
                       step = 100,
                       value = 1000),
           
           sliderInput("an_contrib", 
                       label = "Annual Contribution:", 
                       min = 0, max = 5000,
                       step = 100,
                       value = 200),
           
           sliderInput("annual_grate", 
                       label = "Annual Growth Rate (%)", 
                       min = 0,
                       max = 20, 
                       step = 0.1,
                       value = 2),
    ),
    
    column(3, 
           sliderInput("high_rate", 
                       label = "High Yield rate (%)", 
                       min = 0,
                       max = 20, 
                       step = 0.1,
                       value = 2),
           
           sliderInput("fixed_rate", 
                       label = "Fixed Income rate (%)", 
                       min = 0,
                       max = 20, 
                       step = 0.1,
                       value = 5),
           
           sliderInput("equity_rate", 
                       label = "US Equity rate (%)", 
                       min = 0,
                       max = 20, 
                       step = 0.1,
                       value = 10),
    ),
    
    column(3, 
           sliderInput("high_volat", 
                       label = "High Yield volatility (%)", 
                       min = 0,
                       max = 20, 
                       step = 0.1,
                       value = 0.1),
           
           sliderInput("fixed_volat", 
                       label = "Fixed Income volatility (%)", 
                       min = 0,
                       max = 20, 
                       step = 0.1,
                       value = 4.5),
           
           sliderInput("equity_volat", 
                       label = "US Equity volatility (%)", 
                       min = 0,
                       max = 20, 
                       step = 0.1,
                       value = 15),
           
    ),
    
    column(3, 
           sliderInput("years", 
                       label = "Years", 
                       min = 0,
                       max = 50, 
                       step = 1,
                       value = 20),
           
           numericInput("seed", 
                        label = "Random seed", 
                        value = 12345),
           
           selectInput("facet",
                       label = "Facet",
                       c("Yes", "No"),
                       selected = "Yes"),
    )
  ),
  
  hr(),
  
  plotOutput("returnsPlot"),
  
  hr()
)


# Define server logic required to draw plot
server <- function(input, output) {
  
  
  output$returnsPlot <- renderPlot ({
    
    investment_returns <- function (initamt, acontrib, years = 1,  growthrate, anrate, volat) {
      
      #convert integer % input to decimal form
      growthrate <- growthrate/100
      anrate <- anrate/100
      volat <- volat/100
      
      #vector for storing annual returns amounts 
      anreturns <- c()
      
      #initial amount begins at index 1
      anreturns[1] <- initamt
      
      
      
      #loop generate vector of amounts for require years
      if (years == 0) {
        return(anreturns)
        
      } else { 
        #set random seed
        set.seed(input$seed)
        
        for (i in 1:years) {
          #generate annual rate
          anreturn_rate <- rnorm(1, anrate, volat)
          
          #each anreturns entry counted at + 1 index
          anreturns[(i+1)] <- anreturns[i] * (1 + anreturn_rate) + 
            acontrib*(1 + growthrate)^(i-1)
        }
        return(anreturns)
      } 
    }
    
    
    
    
    
    #create vectors with different investment types returns
    high_yield <- investment_returns(input$initamount, 
                                     input$an_contrib, 
                                     input$years, 
                                     input$annual_grate, 
                                     input$high_rate,
                                     input$high_volat)
    
    us_bonds <- investment_returns(input$initamount, 
                                   input$an_contrib, 
                                   input$years, 
                                   input$annual_grate, 
                                   input$fixed_rate, 
                                   input$fixed_volat)
    
    us_stocks <- investment_returns(input$initamount, 
                                    input$an_contrib,
                                    input$years, 
                                    input$annual_grate, 
                                    input$equity_rate, 
                                    input$equity_volat)
    
    
    #combine all returns
    Years <- c(0:input$years)
    
    all_returns <- cbind(Years, high_yield, us_bonds, us_stocks)
    
    #data frame of returns
    returns_data <- setNames(data.frame(Years, high_yield, us_bonds, us_stocks), 
                             c("Years", "high_yield", "us_bonds", "us_stocks" ) )
    
    #group by investment type, organized by Years and Amount
    all_returns_by_year <- melt(returns_data, id=c("Years"),
                                value.name = "Amount")
    
    if (input$facet == "Yes") {
      
      #plot investment returns by  years with facet
      ggplot(data = all_returns_by_year, aes(x = Years, y = Amount, group = variable)) + 
        geom_line(aes(color = variable)) + 
        geom_point(aes(color = variable))+
        geom_area(aes(fill = variable, alpha = 0.4))+
        guides(alpha = FALSE, fill = FALSE)+
        ggtitle("Investment Timeline") + 
        facet_wrap(~variable) +
        labs(color = "Investment Type")+
        theme_bw()
      
    } else {  
      #plot investment returns by  years
      ggplot(data = all_returns_by_year, aes(x = Years, y = Amount, group = variable)) + 
        geom_line(aes(color = variable)) + 
        geom_point(aes(color = variable))+
        ggtitle("Investment Timeline") + 
        labs(color = "Investment Type") +
        theme_bw()
    }
    
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
