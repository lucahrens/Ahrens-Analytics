library(shiny)
library(shinyWidgets)
library(quantmod)
library(shinythemes)
library(tidyquant)
library(tidyverse)
library(plogr)
library(DT)
library(shinycssloaders)
library(tidyquant)
library(tidyverse)

css <- "
 .shiny-output-error { visibility: hidden; }
 .shiny-output-error:before {
   visibility: visible;
   content: 'Invalid ticker symbol'; }
 }
 "

options(spinner.color="#7A63FF", spinner.color.background="#1E1E1E", spinner.size=2)

ui <-
fluidPage(  
  HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
          <script async src='https://www.googletagmanager.com/gtag/js?id=UA-168045560-1'></script>
          <script>
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());
          gtag('config', 'UA-168045560-1');
          </script>"
  ),
  HTML("<script data-ad-client='ca-pub-5525514524680116' async src='https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js'></script>"),
  theme = shinytheme("cyborg"),
  #tags$style(type="text/css", css),
  setBackgroundColor("#1E1E1E"),
  tags$head(
    tags$style(
      "#signal{color: #E7E7E7; font-size: 20px}"
    )
  ),
  navbarPage("Ahrens Analytics",
    tabPanel("Search",
      fluidPage(
        # App title ----
        titlePanel(img(src = "Ahrens Analytics Logo.png", height = 200, width = 275)),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
          
          # Sidebar panel for inputs ----
          sidebarPanel(
            textInput("ticker", "Enter a ticker symbol", "SPY")
          ),
          
          # Main panel for displaying outputs ----
          mainPanel(
            textOutput("signal"),
            plotOutput("graph"),
          )
        )
      )
  ),
  tabPanel("Featured",
    fluidPage(
      theme = shinytheme("cyborg"),
      setBackgroundColor('#1E1E1E'),
      titlePanel(img(src = "Ahrens Analytics Logo.png", height = 200, width = 275)),
      mainPanel(
        withSpinner(tableOutput("featured"), type = 3)
      )
    )
  )
)
)


server <- function(input, output) {
  output$signal <- renderText({
    stock <- getSymbols(input$ticker, from = Sys.Date() - 365, to = Sys.Date(), auto.assign = FALSE)
    ma50 <- SMA(Cl(stock), n = 50)
    ma200 <- SMA(Cl(stock), n = 200)
    ma50 <- tail(ma50, n = 7)
    ma200 <- tail(ma200, n = 7)
    movingAverages <- merge(ma50, ma200)
    colnames(movingAverages) <- c("50", "200")
    isCol1GreaterThanCol2 <- function(x) {
      x[j = 1] > x[j = 2]
    }
    
    differences <- apply(movingAverages, MARGIN = 1, FUN = isCol1GreaterThanCol2)
    if(var(differences) == 0) {
      crossover = FALSE;
    } else {
      crossover = TRUE
    }
    
    if (crossover) {
        if (differences[7]) {
          "A golden cross has been recognized!"
        } else {
          "A death cross has been recognized!"
        }
    } else {
      "There has been no noticeable patterns in the last seven trading days."
    }
  })
  
  output$graph <- renderPlot({
    stock <- getSymbols(input$ticker, from = Sys.Date() - 365, to = Sys.Date(), auto.assign = FALSE)
    chartSeries(stock, subset = "last 50 days", theme=chartTheme('black'), TA = "addSMA(n = 200, col = '#7A63FF'); addSMA(n = 50, col = '#74E0EF')")
  })
  
  output$featured <- renderTable({
    tickers <- tq_index("SP500")
    tickers <- tickers$symbol
    
    isGoldenOrDeathCross <- function(x) {
      stock <- getSymbols(x, from = Sys.Date() - 365, to = Sys.Date(), auto.assign = FALSE)
      stock <- na.omit(stock)
      if (nrow(stock) < 200) {
        return("Not enough data to identify a pattern.")
      }
      ma50 <- SMA(Cl(stock), n = 50)
      ma200 <- SMA(Cl(stock), n = 200)
      ma50 <- tail(ma50, n = 7)
      ma200 <- tail(ma200, n = 7)
      movingAverages <- merge(ma50, ma200)
      colnames(movingAverages) <- c("50", "200")
      isCol1GreaterThanCol2 <- function(x) {
        x[j = 1] > x[j = 2]
      }
      
      differences <- apply(movingAverages, MARGIN = 1, FUN = isCol1GreaterThanCol2)
      if(var(differences) == 0) {
        crossover = FALSE;
      } else {
        crossover = TRUE
      }
      
      if (crossover) {
        if (differences[7]) {
          return("Golden cross!")
        } else {
          return("A death cross has been recognized!")
        }
      } else {
        return("There has been no noticeable patterns in the last seven trading days.")
      }
    }

    tickers <- chartr(old =  ".", new =  "-", x = tickers)
    featured <- cbind(tickers, sapply(tickers, FUN = isGoldenOrDeathCross))
    featured
  })
}

shinyApp(ui = ui, server = server)