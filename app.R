library(shinydashboard)
library(shiny)
library(xts)
library(quantmod)
library(plotly)
library(ggplot2)
library(ggfortify)
library(quadprogXT)
library(MASS)
source("project_script.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Risk Management",
                  titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem(" Introduction", tabName = "intro", icon = icon("chart-line")),
      menuItem(" Time Series", tabName = "ts", icon = icon("chart-line")),
      menuItem(" Results", tabName = "results", icon = icon("chart-line")),
      menuItem(" About us", tabName = "about", icon = icon("linkedin"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem("intro",
              fluidRow(
                box(
                  width = 12,
                  column(width = 2),
                  column(width = 8,
                         titlePanel(h1("Portfolio Management and Optimisation", align = "center"))),
                  column(width = 2)
                ),
                fluidPage(
                  width = 12,
                  box(
                    width = 6,
                    column(img(src = "https://www.matsonmoney.com/wp-content/uploads/2019/10/Academic-Advisory-Board-Matson-Money-Harry-M-Markowitz-1024x683.jpg",
                          align = "center", style = paste0("width: 100%; height: ", "30em", ";")),
                      width = 7),
                    column(
                      h1("Harry Markovitz", align = "center"),
                      p("Harry Max Markowitz (born August 24, 1927) is an American economist who received the 1989 
                      John von Neumann Theory Prize and the 1990 Nobel Memorial Prize in Economic Sciences. Markowitz is a professor of finance at 
                        the Rady School of Management at the University of California, San Diego (UCSD). He is best 
                        known for his pioneering work in modern portfolio theory, studying the effects of asset risk, 
                        return, correlation and diversification on probable investment portfolio returns. 
                        "),
                      p("Modern portfolio theory (MPT), or mean-variance analysis, is a mathematical framework for 
                        assembling a portfolio of assets such that the expected return is maximized for a given level 
                        of risk. It is a formalization and extension of diversification in investing, the idea that 
                        owning different kinds of financial assets is less risky than owning only one type. Its key 
                        insight is that an asset's risk and return should not be assessed by itself, but by how it 
                        contributes to a portfolio's overall risk and return. It uses the variance of asset prices as 
                        a proxy for risk."),
                      p("MPT assumes that investors are risk averse, meaning that given two portfolios that offer the same expected return, 
                      investors will prefer the less risky one. Thus, an investor will take on increased risk only if compensated by higher expected returns. 
                      Conversely, an investor who wants higher expected returns must accept more risk. The exact trade-off will not be the same for all investors. 
                      Different investors will evaluate the trade-off differently based on individual risk aversion characteristics. The implication 
                      is that a rational investor will not invest in a portfolio if a second portfolio exists with a more favorable risk-expected 
                      return profile—i.e., if for that level of risk an alternative portfolio exists that has better expected returns.
                      Under the model: Portfolio return is the proportion-weighted combination of the constituent assets' returns. 
                      Portfolio return volatility σ is a function of the correlations ρij of the component 
                        assets, for all asset pairs (i, j). The volatility gives insight into the risk which is associated with the investment. 
                        The higher the volatility, the higher the risk."),
                      p("An investor can reduce portfolio risk (especially σ) simply by holding combinations of instruments 
                      that are not perfectly positively correlated (correlation coefficient − 1 ≤ ρ i j < 1). In other 
                      words, investors can reduce their exposure to individual asset risk by holding a diversified portfolio of assets. 
                      Diversification may allow for the same portfolio expected return with reduced risk. The mean-variance framework for 
                      constructing optimal investment portfolios was first posited by Markowitz and has since been reinforced and improved 
                      by other economists and mathematicians who went on to account for the limitations of the framework. 
                      If all the asset pairs have correlations of 0—they are perfectly uncorrelated—the portfolio's return variance 
                      is the sum over all assets of the square of the fraction held in the asset times the asset's return variance (and 
                      the portfolio standard deviation is the square root of this sum). 
                      If all the asset pairs have correlations of 1—they are perfectly positively correlated—then 
                      the portfolio return’s standard deviation is the sum of the asset returns’ standard deviations weighted by the fractions 
                      held in the portfolio. For given portfolio weights and given standard deviations of asset returns, the 
                      case of all correlations being 1 gives the highest possible standard deviation of portfolio return."),
                           width = 5)
                    ),
                  box(
                    width = 6,
                    column(img(src = "https://imageio.forbes.com/blogs-images/danielfisher/files/2017/04/0417_inv-frontier-dick-robert-michaud_650x455.jpg?format=jpg&width=960",
                               align = "center", style = paste0("width: 100%; height: ", "30em", ";")),
                           width = 8),
                    column(
                      h1("Richard and Robert Michaud", align="center"),
                      p("Dr. Richard Michaud is the President and Chief Executive Officer at New Frontier. He earned a Ph.D. in Mathematics from Boston
                      University and has taught investment management at Columbia University. He is the author of Efficient Asset Management: 
                      A Practical Guide to Stock Portfolio Optimization and Asset Allocation (1998, 2nd ed. 2008 with Robert Michaud), 
                      a CFA Research Monograph (1999) on Global Asset Management, and numerous academic and research articles available 
                      on www.ssrn.com and www.researchgate.net.  He is co-holder of four U.S. patents in portfolio optimization and 
                      asset management, a Graham and Dodd Scroll winner for his work on optimization, a former editorial board member of 
                      the Financial Analysts Journal, associate editor of the Journal Of Investment Management, and former director of the “Q” Group."),
                      p("Robert Michaud is the co-holder of four U.S. patents in portfolio optimization and asset 
                        management and is the Chief Investment Officer at New Frontier. He holds a Masters in Mathematics from Boston University 
                        and pursued a Ph.D. in finance from the Anderson School of Management at the University of California, Los Angeles 
                        before joining New Frontier. His research interests include portfolio theory, risk models, empirical asset pricing, and 
                        international finance. He is co-author of Efficient Asset Management: A Practical Guide to Stock 
                        Portfolio Optimization and Asset Allocation, (2nd ed. Oxford University Press, 2008) and research articles in refereed journals."),
                      p("In investment portfolio construction, an investor or analyst is faced with determining which asset classes, such as 
                        domestic fixed income, domestic equity, foreign fixed income, and foreign equity, to invest in and what proportion of 
                        the total portfolio should be of each asset class. Harry Markowitz (1959) first described a method for constructing a 
                        portfolio with optimal risk/return characteristics. His portfolio optimization method finds the minimum risk portfolio 
                        with a given expected return. Because the Markowitz or Mean-Variance Efficient Portfolio is calculated from the sample 
                        mean and covariance, which are likely different from the population mean and covariance, the resulting investment 
                        portfolio may allocate too much weight to assets with better estimated than true risk/return characteristics. To 
                        account for the uncertainty of the sample estimates, a financial analyst can create many alternative efficient 
                        frontiers based on resampled versions of the data. Each resampled dataset will result in a different set of 
                        Markowitz efficient portfolios. These efficient frontiers of portfolios can then be averaged to create a resampled 
                        efficient frontier. The appropriate compromise between the investor's Risk aversion and desired return will then 
                        guide the financial analyst to choose a portfolio from the set of resampled efficient frontier portfolios. 
                        Since such a portfolio is different from the Markowitz efficient portfolio it will have suboptimal risk/return 
                        characteristics with respect to the sample mean and covariance, but optimal characteristics when averaged 
                        over the many possible values of the unknown true mean and covariance. (Michaud, 1998) Resampled Efficiency is 
                        covered by U. S. patent #6,003,018, patent pending worldwide. New Frontier Advisors, LLC, has exclusive worldwide licensing rights."),
                      width = 4)
                  )
                )
              )
      ),
      tabItem("ts",
              fluidRow(
                width = 12,
                box(
                  width = 4,
                  title = "SMI Stocks",
                  selectInput("select_ticker", "Select Index: ",
                              c("ABB" = "ABBN.SW",
                              "Credit Suisse" = "CSGN.SW",
                              "Geberit" = "GEBN.SW",
                              "Givaudan" = "GIVN.SW",
                              "Holcim" = "HOLN.SW",
                              "Logitech" = "LOGN.SW",
                              "Lonza Group" = "LONN.SW",
                              "Nestlé" = "NESN.SW",
                              "Novartis" = "NOVN.SW",
                              "Partners Group" = "PGHN.SW",
                              "Richemont" = "CFR.SW",
                              "Roche" = "ROG.SW",
                              "Sika" = "SIKA.SW",
                              "Sonova" = "SOON.SW",
                              "Swisscom" = "SCMN.SW",
                              "SwissLife" = "SLHN.SW",
                              "SwissRe" = "SREN.SW",
                              "UBS" = "UBSG.SW",
                              "Zurich" = "ZURN.SW")),
                  div(tableOutput("key_figures"), align = "center")
                ),
                box(
                  width = 8,
                  title = "Stock prices from 2007 to today",
                  div(tableOutput("ticker_data"), align = "center", style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                )
              ),
              fluidRow(
                width = 12,
                box(
                  width = 4,
                  plotlyOutput("open")
                ),
                box(
                  width = 4,
                  plotlyOutput("close")
                ),
                box(
                  width = 4,
                  plotlyOutput("return")
                )
              ),
              fluidRow(
                width = 12,
                box(
                  width = 4,
                  title ="Shapiro-Wilk Normality test",
                  div(verbatimTextOutput("shapiro_test"), align = "center")
                ),
                box(
                  width = 8,
                  div(plotOutput("qqplot"), align = "center")
                )
              ),
              fluidRow(
                width = 12,
                box(
                  width = 12,
                  div(verbatimTextOutput("covariance"))
                )
              )
      ),
      tabItem("results",
              fluidRow(
                box(
                  title = "Select your assets:",
                  width = 12,
                  column(
                    width = 4,
                    checkboxInput("ABBN", label = "ABB", value = FALSE),
                    checkboxInput("GIVN", label = "Givaudan", value = FALSE),
                    checkboxInput("LONN", label = "Lonza Group", value = TRUE),
                    checkboxInput("PHGN", label = "Partners Group", value = FALSE),
                    checkboxInput("SIKA", label = "Sika", value = TRUE),
                    checkboxInput("SLHN", label = "SwissLife", value = FALSE),
                    checkboxInput("ZURN", label = "Zurich", value = FALSE)
                  ),
                  column(
                    width = 4,
                    checkboxInput("CSGN", label = "Credit Suisse", value = FALSE),
                    checkboxInput("HOLN", label = "Holcim", value = FALSE),
                    checkboxInput("NESN", label = "Nestlé", value = TRUE),
                    checkboxInput("CFR", label = "Richemont", value = FALSE),
                    checkboxInput("SOON", label = "Sonova", value = TRUE),
                    checkboxInput("SREN", label = "SwissRe", value = FALSE)
                  ),
                  column(
                    width = 4,
                    checkboxInput("GEBN", label = "Geberit", value = FALSE),
                    checkboxInput("LOGN", label = "Logitech", value = FALSE),
                    checkboxInput("NOVN", label = "Novartis", value = FALSE),
                    checkboxInput("ROG", label = "Roche", value = TRUE),
                    checkboxInput("SCMN", label = "Swisscom", value = FALSE),
                    checkboxInput("UBSG", label = "UBS", value = FALSE)
                  )
                )
              ),
              fluidRow(
                box(
                  width = 4,
                  div(numericInput("initial_capital", label = "Initial Capital",value = 10000, min = 1000, max = 10000000, step = 1000)),
                  h2("Portfolio performances"),
                  div(verbatimTextOutput("portfolio_stats"), align = "center"),
                ),
                box(
                  title = "Markovitz and Michaud Portfolio frontier analysis",
                  width = 8,
                  div(plotOutput("frontier"), align = "center")
                )
              )
      ),
      tabItem("about",
              fluidRow(fluidRow(column(h1("About Us", align = "center"), width = 12)),
                       fluidRow(column(width = 1),
                                box(img(src = "https://media.licdn.com/dms/image/D4E03AQExcpYim60THQ/profile-displayphoto-shrink_200_200/0/1667482302683?e=1689811200&v=beta&t=ySH0Es6IMMGyJd6Z2KUG4SQFZwpInbGYMuWbo4gfo2o",
                                        align = "center", style = paste0("width: 100%; height: ", "30em", ";")),
                                    h4("Goran Nikolov", align = "center"),
                                    h4("Student MSc Applied Information and Data Science at HSLU"),
                                    h6("https://www.linkedin.com/in/goran-nikolov-1628a5210/"),
                                    width = 4),
                                column(width = 2),
                                box(img(src = "https://media.licdn.com/dms/image/C4D03AQHdsQZ43lJd0Q/profile-displayphoto-shrink_800_800/0/1619939966754?e=1686182400&v=beta&t=iIVlj2x-Li5MUFOiDqbNHi5n7GztWTvZIQ2bkSub_38",
                                        align = "center", style = paste0("width: 100%; height: ", "30em", ";")),
                                    h4("Daniele Buson"),
                                    h4("Student MSc Applied Information & Data Science at HSLU"),
                                    h6("https://www.linkedin.com/in/daniele-buson-325471168/"),
                                    h6("https://github.com/DanieleBuson"),
                                    width = 4),
                                column(width = 1),
                       ))
      )
    )))

server <- function(input, output) {
  
  output$key_figures <- renderTable({
    value <- getSymbols(Symbols = input$select_ticker, auto.assign = FALSE)
    
    value_return <- diff.xts(value[,4])[2:length(value[,4])]/value[1:length(value[,4])-1,4]
    expected_return <- mean(value_return)
    
    risk <- sd(value_return)
    
    sharpe <- expected_return/risk
    
    key_figures <- data.frame(Daily.Expected.Return = round(expected_return, 5), 
                          Volatility = round(risk,5),
                          DailySharpeRatio = round(sharpe,5))
    
    key_figures
  }, digits = 5)
  
  output$ticker_data <- renderTable({
    value <- getSymbols(Symbols = input$select_ticker, auto.assign = FALSE)
    
    ticker_data <- as.data.frame(value)
    colnames(ticker_data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    
    ticker_data
  }, digits = 8)
  
  output$open <- renderPlotly({
    value <- getSymbols(Symbols = input$select_ticker, auto.assign = FALSE)
    
    value_plot <- autoplot(value[,1], colour = "navy") +
      theme(
        legend.title = element_text(size = 10),
        plot.title = element_text(color = "black", size = 20)
      ) +
      ggtitle(paste(input$select_ticker, "Open Prices", sep = " ")) +
      ylab("Open Price (in US dollars)") +
      xlab("Time")
    ggplotly(value_plot)
  })
  
  output$close <- renderPlotly({
    value <- getSymbols(Symbols = input$select_ticker, auto.assign = FALSE)
    
    value_plot <- autoplot(value[,4], colour = "navy") +
      theme(
        legend.title = element_text(size = 10),
        plot.title = element_text(color = "black", size = 20)
      ) +
      ggtitle(paste(input$select_ticker, "Close Prices", sep = " ")) +
      ylab("Close Price (in US dollars)") +
      xlab("Time")
    ggplotly(value_plot)
  })
  
  output$return <- renderPlotly({
    value <- getSymbols(Symbols = input$select_ticker, auto.assign = FALSE)
    value_return <- diff.xts(value[,4])[2:length(value[,4])]/value[1:length(value[,4])-1,4] 
    
    value_plot <- autoplot(value_return, colour = "navy") +
      geom_abline(slope = 0, intercept = mean(value_return), col = "yellow") +
      theme(
        legend.title = element_text(size = 10),
        plot.title = element_text(color = "black", size = 20)
      ) +
      ggtitle(paste(input$select_ticker, "Daily Returns", sep = " ")) +
      ylab("Daily Return (in US dollars)") +
      xlab("Time")
    ggplotly(value_plot)
  })
  
  output$shapiro_test <- renderPrint({
    value <- getSymbols(Symbols = input$select_ticker, auto.assign = FALSE)
    value_return <- diff.xts(value[,4])[2:length(value[,4])]/value[1:length(value[,4])-1,4]

    df_return <- as.data.frame(value_return)

    print(shapiro.test(df_return[,1]))
  })

  output$qqplot <- renderPlot({
    value <- getSymbols(Symbols = input$select_ticker, auto.assign = FALSE)
    value_return <- diff.xts(value[,4])[2:length(value[,4])]/value[1:length(value[,4])-1,4]

    normal_z <- rnorm(length(value_return[,1]), mean = 0, sd = 1)

    qqplot(y = value_return, x = normal_z, col = "navy")
    qqline(y= value_return, col = "red")
    title("QQPlot Returns vs Z normal distribution")

  })
  
  output$covariance <- renderPrint({
    SMI_stocks <- data.frame(
      ABB = as.vector(ABB_returns),
      CreditSuisse = as.vector(CreditSuisse_returns),
      Geberit = as.vector(Geberit_returns),
      Givaudan = as.vector(Givaudan_retunrs),
      Holcim = as.vector(Holcim_retunrs),
      Logitech = as.vector(Logitech_returns),
      LonzaGroup = as.vector(LonzaGroup_returns),
      Nestle = as.vector(Nestle_returns),
      Novartis = as.vector(Novartis_returns),
      PartnersGroup = as.vector(PartnersGroup_returns),
      Richemont = as.vector(Richemont_returns),
      Roche = as.vector(Roche_returns),
      Sika = as.vector(Sika_returns),
      Sonova = as.vector(Sonova_returns),
      Swisscom = as.vector(Swisscom_returns),
      SwissLife = as.vector(SwissLife_returns),
      SwissRe = as.vector(SwissRe_returns),
      UBS = as.vector(UBS_returns),
      Zurich = as.vector(Zurich_returns))
    SMI_stocks
    
    print(cov(SMI_stocks, use = "pairwise.complete.obs"))
  })
  
  output$portfolio_stats <- renderPrint({
    returns_portfolio <- xts()
    i = 0
    if (input$ABBN){
      returns_portfolio <- merge.xts(returns_portfolio, ABB_returns)
      i = 1 + i
    }
    if (input$CSGN){
      returns_portfolio <- merge.xts(returns_portfolio, CreditSuisse_returns)
      i = 1 + i
    }
    if (input$GEBN){
      returns_portfolio <- merge.xts(returns_portfolio, Geberit_returns)
      i = 1 + i
    }
    if (input$GIVN){
      returns_portfolio <- merge.xts(returns_portfolio, Givaudan_retunrs)
      i = 1 + i
    }
    if (input$HOLN){
      returns_portfolio <- merge.xts(returns_portfolio, Holcim_retunrs)
      i = 1 + i
    }
    if (input$LOGN){
      returns_portfolio <- merge.xts(returns_portfolio, Logitech_returns)
      i = 1 + i
    }
    if (input$LONN){
      returns_portfolio <- merge.xts(returns_portfolio, LonzaGroup_returns)
      i = 1 + i
    }
    if (input$NESN){
      returns_portfolio <- merge.xts(returns_portfolio, Nestle_returns)
      i = 1 + i
    }
    if (input$NOVN){
      returns_portfolio <- merge.xts(returns_portfolio, Novartis_returns)
      i = 1 + i
    }
    if (input$PHGN){
      returns_portfolio <- merge.xts(returns_portfolio, PartnersGroup_returns)
      i = 1 + i
    }
    if (input$CFR){
      returns_portfolio <- merge.xts(returns_portfolio, Richemont_returns)
      i = 1 + i
    }
    if (input$ROG){
      returns_portfolio <- merge.xts(returns_portfolio, Roche_returns)
      i = 1 + i
    }
    if (input$SIKA){
      returns_portfolio <- merge.xts(returns_portfolio, Sika_returns)
      i = 1 + i
    }
    if (input$SOON){
      returns_portfolio <- merge.xts(returns_portfolio, Sonova_returns)
      i = 1 + i
    }
    if (input$SCMN){
      returns_portfolio <- merge.xts(returns_portfolio, Swisscom_returns)
      i = 1 + i
    }
    if (input$SLHN){
      returns_portfolio <- merge.xts(returns_portfolio, SwissLife_returns)
      i = 1 + i
    }
    if (input$SREN){
      returns_portfolio <- merge.xts(returns_portfolio, SwissRe_returns)
      i = 1 + i
    }
    if (input$UBSG){
      returns_portfolio <- merge.xts(returns_portfolio, UBS_returns)
      i = 1 + i
    }
    if (input$ZURN){
      returns_portfolio <- merge.xts(returns_portfolio, Zurich_returns)
      i = 1 + i
    }
    if (i == 0){
      print("Please select at least a checkbox")
    }
    else{
      ## first of all the 
      ew_wgt <- c()
      
      for (i in 1:length(returns_portfolio[1,])){
        ew_wgt[i] <- 1/length(returns_portfolio[1,])
      }
      
      mean_vector <- c()
      
      for (i in 1:length(returns_portfolio[1,])){
        mean_vector[i] <- mean(returns_portfolio[, i])
      }
      
      expected_return <- sum(ew_wgt*mean_vector)
      
      var_portfolio <- 0
      # calculation of the standard deviation of this portfolio
      for (i in 1:length(returns_portfolio[1,])){
        for (j in 1:length(returns_portfolio[1,])){
          
          if (names(returns_portfolio)[i] == names(returns_portfolio)[j]){
            var_portfolio <- var_portfolio + (ew_wgt[i]**2) * var(returns_portfolio[ , i])
            # print(var_portfolio)
          }
          else{
            var_portfolio <- var_portfolio + (ew_wgt[i] * ew_wgt[j]) * var(x = returns_portfolio[ , i], y = returns_portfolio[ , j])
            # print(var_portfolio)
          }
          
        }
      }
      
      risk_portfolio <- as.numeric(sqrt(var_portfolio))
      
      sharpe_portfolio <- as.numeric(expected_return)/(risk_portfolio)
      
      df <- data.frame(Returns = as.numeric(expected_return)*input$initial_capital,
                       Risk = risk_portfolio,
                       Sharpe = sharpe_portfolio)
      
      print("Equally Weighted Portfolio")
      print(df)
      
      ## Second Portfolio
      portfolio_numbers <- 1000
      
      # weight_matrix <- matrix(nrow = portfolio_numbers, ncol = length(returns_portfolio[1,]))
      weight_matrix <- data.frame(matrix(nrow = 0, ncol = length(returns_portfolio[1,])))
      
      # vector("numeric", length = portfolio_numbers) 
      port_ret <- c()
      port_risk <- c()
      port_sharpe <- c()
      
      for (i in 1:portfolio_numbers){
        
        temp_weight <- runif(length(returns_portfolio[1,]), min = 0, max = 1)
        norm_temp_weight <- temp_weight/sum(temp_weight)
        
        weight_matrix[i, ] <- norm_temp_weight[1:length(norm_temp_weight)]
        
        port_daily_returns <- sum(temp_weight*mean_vector)
        # port_return_value <- (((1 + port_daily_returns)**252) - 1)
        port_ret[i] <- port_daily_returns
        
        port_risk_value <- 0
        returns_portfolio_temp <- as.data.frame(returns_portfolio)
        # calculation of the standard deviation of this portfolio
        for (j in 1:length(returns_portfolio_temp)){
          for (k in 1:length(returns_portfolio_temp)){
            if (names(returns_portfolio_temp)[j] == names(returns_portfolio_temp)[k]){
              port_risk_value <- port_risk_value + (norm_temp_weight[j]**2) * var(returns_portfolio_temp[ , j])
            }
            else{
              port_risk_value <- port_risk_value + (norm_temp_weight[j] * norm_temp_weight[k]) * var(x = returns_portfolio_temp[ , j], y = returns_portfolio_temp[ , k])
            }
          }
        }
        
        port_risk[i] <- sqrt(port_risk_value)
        
        port_sharpe_ratio <- (port_daily_returns/(sqrt(port_risk_value)))
        port_sharpe[i] <- port_sharpe_ratio
      }
      
      library(tibble)
      library(timetk)
      total_portfolio <- tibble(Returns = port_ret*input$initial_capital,
                                Risk = port_risk,
                                Sharpe = port_sharpe)
      
      weight_matrix <- tk_tbl(weight_matrix)
      colnames(weight_matrix) <- colnames(returns_portfolio)
      
      total_portfolio <- tk_tbl(cbind(weight_matrix, total_portfolio))
      
      minimum_variance_portfolio <- total_portfolio[which.min(total_portfolio$Risk), ]
      print("Empirical Minimum Volatility Portfolio")
      print(minimum_variance_portfolio)
      
      
      maximum_sharpe_ratio_portfolio <- total_portfolio[which.max(total_portfolio$Sharpe),]
      print("Empirical Maximum Sharpe Portfolio")
      print(maximum_sharpe_ratio_portfolio)
    }
  })
  
  output$frontier <- renderPlot({
    returns_portfolio <- xts()
    i = 0
    if (input$ABBN){
      returns_portfolio <- merge.xts(returns_portfolio, ABB_returns)
      i = 1 + i
    }
    if (input$CSGN){
      returns_portfolio <- merge.xts(returns_portfolio, CreditSuisse_returns)
      i = 1 + i
    }
    if (input$GEBN){
      returns_portfolio <- merge.xts(returns_portfolio, Geberit_returns)
      i = 1 + i
    }
    if (input$GIVN){
      returns_portfolio <- merge.xts(returns_portfolio, Givaudan_retunrs)
      i = 1 + i
    }
    if (input$HOLN){
      returns_portfolio <- merge.xts(returns_portfolio, Holcim_retunrs)
      i = 1 + i
    }
    if (input$LOGN){
      returns_portfolio <- merge.xts(returns_portfolio, Logitech_returns)
      i = 1 + i
    }
    if (input$LONN){
      returns_portfolio <- merge.xts(returns_portfolio, LonzaGroup_returns)
      i = 1 + i
    }
    if (input$NESN){
      returns_portfolio <- merge.xts(returns_portfolio, Nestle_returns)
      i = 1 + i
    }
    if (input$NOVN){
      returns_portfolio <- merge.xts(returns_portfolio, Novartis_returns)
      i = 1 + i
    }
    if (input$PHGN){
      returns_portfolio <- merge.xts(returns_portfolio, PartnersGroup_returns)
      i = 1 + i
    }
    if (input$CFR){
      returns_portfolio <- merge.xts(returns_portfolio, Richemont_returns)
      i = 1 + i
    }
    if (input$ROG){
      returns_portfolio <- merge.xts(returns_portfolio, Roche_returns)
      i = 1 + i
    }
    if (input$SIKA){
      returns_portfolio <- merge.xts(returns_portfolio, Sika_returns)
      i = 1 + i
    }
    if (input$SOON){
      returns_portfolio <- merge.xts(returns_portfolio, Sonova_returns)
      i = 1 + i
    }
    if (input$SCMN){
      returns_portfolio <- merge.xts(returns_portfolio, Swisscom_returns)
      i = 1 + i
    }
    if (input$SLHN){
      returns_portfolio <- merge.xts(returns_portfolio, SwissLife_returns)
      i = 1 + i
    }
    if (input$SREN){
      returns_portfolio <- merge.xts(returns_portfolio, SwissRe_returns)
      i = 1 + i
    }
    if (input$UBSG){
      returns_portfolio <- merge.xts(returns_portfolio, UBS_returns)
      i = 1 + i
    }
    if (input$ZURN){
      returns_portfolio <- merge.xts(returns_portfolio, Zurich_returns)
      i = 1 + i
    }
    if (i == 0){
      print("Please select at least a checkbox")
    }
    else{
      ## first of all the 
      ew_wgt <- c()
      
      for (i in 1:length(returns_portfolio[1,])){
        ew_wgt[i] <- 1/length(returns_portfolio[1,])
      }
      
      mean_vector <- c()
      
      for (i in 1:length(returns_portfolio[1,])){
        mean_vector[i] <- mean(returns_portfolio[, i])
      }
      
      expected_return <- sum(ew_wgt*mean_vector)
      
      var_portfolio <- 0
      # calculation of the standard deviation of this portfolio
      for (i in 1:length(returns_portfolio[1,])){
        for (j in 1:length(returns_portfolio[1,])){
          
          if (names(returns_portfolio)[i] == names(returns_portfolio)[j]){
            var_portfolio <- var_portfolio + (ew_wgt[i]**2) * var(returns_portfolio[ , i])
            # print(var_portfolio)
          }
          else{
            var_portfolio <- var_portfolio + (ew_wgt[i] * ew_wgt[j]) * var(x = returns_portfolio[ , i], y = returns_portfolio[ , j])
            # print(var_portfolio)
          }
          
        }
      }
      
      risk_portfolio <- as.numeric(sqrt(var_portfolio))
      
      sharpe_portfolio <- as.numeric(expected_return)/(risk_portfolio)
      
      df <- data.frame(Returns = as.numeric(expected_return)*input$initial_capital,
                       Risk = risk_portfolio,
                       Sharpe = sharpe_portfolio)
      
      print("Equally Weighted Portfolio")
      print(df)
      
      ## Second Portfolio
      portfolio_numbers <- 100
      
      # weight_matrix <- matrix(nrow = portfolio_numbers, ncol = length(returns_portfolio[1,]))
      weight_matrix <- data.frame(matrix(nrow = 0, ncol = length(returns_portfolio[1,])))
      
      # vector("numeric", length = portfolio_numbers) 
      port_ret <- c()
      port_risk <- c()
      port_sharpe <- c()
      
      for (i in 1:portfolio_numbers){
        
        temp_weight <- runif(length(returns_portfolio[1,]), min = 0, max = 1)
        norm_temp_weight <- temp_weight/sum(temp_weight)
        
        weight_matrix[i, ] <- norm_temp_weight[1:length(norm_temp_weight)]
        
        port_daily_returns <- sum(temp_weight*mean_vector)
        # port_return_value <- (((1 + port_daily_returns)**252) - 1)
        port_ret[i] <- port_daily_returns
        
        port_risk_value <- 0
        returns_portfolio_temp <- as.data.frame(returns_portfolio)
        # calculation of the standard deviation of this portfolio
        for (j in 1:length(returns_portfolio_temp)){
          for (k in 1:length(returns_portfolio_temp)){
            if (names(returns_portfolio_temp)[j] == names(returns_portfolio_temp)[k]){
              port_risk_value <- port_risk_value + (norm_temp_weight[j]**2) * var(returns_portfolio_temp[ , j])
            }
            else{
              port_risk_value <- port_risk_value + (norm_temp_weight[j] * norm_temp_weight[k]) * var(x = returns_portfolio_temp[ , j], y = returns_portfolio_temp[ , k])
            }
          }
        }
        
        port_risk[i] <- sqrt(port_risk_value)
        
        port_sharpe_ratio <- (port_daily_returns/(sqrt(port_risk_value)))
        port_sharpe[i] <- port_sharpe_ratio
      }
      
      library(tibble)
      library(timetk)
      total_portfolio <- tibble(Returns = port_ret*input$initial_capital,
                                Risk = port_risk,
                                Sharpe = port_sharpe)
      
      weight_matrix <- tk_tbl(weight_matrix)
      colnames(weight_matrix) <- colnames(returns_portfolio)
      
      total_portfolio <- tk_tbl(cbind(weight_matrix, total_portfolio))
      
      minimum_variance_portfolio <- total_portfolio[which.min(total_portfolio$Risk), ]
      print("Empirical Minimum Volatility Portfolio")
      print(minimum_variance_portfolio)
      
      
      maximum_sharpe_ratio_portfolio <- total_portfolio[which.max(total_portfolio$Sharpe),]
      print("Empirical Maximum Sharpe Portfolio")
      print(maximum_sharpe_ratio_portfolio)
      
      number_assets <- length(returns_portfolio[1,])
      
      covariance_matrix <- (cov(returns_portfolio))
      number_of_points <- 100
      
      set_mu <- seq(min(mean_vector), max(mean_vector), length = number_of_points + 2)
      
      set_mu <- set_mu[1:number_of_points + 1]
      
      
      portfolio_ret <- set_mu
      portfolio_std <- set_mu*0
      portfolio_wgt <- matrix(0,number_of_points, number_assets)
      
      for (i in 1:number_of_points){
        # print(i)
        Dmat <- 2*covariance_matrix
        # print(Dmat)
        dvec <- rep(0,number_assets)
        # print(dvec)
        Amat <- t(rbind(t(rep(1,number_assets)),t(mean_vector),diag(number_assets)))
        # print(Amat)
        bvec <- c(1, set_mu[i], rep(0,number_assets))
        # print(bvec)
        
        m <- solveQPXT(Dmat, dvec, Amat, bvec, meq=2, factorized=FALSE)
        
        portfolio_std[i] <- sqrt(m$value)
        portfolio_wgt[i, ] <- t(m$solution)
        
      }
      
      ########## Michaud #######################################################
      
      resampling_number <- 1000
      
      set_mu <- seq(min(mean_vector), max(mean_vector), length = number_of_points + 2)
      
      set_mu <- set_mu[1:number_of_points + 1]
      
      
      portfolio_ret_re <- set_mu
      portfolio_std_re <- set_mu*0
      portfolio_wgt_re <- matrix(0,number_of_points, number_assets)
      
      for (rr in 1:resampling_number){
        simulated_ts <- mvrnorm(n=120, mean_vector, covariance_matrix)
        mu_sim <- colMeans(simulated_ts)
        cov_sim <- cov(simulated_ts)
        
        mu_set_sim <- seq(min(mu_sim),max(mu_sim),length=(number_of_points+2))
        mu_set_sim <- mu_set_sim[1:number_of_points+1]
        
        port_ret_sim <- mu_set_sim
        port_std_sim <- mu_set_sim*0
        port_wgt_sim <- matrix(0,number_of_points, number_assets)
        
        nn = 0
        for (i in 1:number_of_points){
          # print(i)
          Dmat <- 2*cov_sim
          # print(Dmat)
          dvec <- rep(0,number_assets)
          # print(dvec)
          Amat <- t(rbind(t(rep(1,number_assets)),t(mu_sim),diag(number_assets)))
          # print(Amat)
          bvec <- c(1, mu_set_sim[i], rep(0,number_assets))
          # print(bvec)
          
          m <- solveQPXT(Dmat, dvec, Amat, bvec, meq=2, factorized=FALSE)
          
          # print(m)
          port_std_sim[i] <- sqrt(m$value)
          port_wgt_sim[i, ] <- t(m$solution)
          
        }
        
        portfolio_ret_re <- portfolio_ret_re + port_ret_sim
        portfolio_wgt_re <- portfolio_wgt_re + port_wgt_sim
      }
      
      portfolio_wgt_re <- portfolio_wgt_re/resampling_number
      portfolio_ret_re <- portfolio_ret_re/resampling_number
      
      for (i in 1:number_of_points){
        portfolio_ret_re[i] <- portfolio_wgt_re[i, ]%*%mean_vector
        portfolio_std_re[i] <- sqrt(portfolio_wgt_re[i,]%*%covariance_matrix%*%portfolio_wgt_re[i,])
      }
      
      
      
      ########## Plotting all together #########################################
      
      
      plot(sqrt(diag(covariance_matrix)), mean_vector,
           xlim=c(0.5*min(portfolio_std),1.5*max(portfolio_std)), 
           ylim=c(0.5*min(mean_vector),2.5*max(mean_vector)),
           col = "yellow", lwd = 3,
           xlab = "Risk",
           ylab = "Daily return")
      title("Portfolio Frontier")
      text(sqrt(diag(covariance_matrix)), mean_vector,
           labels=colnames(returns_portfolio), cex= 0.7)
      
      
      # efficient frontier
      lines(portfolio_std,portfolio_ret,col = "navy", lwd = 5)
      lines(portfolio_std_re,portfolio_ret_re,col = "cyan", lwd = 2)
      points(y = df$Return/input$initial_capital, x = df$Risk, col = "green", lwd = 3)
      text(y = df$Return/input$initial_capital, x = df$Risk, labels="EWP", cex = 0.7)
      points(y = minimum_variance_portfolio[1,length(minimum_variance_portfolio)-2]/input$initial_capital, x = minimum_variance_portfolio[1,length(minimum_variance_portfolio)-1], col = "orange", lwd = 3)
      text(y = minimum_variance_portfolio[1,length(minimum_variance_portfolio)-2]/input$initial_capital, x = minimum_variance_portfolio[1,length(minimum_variance_portfolio)-1], labels="MinRisk", cex = 0.7)
      points(y = maximum_sharpe_ratio_portfolio[1,length(minimum_variance_portfolio)-2]/input$initial_capital, x = maximum_sharpe_ratio_portfolio[1,length(minimum_variance_portfolio)-1], col = "red", lwd = 3)
      text(y = maximum_sharpe_ratio_portfolio[1,length(minimum_variance_portfolio)-2]/input$initial_capital, x = maximum_sharpe_ratio_portfolio[1,length(minimum_variance_portfolio)-1], labels="MaxSR", cex = 0.7)
    }
  })

}

shinyApp(ui = ui, server = server)