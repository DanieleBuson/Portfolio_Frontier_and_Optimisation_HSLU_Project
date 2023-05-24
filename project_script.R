library(quantmod)

############################ Download of main Swiss Index ######################
getSymbols(Symbols = "ABBN.SW", auto.assign = TRUE)
getSymbols(Symbols = "ALC.SW", auto.assign = TRUE)
getSymbols(Symbols = "CSGN.SW", auto.assign = TRUE)
getSymbols(Symbols = "GEBN.SW", auto.assign = TRUE)
getSymbols(Symbols = "GIVN.SW", auto.assign = TRUE)
getSymbols(Symbols = "HOLN.SW", auto.assign = TRUE)
getSymbols(Symbols = "LOGN.SW", auto.assign = TRUE)
getSymbols(Symbols = "LONN.SW", auto.assign = TRUE)
getSymbols(Symbols = "NESN.SW", auto.assign = TRUE)
getSymbols(Symbols = "NOVN.SW", auto.assign = TRUE)
getSymbols(Symbols = "PGHN.SW", auto.assign = TRUE)
getSymbols(Symbols = "CFR.SW", auto.assign = TRUE)
getSymbols(Symbols = "ROG.SW", auto.assign = TRUE)
getSymbols(Symbols = "SIKA.SW", auto.assign = TRUE)
getSymbols(Symbols = "SOON.SW", auto.assign = TRUE)
getSymbols(Symbols = "SLHN.SW", auto.assign = TRUE)
getSymbols(Symbols = "SREN.SW", auto.assign = TRUE)
getSymbols(Symbols = "SCMN.SW", auto.assign = TRUE)
getSymbols(Symbols = "UBSG.SW", auto.assign = TRUE)
getSymbols(Symbols = "ZURN.SW", auto.assign = TRUE)

########### Calculating the daily returns #############
ABB_returns <- diff.xts(ABBN.SW$ABBN.SW.Close)[2:length(ABBN.SW$ABBN.SW.Close)]/ABBN.SW$ABBN.SW.Close[1:length(ABBN.SW$ABBN.SW.Close)-1]
colnames(ABB_returns) <- "ABB"
Alcon_returns <- diff.xts(ALC.SW$ALC.SW.Close)[2:length(ALC.SW$ALC.SW.Close)]/ALC.SW$ALC.SW.Close[1:length(ALC.SW$ALC.SW.Close)-1]
colnames(Alcon_returns) <- "ALC"
CreditSuisse_returns <- diff.xts(CSGN.SW$CSGN.SW.Close)[2:length(CSGN.SW$CSGN.SW.Close)]/CSGN.SW$CSGN.SW.Close[1:length(CSGN.SW$CSGN.SW.Close)-1]
colnames(CreditSuisse_returns) <- "CSGN"
Geberit_returns <- diff.xts(GEBN.SW$GEBN.SW.Close)[2:length(GEBN.SW$GEBN.SW.Close)]/GEBN.SW$GEBN.SW.Close[1:length(GEBN.SW$GEBN.SW.Close)-1]
colnames(Geberit_returns) <- "GEBN"
Givaudan_retunrs <- diff.xts(GIVN.SW$GIVN.SW.Close)[2:length(GIVN.SW$GIVN.SW.Close)]/GIVN.SW$GIVN.SW.Close[1:length(GIVN.SW$GIVN.SW.Close)-1]
colnames(Givaudan_retunrs) <- "GIVN"
Holcim_retunrs <- diff.xts(HOLN.SW$HOLN.SW.Close)[2:length(HOLN.SW$HOLN.SW.Close)]/HOLN.SW$HOLN.SW.Close[1:length(HOLN.SW$HOLN.SW.Close)-1]
colnames(Holcim_retunrs) <- "HOLN"
Logitech_returns <- diff.xts(LOGN.SW$LOGN.SW.Close)[2:length(LOGN.SW$LOGN.SW.Close)]/LOGN.SW$LOGN.SW.Close[1:length(LOGN.SW$LOGN.SW.Close)-1]
colnames(Logitech_returns) <- "LOGN"
LonzaGroup_returns <- diff.xts(LONN.SW$LONN.SW.Close)[2:length(LONN.SW$LONN.SW.Close)]/LONN.SW$LONN.SW.Close[1:length(LONN.SW$LONN.SW.Close)-1]
colnames(LonzaGroup_returns) <- "LONN"
Nestle_returns <- diff.xts(NESN.SW$NESN.SW.Close)[2:length(NESN.SW$NESN.SW.Close)]/NESN.SW$NESN.SW.Close[1:length(NESN.SW$NESN.SW.Close)-1]
colnames(Nestle_returns) <- "NESN"
Novartis_returns <- diff.xts(NOVN.SW$NOVN.SW.Close)[2:length(NOVN.SW$NOVN.SW.Close)]/NOVN.SW$NOVN.SW.Close[1:length(NOVN.SW$NOVN.SW.Close)-1]
colnames(Novartis_returns) <- "NOVN"
PartnersGroup_returns <- diff.xts(PGHN.SW$PGHN.SW.Close)[2:length(PGHN.SW$PGHN.SW.Close)]/PGHN.SW$PGHN.SW.Close[1:length(PGHN.SW$PGHN.SW.Close)-1]
colnames(PartnersGroup_returns) <- "PGHN"
Richemont_returns <- diff.xts(CFR.SW$CFR.SW.Close)[2:length(CFR.SW$CFR.SW.Close)]/CFR.SW$CFR.SW.Close[1:length(CFR.SW$CFR.SW.Close)-1]
colnames(Richemont_returns) <- "CFR"
Roche_returns <- diff.xts(ROG.SW$ROG.SW.Close)[2:length(ROG.SW$ROG.SW.Close)]/ROG.SW$ROG.SW.Close[1:length(ROG.SW$ROG.SW.Close)-1]
colnames(Roche_returns) <- "ROG"
Sika_returns <- diff.xts(SIKA.SW$SIKA.SW.Close)[2:length(SIKA.SW$SIKA.SW.Close)]/SIKA.SW$SIKA.SW.Close[1:length(SIKA.SW$SIKA.SW.Close)-1]
colnames(Sika_returns) <- "SIKA"
Sonova_returns <- diff.xts(SOON.SW$SOON.SW.Close)[2:length((SOON.SW$SOON.SW.Close))]/SOON.SW$SOON.SW.Close[1:length(SOON.SW$SOON.SW.Close)-1]
colnames(Sonova_returns) <- "SOON"
SwissLife_returns <- diff.xts(SLHN.SW$SLHN.SW.Close)[2:length(SLHN.SW$SLHN.SW.Close)]/SLHN.SW$SLHN.SW.Close[1:length(SLHN.SW$SLHN.SW.Close)-1]
colnames(SwissLife_returns) <- "SLHN"
SwissRe_returns <- diff.xts(SREN.SW$SREN.SW.Close)[2:length(SREN.SW$SREN.SW.Close)]/SREN.SW$SREN.SW.Close[1:length(SREN.SW$SREN.SW.Close)-1]
colnames(SwissRe_returns) <- "SREN"
Swisscom_returns <- diff.xts(SCMN.SW$SCMN.SW.Close)[2:length(SCMN.SW$SCMN.SW.Close)]/SCMN.SW$SCMN.SW.Close[1:length(SCMN.SW$SCMN.SW.Close)-1]
colnames(Swisscom_returns) <- "SCMN"
UBS_returns <- diff.xts(UBSG.SW$UBSG.SW.Close)[2:length(UBSG.SW$UBSG.SW.Close)]/UBSG.SW$UBSG.SW.Close[1:length(UBSG.SW$UBSG.SW.Close)-1]
colnames(UBS_returns) <- "UBSG"
Zurich_returns <- diff.xts(ZURN.SW$ZURN.SW.Close)[2:length(ZURN.SW$ZURN.SW.Close)]/ZURN.SW$ZURN.SW.Close[1:length(ZURN.SW$ZURN.SW.Close)-1]
colnames(Zurich_returns) <- "ZURN"

############################### Overview of the data ###########################
library(ggplot2)
library(ggfortify)
library(plotly)

ABB_Open_plot <- autoplot(ABBN.SW$ABBN.SW.Open, colour = "navy") +
  theme(
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle(paste("Abb", "Open price", sep = " ")) +
  ylab("Open Price (in US dollars)") +
  xlab("Time")
ggplotly(ABB_Open_plot)

ABB_Close_plot <- autoplot(ts(ABBN.SW$ABBN.SW.Close, start = c(2007, 1, 3), end = c(2023, 5, 8), frequency = 256), colour = "navy")  +
  theme(
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("ABB Close Price") +
  ylab("Close Price (in US dollars)") +
  xlab("Time")
ggplotly(ABB_Close_plot)

ABB_daily_returns <- autoplot(ABB_returns, colour = "navy")  +
  geom_abline(slope = 0, intercept = mean(ABB_returns), col = "yellow") +
  theme(
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("ABB Daily Returns") +
  ylab("Returns (in US dollars)") +
  xlab("Time")
ggplotly(ABB_daily_returns)


############################# Normality Test ###################################
ABB_mean = mean(ABB_returns)
ABB_sigma = sd(ABB_returns)
normal_mean_sigma = rnorm(length(ABB_returns[,1]), mean = 0, sd = 1)

qqplot(y = ABB_returns, x = normal_mean_sigma)
qqline(y= ABB_returns, col = "red")
title("QQPlot ABB returns vs potential normal distribution")
df <- as.data.frame(ABB_returns)

print(shapiro.test(df$ABB))
# definitely not normal, it has to be considered later on in the Var

########################### Correlation plot ###################################
library(GGally)

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

# correlation_plot <- ggpairs(data = SMI_stocks, columns = 1:19) +
#   theme(
#     legend.title = element_text(size = 10),
#     plot.title = element_text(color = "black", size = 20)
#   ) + 
#   ggtitle("Pearson Correlation Plot")
# correlation_plot


library(PerformanceAnalytics)
# 
# chart.Correlation(SMI_stocks, histogram=TRUE, pch=19)

###################### equal weighted portfolio ################################
###################### first case with 5 stocks ################################

weights_5_stocks = ((1/5)*c(1,1,1,1,1))

# we select the last five stocks, for not particular reason
stocks_returns <- xts()
stocks_returns <- merge.xts(LonzaGroup_returns, 
                            Sonova_returns,
                            Richemont_returns,
                            Sika_returns,
                            Nestle_returns)

mean_vector <- c()

for (i in 1:length(stocks_returns[1,])){
  mean_vector[i] <- mean(stocks_returns[, i])
}


expected_return_5_stock <- 0 

for (i in 1:length(stocks_returns[1,])){
  expected_return_5_stock <- expected_return_5_stock + weights_5_stocks[i] * mean_vector[i]
}
expected_return_5_stock
expected_return_5_stock_annual <- ((1 + expected_return_5_stock)**252) - 1
expected_return_5_stock_annual
mean_vector

var_5_stoks <- 0
# calculation of the standard deviation of this portfolio
for (i in 1:length(stocks_returns[1,])){
  for (j in 1:length(stocks_returns[1,])){
    
    if (names(stocks_returns)[i] == names(stocks_returns)[j]){
      var_5_stoks <- var_5_stoks + (weights_5_stocks[i]**2) * var(stocks_returns[ , i])
      # print(var_5_stoks)
    }
    else{
      var_5_stoks <- var_5_stoks + (weights_5_stocks[i] * weights_5_stocks[j]) * var(x = stocks_returns[ , i], y = stocks_returns[ , j])
      # print(var_5_stoks)
    }
    
  }
}

sd_5_stocks <- as.numeric(sqrt(var_5_stoks))
sd_5_stocks

sharpe_ratio_5_stocks_ew <- as.numeric((expected_return_5_stock/(sd_5_stocks)) * sqrt(252))
sharpe_ratio_5_stocks_ew

ew_portfolio <- data.frame(expectedReturn = expected_return_5_stock,
                           portfolioRisk = sd_5_stocks)
ew_portfolio
# risk_aversion_coefficient <- 2

############ Optimisation of a portfolio, general case #########################

stocks_returns <- merge.xts(LonzaGroup_returns, 
                            Sonova_returns,
                            Richemont_returns,
                            Sika_returns,
                            Nestle_returns)
stocks_returns

mean_vector <- c()

for (i in 1:length(stocks_returns[1,])){
  mean_vector[i] <- mean(stocks_returns[, i])
}

portfolio_numbers <- 10000

# weight_matrix <- matrix(nrow = portfolio_numbers, ncol = length(stocks_returns[1,]))
weight_matrix <- data.frame(matrix(nrow = 0, ncol = length(stocks_returns[1,])))

# vector("numeric", length = portfolio_numbers) 
portfolio_returns <- c()
portfolio_risk <- c()
portfolio_sharpe <- c()

for (i in 1:portfolio_numbers){
  
  temp_weight <- runif(length(stocks_returns[1,]), min = 0, max = 1)
  norm_temp_weight <- temp_weight/sum(temp_weight)

  weight_matrix[i, ] <- norm_temp_weight[1:length(norm_temp_weight)]
  
  port_daily_returns <- sum(temp_weight*mean_vector)
  port_return_value <- (((1 + port_daily_returns)**252) - 1)
  portfolio_returns[i] <- port_daily_returns
 
  port_risk_value <- 0
  stocks_returns_temp <- as.data.frame(stocks_returns)
  # calculation of the standard deviation of this portfolio
  for (j in 1:length(stocks_returns_temp)){
    for (k in 1:length(stocks_returns_temp)){
      if (names(stocks_returns_temp)[j] == names(stocks_returns_temp)[k]){
        port_risk_value <- port_risk_value + (norm_temp_weight[j]**2) * var(stocks_returns_temp[ , j])
      }
      else{
        port_risk_value <- port_risk_value + (norm_temp_weight[j] * norm_temp_weight[k]) * var(x = stocks_returns_temp[ , j], y = stocks_returns_temp[ , k])
      }
    }
  }
  
  portfolio_risk[i] <- sqrt(port_risk_value)
  
  port_sharpe_ratio <- (port_daily_returns/(sqrt(port_risk_value)))
  portfolio_sharpe[i] <- port_sharpe_ratio
}


library(tibble)
library(timetk)
total_portfolio <- tibble(Returns = portfolio_returns,
                          Risk = portfolio_risk,
                          Sharpe = portfolio_sharpe)

weight_matrix <- tk_tbl(weight_matrix)
colnames(weight_matrix) <- colnames(stocks_returns)

total_portfolio <- tk_tbl(cbind(weight_matrix, total_portfolio))

head(total_portfolio)

minimum_variance_portfolio <- total_portfolio[which.min(total_portfolio$Risk), ]
minimum_variance_portfolio
min_risk_portfolio <- data.frame(expectedReturn = as.numeric(minimum_variance_portfolio[1,6]),
                                   portfolioRisk = as.numeric(minimum_variance_portfolio[1,7]))
min_risk_portfolio
maximum_sharpe_ratio_portfolio <- total_portfolio[which.max(total_portfolio$Sharpe),]
maximum_sharpe_ratio_portfolio
max_sharpe_portfolio <- data.frame(expectedReturn = as.numeric(maximum_sharpe_ratio_portfolio[1,6]),
                           portfolioRisk = as.numeric(maximum_sharpe_ratio_portfolio[1,7]))
max_sharpe_portfolio

# efficient frontier:
p <- ggplot(data = total_portfolio,
            aes(x = Risk, y = Returns, color = Sharpe)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Risk",
       y = "Returns",
       title = "Portfolio Frontier") +
   geom_point(aes(x= Risk, y = Returns), data = minimum_variance_portfolio, color = "green") +
  geom_point(aes(x = Risk, y = Returns), data = maximum_sharpe_ratio_portfolio, color = "red")

ggplotly(p)

################# Normal Markovitz frontier calculation ########################

library(quadprogXT)

stocks_returns <- merge.xts(LonzaGroup_returns, 
                            Sonova_returns,
                            Richemont_returns,
                            Sika_returns,
                            Nestle_returns)
stocks_returns

mean_vector <- c()

for (i in 1:length(stocks_returns[1,])){
  mean_vector[i] <- mean(stocks_returns[, i])
}

number_assets <- length(stocks_returns[1,])

covariance_matrix <- (cov(stocks_returns))
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
  print(m$value)
  portfolio_std[i] <- sqrt(m$value)
  portfolio_wgt[i, ] <- t(m$solution)
  
}

########## Plotting all together ###############################################


plot(sqrt(diag(covariance_matrix)), mean_vector,
     xlim=c(0.5*min(portfolio_std),1.5*max(portfolio_std)), 
     ylim=c(0.5*min(mean_vector),2.5*max(mean_vector)),
     col = "yellow", lwd = 3,
     xlab = "Risk",
     ylab = "Daily return")
title("Portfolio Frontier")
text(sqrt(diag(covariance_matrix)), mean_vector,
     labels=colnames(stocks_returns), cex= 0.7)


# efficient frontier
lines(portfolio_std,portfolio_ret,col = "navy")
points(y = ew_portfolio$expectedReturn[1], x = ew_portfolio$portfolioRisk[1], col = "green", lwd = 3)
text(y = ew_portfolio$expectedReturn[1], x = ew_portfolio$portfolioRisk[1], labels="EWP", cex = 0.7)
points(y = min_risk_portfolio$expectedReturn[1], x = min_risk_portfolio$portfolioRisk[1], col = "orange", lwd = 3)
text(y = min_risk_portfolio$expectedReturn[1], x = min_risk_portfolio$portfolioRisk[1], labels="MinRisk", cex = 0.7)
points(y = max_sharpe_portfolio$expectedReturn[1], x = max_sharpe_portfolio$portfolioRisk[1], col = "red", lwd = 3)
text(y = max_sharpe_portfolio$expectedReturn[1], x = max_sharpe_portfolio$portfolioRisk[1], labels="MaxSR", cex = 0.7)


########## Michaud Resampled portfolio #########################################

library(MASS)

stocks_returns <- merge.xts(LonzaGroup_returns, 
                            Sonova_returns,
                            Richemont_returns,
                            Sika_returns,
                            Nestle_returns)
stocks_returns

mean_vector <- c()

for (i in 1:length(stocks_returns[1,])){
  mean_vector[i] <- mean(stocks_returns[, i])
}

number_assets <- length(stocks_returns[1,])

covariance_matrix <- (cov(stocks_returns))
number_of_points <- 300

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

######## plotting michaud portfolio ############################################

plot(sqrt(diag(covariance_matrix)), mean_vector,
     xlim=c(0.5*min(portfolio_std),1.5*max(portfolio_std)), 
     ylim=c(0.5*min(mean_vector),2.5*max(mean_vector)),
     col = "yellow", lwd = 3,
     xlab = "Risk",
     ylab = "Daily return")
title("Resampled Portfolio Frontier")
text(sqrt(diag(covariance_matrix)), mean_vector,
     labels=colnames(stocks_returns), cex= 0.7)


# efficient frontier
lines(portfolio_std,portfolio_ret,col = "navy", lwd = 3)
lines(portfolio_std_re,portfolio_ret_re,col = "cyan", lwd = 3)
points(y = ew_portfolio$expectedReturn[1], x = ew_portfolio$portfolioRisk[1], col = "green", lwd = 3)
text(y = ew_portfolio$expectedReturn[1], x = ew_portfolio$portfolioRisk[1], labels="EWP", cex = 0.7)
points(y = min_risk_portfolio$expectedReturn[1], x = min_risk_portfolio$portfolioRisk[1], col = "orange", lwd = 3)
text(y = min_risk_portfolio$expectedReturn[1], x = min_risk_portfolio$portfolioRisk[1], labels="MinRisk", cex = 0.7)
points(y = max_sharpe_portfolio$expectedReturn[1], x = max_sharpe_portfolio$portfolioRisk[1], col = "red", lwd = 3)
text(y = max_sharpe_portfolio$expectedReturn[1], x = max_sharpe_portfolio$portfolioRisk[1], labels="MaxSR", cex = 0.7)

