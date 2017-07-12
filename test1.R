library(data.table)
library(scales)
library(ggplot2)
library(reshape)

link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/fin_data.csv"
dt <- data.table(read.csv(link))
dt[, date := as.Date(date)]
#my data:
my_data <- melt(excess.data)
names(my_data) <- c("dates", "types", "returns")
my_data[, "year"] <- as.numeric(substr(my_data$dates[1:2760],1,4))
#my_data$year <- as.Date(my_data$year)
# create indexed values
dt[, idx_price := price/price[1], by = ticker]
# my data : I do not apply this on my data

# plot the indexed values
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Price Developments") +
  xlab("Date") + ylab("Pricen(Indexed 2000 = 1)") +
  scale_color_discrete(name = "Company")
#my data
ggplot(my_data, aes(x = year, y = returns, color = types)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Excess Developments") +
  xlab("year") + ylab("Excess returns") +
  scale_color_discrete(name = "BE/ME - Size type")

#### ############
# calculate the arithmetic returns
dt[, ret := price / shift(price, 1) - 1, by = ticker]
#my data
my_data[ , returns := excess / shift(excess, 1) -1, by = types]

# summary table
# take only non-na values
tab <- dt[!is.na(ret), .(ticker, ret)]

# calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
tab <- tab[, .(er = round(mean(ret), 4), sd = round(sd(ret), 4)), by = "ticker"]
               
           
ggplot(tab, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, 0.03)) +
  scale_x_continuous(label = percent, limits = c(0, 0.1))


## ##################
# load the data
link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv"
df <- data.table(read.csv(link))

# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)

# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)

# III) covariance
cov_xy <- cov(df$x, df$y)

# create 1000 portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)

# create a data.table that contains the weights for the two assets
two_assets <- data.table(wx = x_weights,
                         wy = 1 - x_weights)

# calculate the expected returns and standard deviations for the 1000 possible portfolios
two_assets[, ':=' (er_p = wx * er_x + wy * er_y,
                   sd_p = sqrt(wx^2 * sd_x^2 +
                                 wy^2 * sd_y^2 +
                                 2 * wx * (1 - wx) * cov_xy))]

######
# lastly plot the values
ggplot() +
  geom_point(data = two_assets, aes(x = sd_p, y = er_p, color = wx)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y), mean = c(er_x, er_y)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Possible Portfolios with Two Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(two_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(two_assets$sd_p) * 1.2)) +
  scale_color_continuous(name = expression(omega[x])