# Script for Empirical Apprlication Section, Scaillet, Topaloglou, Testing for SDE
# The time span is 1963-2001
install.packages("reshape")
install.packages("psych", dependencies = T)
install.packages("data.table")
install.packages("e1071", dependencies = FALSE)
install.packages("xtable", dependencies = FALSE)
install.packages("fPortfolio", dependencies = FALSE)
install.packages("testthat")
install.packages("stringr")
install.packages("ggplot2")
library(stringr)
library(ggplot2)
library(scales)
library(ggthemes)
library(PerformanceAnalytics)
library(RColorBrewer)
library(zoo)
library(psych)
library(data.table)
library(e1071)
library(xtable)
library(testthat)
library(stringr)
library(ggplot2)

# The URL for the data
url.name <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
file.name <- "6_Portfolios_2x3_CSV.zip"
full.url <- paste(url.name, file.name, sep="/")

# change the ending date of the analysis by changing these variables. Though, source file could be updated annually


end.year <- 2001
end.month <- 10
window.width <- 28*12     # 28 Years in this case

# Download and Unzip 

temp.file <- tempfile()
download.file(full.url, temp.file)
file.list <- unzip(temp.file, list=TRUE)


## Parse the data
benchmark.data <-read.csv(unzip(temp.file, files=as.character(file.list[1,1])), skip=19, header=TRUE, stringsAsFactors=FALSE)
names(benchmark.data)[[1]] <- "Dates"
treasury.data <- read.csv("C:/Users/Danny/Dropbox/phd/draft_1/1-month-treasury-bill.csv",header=TRUE, stringsAsFactors = FALSE)
#/home/danai for my minux-mint
treasury.data[1] <- NULL 
    # Construct the market portfolio dataframe
market.data <- read.csv("C:/Users/Danny/Dropbox/phd/draft_1/Market_excess.csv", header= TRUE, stringsAsFactors = FALSE)
market.data[1] <- NULL

# Remove all  data below the end date
start.year <- as.numeric(substr(benchmark.data$Dates[[445]],1,4))
start.month <- as.numeric(substr(benchmark.data$Dates[[445]],5,6))
# First delete the observations prior to our wanted date of July 1963 e.g. row 446
benchmark.data <- benchmark.data[-c(1:444), ]
# Keep the first 460 obs ( from July 1963-October 2001 )
num.rows <- 12*(end.year-start.year)+(end.month-start.month)+1
benchmark.data <- head(benchmark.data,num.rows)

####################
# Format dataframe
####################

# Form the date vector as mm-yyyy / class of the vector is character
date_month <- as.character(substr(benchmark.data$Dates,5,6))
date_year <- as.character(substr(benchmark.data$Dates,1,4))
benchmark.data$Dates <- paste(date_year,"-",date_month)

# Rename of the headers
headers<- c("LoBEME&Small", "MeBEME&Small", "HiBEME&Small", "LoBEME&Large", "MeBEME&Large", "HiBEMELarge")
names(benchmark.data)[2:7] <- headers

# Construct the excess returns substracting the 1-month t bill from the returns of the 6 portfolios.
excess.data <- as.data.frame(matrix(NA, nrow = 460, ncol = 7))
for (i in 2:ncol(benchmark.data)) {
  excess.data[ ,i] <- as.numeric(unlist(benchmark.data[ ,i])) - as.numeric(unlist(treasury.data))
}

#Fill the first column with the dates and create headers
excess.data[ ,1] <- benchmark.data$Dates
headers_excess <- c("Dates", "Low BE/ME & Small Size", "Medium BE/ME & Small Size", "High BE/ME & Small Size", 
                    "Low BE/ME & Large Size", "Medium BE/ME & Large Size", "High BE/ME & Large Size")
names(excess.data)[1:7] <- headers_excess



# Transform the data so that the return cells are in numeric decimal format
for (i in 2:ncol(excess.data)) {
  excess.data[,i] <- as.numeric(str_trim(excess.data[,i]))
}

################################################
# Descriptive statistics of the data
################################################
#First we construct the data frame for the descriptive statistics table:

desc_stats <- as.data.frame(matrix(NA, nrow = 7, ncol = 6))
desc_headers <- c( "Mean", "Std Dev", "Skewness" , "Kurtosis" , "Minimum" , "Maximum")
names(desc_stats) <- desc_headers
desc_stats <- (setattr(desc_stats, "row.names", c("Market Portfolio", "Portfolio 1", "Portfolio 2", "Portfolio 3", "Portfolio 4", "Portfolio 5", "Portfolio 6")))

##Secondly we find each statistic (mean, st. dev. etc..)

desc_stats[2:7 ,1] <- sapply(excess.data[2:7], mean)
desc_stats[1,1] <- mean(as.numeric(unlist(market.data)))
desc_stats[2:7, 2] <- sapply(excess.data[2:7], sd)
desc_stats[1,2] <- sd(as.numeric(unlist(market.data)))
desc_stats[2:7, 3] <- sapply(excess.data[2:7], skewness)
desc_stats[1,3] <- skewness(as.numeric(unlist(market.data)))
desc_stats[2:7, 4] <- sapply(excess.data[2:7], kurtosis)
desc_stats[1,4] <- kurtosis(as.numeric(unlist(market.data)))
desc_stats[2:7, 5] <- sapply(excess.data[2:7], min)
desc_stats[1,5] <- min(as.numeric(unlist(market.data)))
desc_stats[2:7, 6] <- sapply(excess.data[2:7], max)
desc_stats[1,6] <- max(as.numeric(unlist(market.data)))



# Transform the data as percentage returns
#for (i in 2:ncol(benchmark.data)) {
 # benchmark.data[,i] <- benchmark.data[,i]/100
#}


# Create tex code for the latex table with the results:
desc_stats.tex <- xtable(desc_stats[1:7,])


#################################################################################################
#-----------------------------------MV analysis ------------------------------------------------
#------------------------------------------------------------------------------------------------


