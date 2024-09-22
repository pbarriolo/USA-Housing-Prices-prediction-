### Macro ML Project: Influence of Macroeconomic Indices on Real Estate Prices
## Approach via DFM

#install.packages("remotes")
#install.packages("stats")
#install.packages("readr")
#install.packages("pracma")
#remotes:: install_github("nk027/bvar")
#remotes:: install_github("cykbennie/fbi")

#libraries

library(BVAR)
library(fbi)

#functions locations
source("C:/Users/pbarr/Documents/ENSAE/3A_MIE/S1/MacroECNM_ML/Workspace/functions/remove_outliers.R")
source("C:/Users/pbarr/Documents/ENSAE/3A_MIE/S1/MacroECNM_ML/Workspace/functions/factors_em.R")
source("C:/Users/pbarr/Documents/ENSAE/3A_MIE/S1/MacroECNM_ML/Workspace/functions/functions_3.R")
source("C:/Users/pbarr/Documents/ENSAE/3A_MIE/S1/MacroECNM_ML/Workspace/functions/transform_data.R")

## Import Data
# data is already transformed so that it is stationary
df_raw_0 <- read.csv("C:/Users/pbarr/Documents/ENSAE/3A_MIE/S1/MacroECNM_ML/Project/fred_data.csv", header = TRUE, sep = ",")

df_raw <- read.csv("C:/Users/pbarr/Documents/ENSAE/3A_MIE/S1/MacroECNM_ML/Project/fred_large_data.csv", header = TRUE, sep = ",")
# Type of transformation performed on each series before factors are
# estimated
#   0 --> no transformation
#   1 --> demean only
#   2 --> demean and standardize
#   3 --> recursively demean and then standardize
DEMEAN <- 2

# Information criterion used to select the number of factors; for more details,
# see auxiliary function factors_em()
#   1 --> information criterion PC_p1
#   2 --> information criterion PC_p2
#   3 --> information criterion PC_p3
jj <- 3

# Maximum number of factors to be estimated; if set to 99, the number of
# factors selected is forced to equal 8
kmax <- 8

# =========================================================================
# PART 1: LOAD AND LABEL DATA

# Variable names
series <- colnames(df_raw[,2:length(df_raw)])

# Transformation numbers (we do not need it because data are already transformed)
#tcode <- data[1,]

# Raw data
rawdata <- df_raw[2:nrow(df_raw),2:length(df_raw)]

# Month/year of the final observation
final_date <- tail(df_raw$DATE, 1)
dates <- df_raw$DATE

# T = number of months in the sample
TT <- length(dates)

# =========================================================================
# PART 2: PROCESS DATA

# 1. Prepare Missing Data
class(rawdata) <- c("data.frame", "fredmd")
yt <- rawdata[,-1]
# 2. Reduce Sample  to usable dates: remove first two months because some
# series have been first differenced
start_date = '1995-01-01'
start_sample <- which(dates == start_date)
end_date = '2019-12-01'
end_sample <- which(dates == end_date)
result <- yt[start_sample:end_sample, ]
dates <- as.Date(dates[start_sample:end_sample])

# 3. Remove Outliers
#result <- remove_outliers(yt)


# =========================================================================
# PART 3: ESTIMATE FACTORS AND COMPUTE R-SQUARED

# 1. Estimate Factors using function "factors_em()"
#   ehat    = difference between data and values of data predicted by the factors
#   Fhat    = set of factors
#   lamhat  = factor loadings
#   ve2     = eigenvalues of X'X
#   x2      = data with missing values replaced from the EM algorithm (but untrasformed)
result_factors <- factors_em(result, kmax, jj, DEMEAN)

ehat <- result_factors$ehat
Fhat <- result_factors$Fhat
lamhat <- result_factors$lamhat
ve2 <- result_factors$ve2
x2 <- result_factors$x2

# 2. Compute R-Squared and marginal R-squared from estimated factors and factor loadings using function "mrsq()"
#   R2      = R-squared for each series for each factor
#   mR2     = marginal R-squared for each series for each factor
#   mR2_F   = marginal R-squared for each factor
#   R2_T    = total variation explained by all factors
#   t10_s   = top 10 series that load most heavily on each factor
#   t10_mR2 = marginal R-squared corresponding to top 10 series
#             that load most heavily on each factor
result_mrsq <- mrsq(Fhat, lamhat, ve2, series)
R2 <- result_mrsq$R2
mR2 <- result_mrsq$mR2
mR2_F <- result_mrsq$mR2_F
R2_T <- result_mrsq$R2_T
t10_s <- result_mrsq$t10_s
t10_mR2 <- result_mrsq$t10_mR2


# Create a bar plot

# Set the length of the vertical axis

custom_ylim <- c(0, 1)

barplot(R2[,2], names.arg = 1:length(R2[,2]), main = "Importance of Factors: R2", xlab = "Series",
        
        ylab = "R2", ylim = custom_ylim, col = "blue")



plot(dates, Fhat[,1], type = 'l', col = "black")
lines(dates, Fhat[,2], col = 'blue')
lines(dates, Fhat[,3], col = 'purple')
lines(dates, Fhat[,4], col = 'yellow')
lines(dates, Fhat[,5], col = 'red')
lines(dates, Fhat[,6], col = 'green')
lines(dates, Fhat[,7], col = 'orange')
lines(dates, Fhat[,8], col = 'brown')
#lines(dates, rawdata$CSUSHPISA[2:length(rawdata$CSUSHPISA)], col = 'black')





