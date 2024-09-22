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
source("C:/Users/pbarr/Documents/ENSAE/3A_MIE/S1/MacroECNM_ML/Workspace/functions/functions_diff_indices.R")
source("C:/Users/pbarr/Documents/ENSAE/3A_MIE/S1/MacroECNM_ML/Workspace/functions/functions_3.R")
source("C:/Users/pbarr/Documents/ENSAE/3A_MIE/S1/MacroECNM_ML/Workspace/functions/transform_data.R")
source("C:/Users/pbarr/Documents/ENSAE/3A_MIE/S1/MacroECNM_ML/Project/functions_project.R")

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
jj <- 2

# Maximum number of factors to be estimated; if set to 99, the number of
# factors selected is forced to equal 8
kmax <- 8

# =========================================================================
# PART 1: LOAD AND LABEL DATA

# Variable names
series <- colnames(df_raw[,2:length(df_raw)])

# Transformation numbers (we do not need it because data are already transformed)
#tcode <- data[1,]

#  We get rid of the dates column
df <- df_raw[1:nrow(df_raw),2:length(df_raw)]

# Month/year of the final observation
final_date <- tail(df_raw$DATE, 1)
dates <- df_raw$DATE

# T = number of months in the sample
TT <- length(dates)

# =========================================================================
# PART 2: PROCESS DATA

class(df) <- c("data.frame", "fredmd")


#################################3

#select data
nn = 1 #corresponds to the index number of our target: CSUSHPISA in the following dataframe


S_t = df[,c('CSUSHPISA', 'CURRCIR', 'PCE', 'TTLHHM156N', 'DPSACBW027SBOG', 'GDPr',
            'REAINTRATREARAT10Y', 'PSAVERT', 'MICH', 'CAPUTLG3311A2S', 'INDPRO',
            'IPB52300S', 'IPCONGD', 'IPDCONGD', 'IPG211S', 'IPG311A2S', 'IPG321S',
            'BOXRSA', 'CEXRSA', 'CHXRSA', 'DNXRSA', 'LXXRSA', 'MIXRSA', 'MNXRSA',
            'NYXRSA', 'PHXRSA', 'POXRSA', 'SDXRSA', 'SFXRSA', 'SPCS10RSA', 'TPXRSA',
            'WDXRSA', 'FLTOTALSL', 'NONREVSL', 'REVOLSL', 'TOTALSL',
            'COREFLEXCPIM159SFRBATL', 'CORESTICKM157SFRBATL',
            'CORESTICKM158SFRBATL', 'CORESTICKM159SFRBATL', 'CORESTICKM679SFRBATL',
            'CPIEALL', 'CPIEHOUSE', 'CWSR0000SA0', 'FLEXCPIM679SFRBATL',
            'IA001176M', 'IA001260M', 'MEDCPIM094SFRBCLE', 'MEDCPIM157SFRBCLE',
            'MEDCPIM158SFRBCLE', 'MEDCPIM159SFRBCLE', 'PCEPI', 'PCEPILFE',
            'PCETRIM12M159SFRBDAL', 'PCETRIM1M158SFRBDAL', 'PCETRIM6M680SFRBDAL',
            'STICKCPIM157SFRBATL', 'STICKCPIM159SFRBATL',
            'STICKCPIXSHLTRM159SFRBATL', 'TRMMEANCPIM158SFRBCLE', 'MSACSR',
            'BUSLOANS', 'CONSUMER', 'DPSACBM027SBOG', 'LOANINV', 'LOANS', 'REALLN',
            'TLAACBM027SBOG', 'USGSEC', 'CIVPART', 'LNS11300036', 'LNS11300060',
            'LNS11324230', 'M2REAL', 'M2SL', 'RMFSL', 'STDSL', 'LNS14000001',
            'LNS14000002', 'LNS14000024', 'LNS14000031', 'LNS14024887')]

X_t = df[,c('CSUSHPISA',
            "CPIAUCSL",
            "DSPIC96",
            "SPREAD",
            "MORTGAGE30US",
            "UNRATE")]



# We choose the horizon levels we want to forecast
HH = c(3,6)
###################################
### Forecasting

start_date = "2018-09-01"; #start date of out-of-sample


# =========================================================================
start_sample <- which(dates == start_date)

# Number of time points to use in the estimation of the parameter: Rolling scheme
wind_size = start_sample

j0 <- start_sample - wind_size + 1


# Prepare empty matrices that contain the results
true <- matrix(NA, nrow = TT - tail(HH, 1) - start_sample +1, ncol = length(HH))
PC <- matrix(NA, nrow = TT - tail(HH, 1) - start_sample +1, ncol = length(HH)*3)
NB_factors <- matrix(NA, nrow = TT - tail(HH, 1) - start_sample +1, ncol = length(HH))



for (j in start_sample:(TT - tail(HH, 1) )) {
  # Remark that TT - tail(HH, 1) is '2023-09-01'. This is the last period of the out-of-sample
  # for each j we forecast the target at date j+h
  
  ## Displays the dates at the beginning of each month
  cat('--------------\n')
  cat('now running\n')
  cat(paste(dates[j], collapse = ' '), '\n')
  
  
  ## Define the beginning of the estimation sample
  j0 <- j - wind_size + 1  # Starting period for the in-sample
  
  # We remove outliers from all variables
  
  S_temp <- remove_outliers(S_t[j0:j, ])
  X_1 <- remove_outliers(X_t[j0:j, ])
  
  # We remove outliers from Exogenous variables X_t and replace missing values by unconditionnal mean of non-missing values
  # Number of observations per series in x_new (i.e. number of rows)
  T <- nrow(X_1)
  
  
  # Fill in missing values for each series with the unconditional mean of that series.
  # Demean and standardize the updated dataset. Estimate factors using the demeaned and standardized dataset,
  # and use these factors to predict the original dataset.
  
  # Get unconditional mean of the non-missing values of each series
  mut <- matrix(rep(colMeans(X_1, na.rm = TRUE), T), nrow = nrow(X_1), ncol = ncol(X_1), byrow = TRUE)
  
  # Replace missing values with unconditional mean
  X_2 <- X_1
  X_2[is.na(X_2)] <- mut[is.na(X_2)] # we replace the NA values in the vector x2 with the corresponding non-NA values from the vector mut.
  
  # We check whether there are entire columns of zeros
  Index_zeros <- which(colSums(X_2==0) == nrow(X_2))
  if ((length(Index_zeros))==0){
    X_2_new <- X_2
  } else {
    X_2_new <- subset(X_2,select = -Index_zeros)
  }
  
  
  # We demean and standardize data
  X_3 <- transform_data(X_2_new, DEMEAN)
  X_temp <- X_3$x22
  
  i = 0 # We use this as an index for each forecast horizon
    
  for (h in HH) {  # Loop across the number of steps ahead
      
    i = i+1
      
    
    ## We keep the true value to be predicted
    true[j - wind_size + 1, i] <- S_t[ j+h, nn] 
    
    ## We compute the factors
    result_factors <- factors_em_2(S_temp[,-nn], kmax, jj, DEMEAN)
    
    # Regressors
    A <- as.matrix(cbind(X_temp, result_factors$Fhat)) #model with factors, exogenous variables and actual value of CSHUSHPISA
    B <- as.matrix(cbind(X_temp[,1], result_factors$Fhat)) #model with factors and actual value of CSUSHPISA
    C <- as.matrix(result_factors$Fhat) #model with only the factors as regressors
    
    #we keep a record of how many factors are used for each forecast date of the oos
    NB_factors[j - wind_size + 1, i]<- ncol(result_factors$Fhat)
    
    #We standardize the dependent variable to be predicted
    Y <- S_temp[,nn]
    my = mean(Y)
    sy = sd(Y)
    Y_std = (Y-my)/sy
    
    # Compute the forecasts by OLS for each model
    
    A_trimmed <- A[1:(nrow(A)-h), ]
    gamma_A <- solve(t(A_trimmed) %*% A_trimmed) %*% t(A_trimmed) %*% Y_std[(h+1):length(Y_std)]
    pred_A <- tail(A, 1) %*% gamma_A
    
    B_trimmed <- B[1:(nrow(B)-h), ]
    gamma_B <- solve(t(B_trimmed) %*% B_trimmed) %*% t(B_trimmed) %*% Y_std[(h+1):length(Y_std)]
    pred_B <- tail(B, 1) %*% gamma_B
    
    C_trimmed <- C[1:(nrow(C)-h), ]
    gamma_C <- solve(t(C_trimmed) %*% C_trimmed) %*% t(C_trimmed) %*% Y_std[(h+1):length(Y_std)]
    pred_C <- tail(C, 1) %*% gamma_C
    
    PC[j - wind_size + 1, i] <- (pred_A*sy + my)
    PC[j - wind_size + 1, i + 2] <- (pred_B*sy + my)
    PC[j - wind_size + 1, i + 4] <- (pred_C*sy + my)
      
  }
}



## We compute the RMSE and the R^2 for all  models at all horizons
# horizon 3 months
true_NA_3 <- na.omit(true[,1])
PC_NA_3_A <- na.omit(PC[,1])
PC_NA_3_B <- na.omit(PC[,3])
PC_NA_3_C <- na.omit(PC[,5])

#horizon 6 months
true_NA_6 <- na.omit(true[,2])
PC_NA_6_A <- na.omit(PC[,2])
PC_NA_6_B <- na.omit(PC[,4])
PC_NA_6_C <- na.omit(PC[,6])

RMSE_PC_3_A <- sqrt(mean((true_NA_3 - PC_NA_3_A)^2))
RMSE_PC_3_B <- sqrt(mean((true_NA_3 - PC_NA_3_B)^2))
RMSE_PC_3_C <- sqrt(mean((true_NA_3 - PC_NA_3_C)^2))
R_2_3_A <- calculate_adjusted_r_squared(true_NA_3, PC_NA_3_A, 14)
R_2_3_B <- calculate_adjusted_r_squared(true_NA_3, PC_NA_3_B, 9)
R_2_3_C <- calculate_adjusted_r_squared(true_NA_3, PC_NA_3_C, 8)


RMSE_PC_6_A <- sqrt(mean((true_NA_6 - PC_NA_6_A)^2))
RMSE_PC_6_B <- sqrt(mean((true_NA_6 - PC_NA_6_B)^2))
RMSE_PC_6_C <- sqrt(mean((true_NA_6 - PC_NA_6_C)^2))
R_2_6_A <- calculate_adjusted_r_squared(true_NA_6, PC_NA_6_A, 14)
R_2_6_B <- calculate_adjusted_r_squared(true_NA_6, PC_NA_6_B, 9)
R_2_6_C <- calculate_adjusted_r_squared(true_NA_6, PC_NA_6_C, 8)

#plot results dependding on the forecast horizon

h = 6

dates_OOS <- as.Date(dates[(start_sample + h):(length(dates)-tail(HH, 1) +h)])

plot(dates_OOS, PC_NA_6_A, type = 'l', col = "red")
lines(dates_OOS, true_NA_6, col="blue",lty=2)
legend("bottomleft", legend = c("forecast", "target"), 
       col = c("red", "blue"), lty = c(1, 2),  # 'n' means no box around the legend
       title = "Forecast of HP 6 months ahead")

####NEXT STEPS

# 1. COMPUTE THE IN-SAMPLE RESULTS (basically train the model on the whole in-sample period and multiply parameters by time series)
# 2. LOOK AT THE WEIGHT OF FACTORS IN THE FINAL REGRESSION
# 3. COMPARE WITH A SIMPLE LINEAR REGRESSION WITHOUT FACTORS

########### 1. COMPUTE IN-SAMPLE

S_temp2 <- remove_outliers(S_t[1:(start_sample-1+h), ])  # The available data at each time point of the evaluation exercise
X_1 <- remove_outliers(X_t[1:start_sample-1, ])
# We remove outliers from exogenous variables and also replace missing values by unconditionnal mean of non-missing values
# Number of observations per series in x_new (i.e. number of rows)
T <- nrow(X_1)


# Fill in missing values for each series with the unconditional mean of that series.
# Demean and standardize the updated dataset. Estimate factors using the demeaned and standardized dataset,
# and use these factors to predict the original dataset.

# Get unconditional mean of the non-missing values of each series
mut <- matrix(rep(colMeans(X_1, na.rm = TRUE), T), nrow = nrow(X_1), ncol = ncol(X_1), byrow = TRUE)

# Replace missing values with unconditional mean
x2 <- X_1
x2[is.na(x2)] <- mut[is.na(x2)]         # we replace the NA values in the vector x2 with the corresponding non-NA
# values from the vector mut.
# Check whether there are entire columns of zeros
Index_zeros <- which(colSums(x2==0) == nrow(x2))
if ((length(Index_zeros))==0){
  x2_new <- x2
  #x1_new <- x1
} else {
  x2_new <- subset(x2,select = -Index_zeros)
  #x1_new <- subset(x1,select = - Index_zeros)
  #x_new <- subset(x_new,select = -Index_zeros)
}
# Demean and standardize data
X_3 <- transform_data(x2_new, DEMEAN)
X_temp2 <- X_3$x22

#Estimate factor on whole training period
S_pred2 <- S_temp2[1:(start_sample-1), -1]
result_factors2 <- factors_em_2(S_pred2, kmax, jj, DEMEAN)

# Regressors
Z <- as.matrix(cbind(X_temp2, result_factors2$Fhat))

# Compute the dependent variable to be predicted
# Apply moving average filter: Y = (y_{+1}+...+y_{+h})/h
#Y <- stats::filter(S_temp[,nn], filter = rep(1/h, h), sides = 1)
Y <- S_temp2[,nn]
my = mean(Y)
sy = sd(Y)
Y_std = (Y-my)/sy
# Compute the forecasts
Z_trimmed <- Z[1:(nrow(Z)), ]
gamma <- solve(t(Z_trimmed) %*% Z_trimmed) %*% t(Z_trimmed) %*% Y_std[(h+1):length(Y_std)]
pred_train <- (Z_trimmed %*% gamma)*sy + my

dates_INS = as.Date(dates[(1+h):(start_sample-1+h)])
plot(dates_INS,pred_train, type = 'l', col = "black")
lines(dates_INS, S_t[(1+h):(start_sample-1+h),1], col="blue",lty=2)
