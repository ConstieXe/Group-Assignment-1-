# Load necessary libraries
# Load necessary libraries
library(knitr)
library(readr)
library(glmnet)
library(caret)
library(tibble)
library(dplyr)

# Set working directory and read data
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
brazil_census <- read.csv2("Team_2_Brazil_data_census.csv", sep = ",", dec = ".", header = TRUE)[-1] #loads the data and removes the first column

# Create regions data frame
df_regions <- tibble(
  UF = c(12, 16, 13, 15, 11, 14, 17, 27, 29, 23, 21, 25, 22, 26, 24, 28, 53, 52, 51, 50, 32, 31, 33, 35, 41, 42, 43),
  state_letters = c("AC", "AP", "AM", "PA", "RO", "RR", "TO", "AL", "BA", "CE", "MA", "PB", "PI", "PE", "RN", "SE", "DF", "GO", "MT", "MS", "ES", "MG", "RJ", "SP", "PR", "SC", "RS"),
  state_names = c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins", "Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Piauí", "Pernambuco", "Rio Grande do Norte", "Sergipe", "Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul", "Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul"),
  region_letters = c(rep("NO", 7), rep("NE", 9), rep("CW", 4), rep("SE", 4), rep("SO", 3)),
  region_names = c(rep("North", 7), rep("Northeast", 9), rep("Central West", 4), rep("Southeast", 4), rep("South", 3)))

# Merge and convert to factors
brazil_census <- merge(brazil_census, df_regions, by = "UF") %>%
  mutate(across(c(state_letters, state_names, region_letters, region_names, UF, CODMUN6, NOMEMUN), as.factor),
         House_services = AGUA_ESGOTO + T_SLUZ)
brazil_census<- brazil_census[,-c(1,2,3,30,31,32)] #for the factors created we only consider the region names

#OLS regression
OLS_model <- lm(R1040 ~ . , data = brazil_census) #Removes the variables that are not necessary

alias(OLS_model) # we see that we need to remove HOMEMTOT, T_SLUZ and pesourb
OLS_model <- update(OLS_model, . ~ . - HOMEMTOT - T_SLUZ - pesourb)
print(vif(OLS_model)) #Check multicollinearity

OLS_model <- update(OLS_model, . ~ . - pesotot)
print(vif(OLS_model))
OLS_model <- update(OLS_model, . ~ . - region_names) #We remove these variables to address multicollinearity
print(vif(OLS_model))
OLS_model <- update(OLS_model, . ~ . - ESPVIDA)
print(vif(OLS_model))
OLS_model <- update(OLS_model, . ~ . - AGUA_ESGOTO)
print(vif(OLS_model))

OLS_model <- lm(R1040 ~ FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + T_MED18M 
                + PRENTRAB + RDPC + T_ATIV2529 + T_DES2529 + TRABSC + T_DENS + PAREDE + T_M10A14CF     #We end up with this model 
                + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + MULHERTOT + pesoRUR + House_services,
                data = brazil_census,
                singular.ok =  FALSE) 

summary(OLS_model)

x <- model.matrix(~ FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + 
                    T_MED18M + PRENTRAB + RDPC + T_ATIV2529 + T_DES2529 + 
                    TRABSC + T_DENS + PAREDE + T_M10A14CF + 
                    T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + MULHERTOT + 
                    pesoRUR + House_services, data = brazil_census)[,-1]  # Create matrix and remove intercept

y <- brazil_census$R1040

first_lasso_model <- glmnet(x, y, alpha = 1)  # Run Lasso regression 

coef(first_lasso_model)  # Lasso coefficients
print(data.frame(Variable = names(coef(OLS_model)), Coefficient = coef(OLS_model), row.names = NULL), row.names = FALSE) #OLS coefficients

#Dividing the data into train ad test
set.seed(123) #This ensures that the results are reproducible
train_indices <- sample(1:nrow(brazil_census), 3150)
train_data <- brazil_census[train_indices,] 
test_data <- brazil_census[-train_indices,] #state_names, region_letters
x_train <- model.matrix(~ . - 1, data = train_data[,-10]) #Remove dependent variable 
y_train <- train_data$R1040
x_test <- model.matrix(~ . - 1, data = test_data[,-10])
y_test <- test_data$R1040

# Lasso regression
set.seed(123) 
cvfit_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
lasso_model_min <- glmnet(x_train, y_train, alpha = 1, lambda = cvfit_lasso$lambda.min)

# Elastic Net
alpha_values <- seq(0, 1, by = 0.1)
best_alpha_net <- NA
lambdamin_net <- NA
best_mse_net <- Inf

for (alpha_val in alpha_values) {cvfit_net <- cv.glmnet(x = x_train, y = y_train, alpha = alpha_val)
if (min(cvfit_net$cvm) < best_mse_net) 
{best_mse_net <- min(cvfit_net$cvm)
best_alpha_net <- alpha_val
lambdamin_net <- cvfit_net$lambda.min}}

net_model_min <- glmnet(x = x_train, y = y_train, alpha = best_alpha_net, lambda = lambdamin_net)

# Compare coefficients
models_coef <- cbind(coef(lasso_model_min), coef(net_model_min))
colnames(models_coef) <- c("Lasso Coef", "Elastic Net Coef")
print(models_coef)

#Lasso Coef path
lasso_coef_path <- glmnet(x_train, y_train, alpha = 1, standardize = FALSE) #standardized = False so that coef go back to original scale
plot(lasso_coef_path, xvar = "lambda", label = TRUE)
#Elastic Net Coef path
elasticnet_coef_path <- glmnet(x_train, y_train, alpha = 0.7, standardize = FALSE)
plot(elasticnet_coef_path , xvar = "lambda", label = TRUE)

# Run models on the 1se lambda
lasso_model_1se <- glmnet(x_train, y_train, alpha = 1, lambda = cvfit_lasso$lambda.1se)
net_model_1se <- glmnet(x_train, y_train, alpha = best_alpha_net, lambda = cvfit_net$lambda.1se)

# Predictions
lasso_pred_min <- predict(lasso_model_min, s = cvfit_lasso$lambda.min, newx = x_test)
lasso_pred_1se <- predict(lasso_model_1se, s = cvfit_lasso$lambda.1se, newx = x_test)
net_pred_min <- predict(net_model_min, s = cvfit_net$lambda.min, newx = x_test)
net_pred_1se <- predict(net_model_1se, s = cvfit_net$lambda.1se, newx = x_test)

#RMSE 
rmse_lassomin <- sqrt(mean((y_test - lasso_pred_min)^2))
rmse_lasso1se <- sqrt(mean((y_test - lasso_pred_1se)^2))
rmse_netmin <- sqrt(mean((y_test - net_pred_min)^2))
rmse_net1se <- sqrt(mean((y_test - net_pred_1se)^2))

# Data frame for comparison
rmse_comparison_df <- data.frame(
  Model = c("Lasso Regression", "Elastic Net"),
  RMSE_Lambda_min = c(rmse_lassomin, rmse_netmin),
  RMSE_Lambda_1se = c(rmse_lasso1se, rmse_net1se))
print(rmse_comparison_df)

#Load data
brazil_data_prediction <- read.csv2("Brazil_census_data_prediction.csv", sep = ",", dec = ".", header = TRUE)[,-1] 

brazil_data_prediction <- merge(brazil_data_prediction, df_regions, by = "UF") %>%
  mutate(across(c(state_letters, state_names, region_letters, region_names, UF, CODMUN6, NOMEMUN), as.factor),
         House_services = AGUA_ESGOTO + T_SLUZ)
brazil_data_prediction <- brazil_data_prediction[,-c(1,2,3,30,31,32)] #for the factors created we only consider the region names

# Check for NAs and summary
sum(is.na(brazil_data_prediction))
summary(brazil_data_prediction)

x_pred <- model.matrix(~ . - 1, data = brazil_data_prediction[,-10]) #Remove dependent variable 

region_namesNorth <- matrix(0, nrow = nrow(x_pred), ncol = 1)            
region_namesSoutheast <- matrix(0, nrow = nrow(x_pred), ncol = 1)  
x_pred <- cbind(x_pred, region_namesNorth, region_namesSoutheast)
colnames(x_pred)[30:31] <- c("region_namesNorth", "region_namesSoutheast")

y_pred <- brazil_data_prediction$R1040

new_data_prediction <- predict(lasso_model_min, s = cvfit_lasso$lambda.min, newx = x_pred)
mse_pred <- mean((y_pred - new_data_prediction)^2)
rmse_pred <- sqrt(mse_pred)

# Logistic Regression
set.seed(123)
brazil_census <- brazil_census %>% mutate(Poor = as.integer(R1040 <= 9.5))
train_indices_logistic <- sample(1:nrow(brazil_census), 3150)
train_data_logistic <- brazil_census[train_indices_logistic,]#Remove unecessary columns
test_data_logistic <- brazil_census[-train_indices_logistic,]

x_train_logistic <- model.matrix(Poor ~ . - 1, data = train_data_logistic[,-10])#Remove R1040 variable
y_train_logistic <- train_data_logistic$Poor
x_test_logistic <- model.matrix(Poor ~ . - 1, data = test_data_logistic[,-10])
y_test_logistic <- test_data_logistic$Poor


cvfit_logistic <- cv.glmnet(x_train_logistic, y_train_logistic, alpha = 0, family = "binomial")
logistic_model_min <- glmnet(x_train_logistic, y_train_logistic, alpha = 0, lambda = cvfit_logistic$lambda.min, family = "binomial")
logistic_model_1se <- glmnet(x_train_logistic, y_train_logistic, alpha = 0, lambda = cvfit_logistic$lambda.1se, family = "binomial")

logistic_pred_min <- predict(logistic_model_min, newx = x_test_logistic)
logistic_pred_1se <- predict(logistic_model_1se, newx = x_test_logistic)

logistic_pred_min <- predict(logistic_model_min, s = cvfit_lasso$lambda.min, newx = x_test_logistic)#USE RESPONSE OR NOT#
logistic_pred_1se <- predict(logistic_model_1se, s = cvfit_lasso$lambda.1se, newx = x_test_logistic)
rmse_logistic_min <- sqrt(mean((y_test_logistic - logistic_pred_min)^2))
rmse_logistic_1se <- sqrt(mean((y_test_logistic - logistic_pred_1se)^2))

rmse_logistic <- data.frame(Model = "Logistic Regression", 
                            RMSE_lambda_min = rmse_logistic_min,
                            RMSE_lambda_1se = rmse_logistic_1se)
print(rmse_logistic)

# Step 1: Make predictions
logistic_pred_minlogloss <- predict(logistic_model_min, newx = x_test_logistic, type = "response")
logistic_pred_1selogloss <- predict(logistic_model_1se, newx = x_test_logistic, type = "response")

# Step 2: Define the log loss function
log_loss <- function(y_true, y_pred) 
{eps <- 1e-15
y_pred <- pmin(pmax(y_pred, eps), 1 - eps)  # Clipping the predictions
return(-mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred)))}

# Step 3: Calculate log loss for both models
log_loss_min <- log_loss(y_test_logistic, logistic_pred_minlogloss)
log_loss_1se <- log_loss(y_test_logistic, logistic_pred_1selogloss)

# Step 4: Print the results
log_loss_results <- data.frame(Model = c("Logistic Regression"),
                               Log_loss_lambdamin = log_loss_min,
                               Log_loss_lambda1se = log_loss_1se )
print(log_loss_results)

