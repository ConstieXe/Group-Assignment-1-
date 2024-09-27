path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

library(readr)
library(glmnet)
library(caret)
library(tibble)
library(tidyverse)
library(dplyr)

brazil_census <- read.csv2("Team_2_Brazil_data_census.csv",
                              sep = ",", dec = ".", header = TRUE)


brazil_census <- brazil_census[,-1] #This removes that useless column 


#creating vectors for state information

df_regions <- tibble(
  UF = c(12, 16, 13, 15, 11, 14, 17, 27, 29, 23, 21, 25, 22, 26, 24, 28, 53, 52, 51, 50, 32, 31, 33, 35, 41, 42, 43),
  state_letters = c("AC", "AP", "AM", "PA", "RO", "RR", "TO", "AL", "BA", "CE", "MA", "PB", "PI", "PE", "RN", "SE", "DF", "GO", "MT", "MS", "ES", "MG", "RJ", "SP", "PR", "SC", "RS"),
  state_names = c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins", "Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Piauí", "Pernambuco", "Rio Grande do Norte", "Sergipe", "Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul", "Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul"),
  region_letters = c(rep("NO", 7), rep("NE", 9), rep("CW", 4), rep("SE", 4), rep("SO", 3)),
  region_names = c(rep("North", 7), rep("Northeast", 9), rep("Central West", 4), rep("Southeast", 4), rep("South", 3)))

df_regions

#Merging the df
brazil_census <- merge(brazil_census, df_regions, by = "UF")

#Changing them to factors
brazil_census <- brazil_census %>%
  mutate(state_letters = as.factor(state_letters),
         state_names = as.factor(state_names),
         region_letters = as.factor(region_letters),
         region_names = as.factor(region_names),
         UF = as.factor(UF),
         CODMUN6 = as.factor(CODMUN6),
         NOMEMUN = as.factor(NOMEMUN))

#Check for missing elements
sum(is.na(brazil_census))

#Check the variables
str(brazil_census)
summary(brazil_census)


# Create the House_services variable
brazil_census$House_services <- brazil_census$AGUA_ESGOTO + brazil_census$T_SLUZ

options(scipen = 999)

####################################################################################################################################

#1.1

model <- lm(R1040 ~ ESPVIDA + FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + T_MED18M 
            + PRENTRAB + RDPC + T_ATIV2529 + T_DES2529 + TRABSC + T_DENS + AGUA_ESGOTO + PAREDE + T_M10A14CF 
            + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + T_SLUZ + HOMEMTOT + MULHERTOT + pesoRUR + pesotot + pesourb + House_services,
            data = brazil_census)

alias(model) # we see that we need to remove HOMEMTOT, T_SLUZ and pesourb

model <- lm(R1040 ~ ESPVIDA + FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + T_MED18M 
            + PRENTRAB + RDPC + T_ATIV2529 + T_DES2529 + TRABSC + T_DENS + AGUA_ESGOTO + PAREDE + T_M10A14CF 
            + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + MULHERTOT + pesoRUR + pesotot + House_services,
            data = brazil_census)

##########################################################################################
vif_values <- vif(model)
print(vif_values)


#we don't remove house service because manager wants it in the model, but we should remove it 

model <- lm(R1040 ~ FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + T_MED18M 
            + PRENTRAB + RDPC + T_ATIV2529 + T_DES2529 + TRABSC + T_DENS + PAREDE + T_M10A14CF 
            + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + MULHERTOT + pesoRUR + House_services,
            data = brazil_census,
            singular.ok =  FALSE)

summary(model)
###################################################################################################################################
#1.2

x <- model.matrix(~ FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + T_MED18M 
                  + PRENTRAB + RDPC + T_ATIV2529 + T_DES2529 + TRABSC + T_DENS + PAREDE + T_M10A14CF 
                  + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + MULHERTOT + pesoRUR + House_services,
                  data = brazil_census)

x <- x[,-1] #remove intercept

y <- brazil_census$R1040

# Run Lasso regression (alpha = 1 for Lasso)
first_lasso_model <- glmnet(x, y, alpha = 1)
plot(first_lasso_model, xvar = "lambda", label = TRUE)

# Display the coefficients
coef(first_lasso_model)
coef(model)

###################################################################################################################################
#2.1

#Divide data into test and train

set.seed(123)

train_indices <- sample(1:nrow(brazil_census), 3150)
train_data <- brazil_census[train_indices, ]
test_data <- brazil_census[-train_indices, ]



x_train <- model.matrix(~ ESPVIDA + FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + T_MED18M 
                        + PRENTRAB + RDPC + T_ATIV2529 + T_DES2529 + TRABSC + T_DENS + AGUA_ESGOTO + PAREDE 
                        + T_M10A14CF + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + T_SLUZ + HOMEMTOT + MULHERTOT
                        + pesoRUR + pesotot + pesourb + House_services,
                        data = train_data)

x_train <- x_train[,-1] #remove intercept

y_train <- train_data$R1040


#########LASSO
set.seed(123)
cvfit_lasso <- cv.glmnet(x = x_train, y = y_train,
                         alpha = 1)

plot(cvfit_lasso)
print(cvfit_lasso)
lambdamin_lasso = cvfit_lasso$lambda.min
best_mse_lasso = min(cvfit_lasso$cvm)

lasso_model_lambdamin <- glmnet(x = x_train, y = y_train,
                      alpha = 1,
                      lambda = lambdamin_lasso)

lasso_model_lambdamin_coef <- coef(lasso_model_lambdamin)
lasso_model_lambdamin_coef

#########Elastic net

set.seed(123)

alpha_values <- seq(0, 1, by = 0.1)

best_alpha_net <- NA
lambdamin_net <- NA
best_mse_net <- Inf

for (alpha_val in alpha_values) {cvfit_net <- cv.glmnet(x = x_train, y = y_train, alpha = alpha_val, nfolds = 10)

if (min(cvfit_net$cvm) < best_mse_net) {
  best_mse_net <- min(cvfit_net$cvm)
  best_alpha_net <- alpha_val
  lambdamin_net <- cvfit_net$lambda.min}}


net_model_lambdamin <- glmnet(x = x_train, y = y_train, alpha = best_alpha_net, lambda = lambdamin_net)

net_model_lambdamin_coef <- coef(net_model_lambdamin)
net_model_lambdamin_coef
##########################################################################################################################
#2.3

models_coef <- cbind(lasso_model_lambdamin_coef, net_model_lambdamin_coef)
colnames(models_coef) <- c("Lasso Coef","Elastic Net Coef")
print(models_coef)#This is the matrix comparing the lasso and elastic net coef

mse_comparison <- cbind(best_mse_lasso, best_mse_net)
colnames(mse_comparison) <- c("Lasso", "Elastic Net")
rownames(mse_comparison) <- "Best MSE"
print(mse_comparison)#This is the matrix comparing the best MSE 
#############################################################################################################################
#2.4
#Lasso Coef path
lasso_coef_path <- glmnet(x_train, y_train, alpha = 1)
plot(lasso_coef_path, xvar = "lambda", label = TRUE)

#Elastic Net Coef path
elasticnet_coef_path <- glmnet(x_train, y_train, alpha = 0.7)
plot(elasticnet_coef_path , xvar = "lambda", label = TRUE)
########################################################################################################
#2.5

x_test <- model.matrix(~ ESPVIDA + FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + T_MED18M 
                       + PRENTRAB + RDPC + T_ATIV2529 + T_DES2529 + TRABSC + T_DENS + AGUA_ESGOTO + PAREDE 
                       + T_M10A14CF + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + T_SLUZ + HOMEMTOT + MULHERTOT
                       + pesoRUR + pesotot + pesourb + House_services,
                       data = test_data)
x_test <- x_test[,-1]

y_test <- test_data$R1040

#store the 1se lambdas for both models
lambda1se_lasso <- cvfit_lasso$lambda.1se
lambda1se_net <- cvfit_net$lambda.1se

#run both models on the 1se lambda 
lasso_model_1se <- glmnet(x_train, y_train, alpha = 1, lambda = lambda1se_lasso)
net_model_1se <- glmnet(x_train, y_train, alpha = best_alpha_net, lambda = lambda1se_net)


#PREDICTIONS
#LASSO
lasso_prediction_lambdamin <- predict(lasso_model_lambdamin, s = lambdamin_lasso, newx = x_test)
lasso_prediction_lambda1se <- predict(lasso_model_1se, s = lambda1se_lasso, newx = x_test)
#ELASTIC NET
net_prediction_lambdamin <- predict(net_model_lambdamin, s = lambdamin_net, newx = x_test)
net_prediction_lambda1se <- predict(net_model_1se, s = lambda1se_net, newx = x_test)

#MSE for each model to see which is better
mse_lassomin <- mean((y_test - lasso_prediction_lambdamin)^2)
mse_lasso1se <- mean((y_test - lasso_prediction_lambda1se)^2)
mse_netmin <- mean((y_test - net_prediction_lambdamin)^2)
mse_net1se <- mean((y_test - net_prediction_lambda1se)^2)

#RMSE for each model to see which is better
rmse_lassomin <- sqrt(mse_lassomin)
rmse_lasso1se <- sqrt(mse_lasso1se)
rmse_netmin <- sqrt(mse_netmin)
rmse_net1se <- sqrt(mse_net1se)

#Data frame to compare results
rmse_comparison_df <- as.data.frame(
  list(c("Lasso Regression", "Elastic Net"),
       c(rmse_lassomin, rmse_netmin),  
       c(rmse_lasso1se, rmse_net1se)))
colnames(rmse_comparison_df) <- c("", "RMSE Lambda min", "RMSE Lambda 1se")
print(rmse_comparison_df)#lasso lambda min is best
#######################################################################################################

#3

brazil_data_prediction <- read.csv2("Brazil_census_data_prediction.csv",
                              sep = ",", dec = ".", header = TRUE)

brazil_data_prediction <- brazil_data_prediction[,-1]

brazil_data_prediction <- brazil_data_prediction %>%
  mutate(UF = as.factor(UF),
         CODMUN6 = as.factor(CODMUN6),
         NOMEMUN = as.factor(NOMEMUN))

brazil_data_prediction$House_services <- brazil_data_prediction$AGUA_ESGOTO + brazil_data_prediction$T_SLUZ

summary(brazil_data_prediction)

sum(is.na(brazil_data_prediction)) # No NA values in data set 


x_new <- model.matrix(~ ESPVIDA + FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + T_MED18M 
                     + PRENTRAB + RDPC + T_ATIV2529 + T_DES2529 + TRABSC + T_DENS + AGUA_ESGOTO + PAREDE 
                     + T_M10A14CF + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + T_SLUZ + HOMEMTOT + MULHERTOT
                     + pesoRUR + pesotot + pesourb + House_services,
                     data = brazil_data_prediction)
x_new <- x_new[,-1]

y_new <- brazil_data_prediction$R1040

new_data_prediction <- predict(lasso_model_lambdamin, s = lambdamin_lasso, newx = x_test)

mse <- mean((y_new - new_data_prediction)^2)
rmse <- sqrt(mse)

####################################################################################################3
#4.1

brazil_census <- brazil_census %>% 
  mutate(Poor = ifelse(R1040 <= 9.5, 1, 0))  # 1 for Poor, 0 for Not Poor

print(brazil_census)


set.seed(123)

train_indices_logistic <- sample(1:nrow(brazil_census), 3150)
train_data_logistic <- brazil_census[train_indices_logistic, ]
test_data_logistic <- brazil_census[-train_indices_logistic, ]



x_train_logistic <- model.matrix(Poor ~ ESPVIDA + FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + T_MED18M 
                        + PRENTRAB + RDPC + T_ATIV2529 + T_DES2529 + TRABSC + T_DENS + AGUA_ESGOTO + PAREDE 
                        + T_M10A14CF + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + T_SLUZ + HOMEMTOT + MULHERTOT
                        + pesoRUR + pesotot + pesourb + House_services,
                        data = train_data_logistic)


x_test_logistic <- model.matrix(Poor ~ ESPVIDA + FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + T_MED18M 
                                 + PRENTRAB + RDPC + T_ATIV2529 + T_DES2529 + TRABSC + T_DENS + AGUA_ESGOTO + PAREDE 
                                 + T_M10A14CF + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + T_SLUZ + HOMEMTOT + MULHERTOT
                                 + pesoRUR + pesotot + pesourb + House_services,
                                 data = test_data_logistic)


x_train_logistic <- x_train_logistic[,-1] 
x_test_logistic <- x_test_logistic[,-1] 


y_train_logistic <- train_data_logistic$Poor
y_test_logistic <- test_data_logistic$Poor


#4.2
cvfit_logistic <- cv.glmnet(x_train_logistic, y_train_logistic, alpha = 0, family = "binomial")
print(cvfit_logistic)
plot(cvfit_logistic)

lambdamin_logistic <- cvfit_logistic$lambda.min
lambda1se_logistic <- cvfit_logistic$lambda.1se

logistic_model_lambdamin <- glmnet(x_train_logistic, y_train_logistic, alpha = 0, lambda = lambdamin_logistic, family = "binomial")

coef(logistic_model_lambdamin)


#4.3

logistic_model_lambda1se <- glmnet(x_train_logistic, y_train_logistic, alpha = 0, lambda = lambda1se_logistic, family = "binomial")

logistic_prediction_lambdamin <- predict(logistic_model_lambdamin, s = lambdamin_logistic, newx = x_test_logistic)
logistic_prediction_lambda1se <- predict(logistic_model_lambda1se, s = lambda1se_logistic, newx = x_test_logistic)


mse_logisticmin <- mean((y_test_logistic - logistic_prediction_lambdamin)^2)
mse_logistic1se <- mean((y_test_logistic - logistic_prediction_lambda1se)^2)

rmse_logisticmin <- sqrt(mse_logisticmin)
rmse_logistic1se <- sqrt(mse_logistic1se)


rmse_logistic <- as.data.frame(
  list(
    c("Logistic Regression"),  
    c(rmse_logisticmin),                
    c(rmse_logistic1se)))

# Rename columns
colnames(rmse_logistic) <- c("", "RMSE Lambda min", "RMSE Lambda 1se")

# Print the comparison data frame
print(rmse_logistic)

logistic_model <- glmnet(x_train_logistic, y_train_logistic, alpha = 0, lambda = lambdamin_logistic, family = "binomial")

coef(logistic_model)
