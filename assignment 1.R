path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

library(readr)
library(glmnet)
library(caret)
library(caretEnsemble)
library(elasticnet)
library(psych)
library(tibble)
library(tidyverse)
library(dplyr)
library(kableExtra)

brazil_census_df <- read.csv2("Team_2_Brazil_data_census.csv",
                                       sep = ",", dec = ".", header = TRUE)


brazil_census_df <- brazil_census_df[,-1] #This removes that useless column 


#creating vectors for state information

df_regions <- tibble(
  UF = c(12, 16, 13, 15, 11, 14, 17, 27, 29, 23, 21, 25, 22, 26, 24, 28, 53, 52, 51, 50, 32, 31, 33, 35, 41, 42, 43),
  state_letters = c("AC", "AP", "AM", "PA", "RO", "RR", "TO", "AL", "BA", "CE", "MA", "PB", "PI", "PE", "RN", "SE", "DF", "GO", "MT", "MS", "ES", "MG", "RJ", "SP", "PR", "SC", "RS"),
  state_names = c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins", "Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Piauí", "Pernambuco", "Rio Grande do Norte", "Sergipe", "Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul", "Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul"),
  region_letters = c(rep("NO", 7), rep("NE", 9), rep("CW", 4), rep("SE", 4), rep("SO", 3)),
  region_names = c(rep("North", 7), rep("Northeast", 9), rep("Central West", 4), rep("Southeast", 4), rep("South", 3)))

df_regions

#Merging the df
brazil_census_df <- merge(brazil_census_df, df_regions, by = "UF")

#Changing them to factors
brazil_census_df <- brazil_census_df %>%
  mutate(state_letters = as.factor(state_letters),
         state_names = as.factor(state_names),
         region_letters = as.factor(region_letters),
         region_names = as.factor(region_names),
         UF = as.factor(UF),
         CODMUN6 = as.factor(CODMUN6),
         NOMEMUN = as.factor(NOMEMUN))

#Check for missing elements
sum(is.na(brazil_census_df))

#Check the variables
str(brazil_census_df)
summary(brazil_census_df)


# Create the House_services variable
brazil_census_df$House_services <- brazil_census_df$AGUA_ESGOTO + brazil_census_df$T_SLUZ

####################################################################################################################################

#1.1

#this model has every variable with the exception of the factors and the ones that have direct correlation 
OLS_model_1 <- lm(R1040 ~ ESPVIDA + FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + T_MED18M 
            + PRENTRAB + RDPC + T_ATIV2529 + T_DES2529 + TRABSC + T_DENS + AGUA_ESGOTO + PAREDE + T_M10A14CF 
            + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + T_SLUZ + HOMEMTOT + pesoRUR + pesotot - 1,
            data = brazil_census_df, 
            singular.ok = FALSE)

options(scipen = 999) #stop showing number in scientific notation

summary(OLS_model_1) # Check our variables

OLS_model_1 <- step(OLS_model_1, direction = "backward") #This step chooses the best variables for the model by removing the ones that dont explain much

summary(OLS_model_1) #these are the best variables, there is a SLIGHT increase in adjusted r and F statistic and RSE decrease

#this is our model
OLS_model_1 <- lm(R1040 ~ ESPVIDA + FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + PRENTRAB
                  + RDPC + T_ATIV2529 + T_DES2529 + T_DENS + AGUA_ESGOTO + PAREDE + T_NESTUDA_NTRAB_MMEIO
                  + T_SLUZ + HOMEMTOT + pesoRUR + pesotot - 1,
                  data = brazil_census_df, 
                  singular.ok = FALSE)

#Now we remove the AGUA_ESGOTO and T_SLUZ variables to include the House_services and see how the model performs
OLS_model_2 <- lm(R1040 ~ ESPVIDA + FECTOT + MORT1 + RAZDEP + SOBRE60 + E_ANOSESTUDO + T_ANALF15M + PRENTRAB
                  + RDPC + T_ATIV2529 + T_DES2529 + T_DENS + PAREDE + T_NESTUDA_NTRAB_MMEIO
                  + HOMEMTOT + pesoRUR + pesotot + House_services - 1,
                  data = brazil_census_df, 
                  singular.ok = FALSE)

summary(OLS_model_2)

#We see that our model under performs with this variable instead of the other 2 


###################################################################################################################################
#1.2

x <- as.matrix(select(brazil_census_df, 
                      ESPVIDA, FECTOT, MORT1, RAZDEP, SOBRE60, E_ANOSESTUDO, T_ANALF15M, PRENTRAB,
                      RDPC, T_ATIV2529, T_DES2529, T_DENS, PAREDE, T_NESTUDA_NTRAB_MMEIO,
                      HOMEMTOT, pesoRUR, pesotot, House_services))

y <- brazil_census_df$R1040

# Run Lasso regression (alpha = 1 for Lasso)
first_lasso_model <- glmnet(x, y, alpha = 1)
plot(first_lasso_model, label = TRUE)



plot(first_lasso_model, xvar = "lambda", label = TRUE)

# Display the coefficients
coef(first_lasso_model)
coef(OLS_model_2)

###################################################################################################################################
#2.1

#Divide data into test and train

set.seed(123)

train_indices <- sample(1:nrow(brazil_census_df), 3150)
train_data <- brazil_census_df[train_indices, ]
test_data <- brazil_census_df[-train_indices, ]



x = train_data[,-c(1,2,3,13,30,31,32,33)] #remove factor variables and dependent variable
y = train_data$R1040

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, verboseIter = TRUE)

#########LASSO
set.seed(123)
lasso_model <- train(x = x, y = y,
                     method = 'glmnet',
                     tuneGrid = expand.grid(alpha = 1,
                                            lambda = seq(0.0001, 1, length =100)),
                     trControl = fitControl)

plot(lasso_model)
lasso_model
plot(lasso_model$finalModel, xvar = "lambda", label = TRUE) #Coef path
plot(varImp(lasso_model, scale = TRUE)) #Coef importance

#See the coef with the best lambda for the lasso regression
best_lambda <- lasso_model$bestTune$lambda
final_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(final_model)

#########Elastic net

set.seed(123)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, verboseIter = TRUE)

elasticNet <- train(x = x, y = y,
                    method = 'glmnet',
                    tuneGrid = expand.grid(alpha = seq(0.0001, 1, length =10),
                                           lambda = seq(0.0001, 1, length =10)),
                    trControl = fitControl)

plot(elasticNet)
elasticNet
plot(elasticNet$finalModel, xvar = "lambda", label = TRUE)
plot(elasticNet$finalModel, xvar = "dev", label = TRUE)
plot(varImp(elasticNet, scale = FALSE))
# Optimal alpha = 0.3334 and lambda = 0.1112

#Comparing Models
listOfModels <- list(Lasso = lasso, ElasticNet = elasticNet)
res <- resamples(listOfModels)
summary(res) 
xyplot(res, metric = 'RMSE')

#best model
elasticNet$bestTune
best <- elasticNet$finalModel











#LASSO WITH GLMNET
set.seed(123)
cvfit <- cv.glmnet(x=as.matrix(x), y, alpha = 1, type.measure = "mse", nfolds = 10,)
print(cvfit)
lasso <- glmnet(x = x, y = y, alpha = 1, lambda = cvfit$lambda.min)
coef(lasso)
lasso

