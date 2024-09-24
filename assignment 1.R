
#Question 1
#1.1
#creating the house_services column
Team_2_Brazil_data_census$House_services <- Team_2_Brazil_data_census$AGUA_ESGOTO + Team_2_Brazil_data_census$T_SLUZ

#creating the variables 
UF <- Team_2_Brazil_data_census$UF
CODMUN6 <- Team_2_Brazil_data_census$CODMUN6
NOMEMUN <- Team_2_Brazil_data_census$NOMEMUN
ESPVIDA <- Team_2_Brazil_data_census$ESPVIDA
FECTOT <- Team_2_Brazil_data_census$FECTOT
MORT1 <- Team_2_Brazil_data_census$MORT1
RAZDEP <- Team_2_Brazil_data_census$RAZDEP
SOBRE60 <- Team_2_Brazil_data_census$SOBRE60
E_ANOSESTUDO <- Team_2_Brazil_data_census$E_ANOSESTUDO
T_ANALF15M <- Team_2_Brazil_data_census$T_ANALF15M
T_MED18M <- Team_2_Brazil_data_census$T_MED18M
PRENTRAB <- Team_2_Brazil_data_census$PRENTRAB
R1040 <- Team_2_Brazil_data_census$R1040
RDPC <- Team_2_Brazil_data_census$RDPC
T_ATIV2529 <- Team_2_Brazil_data_census$T_ATIV2529
T_DES2529 <- Team_2_Brazil_data_census$T_DES2529
TRABSC <- Team_2_Brazil_data_census$TRABSC
T_DENS <- Team_2_Brazil_data_census$T_DENS
AGUA_ESGOTO <- Team_2_Brazil_data_census$AGUA_ESGOTO
PAREDE <- Team_2_Brazil_data_census$PAREDE
T_M10A14CF <- Team_2_Brazil_data_census$T_M10A14CF
T_NESTUDA_NTRAB_MMEIO <- Team_2_Brazil_data_census$T_NESTUDA_NTRAB_MMEIO
T_OCUPDESLOC_1 <- Team_2_Brazil_data_census$T_OCUPDESLOC_1
T_SLUZ <- Team_2_Brazil_data_census$T_SLUZ
HOMEMTOT <- Team_2_Brazil_data_census$HOMEMTOT
MULHERTOT <- Team_2_Brazil_data_census$MULHERTOT
pesoRUR <- Team_2_Brazil_data_census$pesoRUR
pesotot <- Team_2_Brazil_data_census$pesotot
pesourb <- Team_2_Brazil_data_census$pesourb
House_services <- Team_2_Brazil_data_census$House_services

#this model has every variable with the exeption of NOMEMUN (because it is a character variable) and House_services 
model_1 <- lm(R1040 ~ AGUA_ESGOTO + CODMUN6 + E_ANOSESTUDO + ESPVIDA + FECTOT + HOMEMTOT + T_MED18M + MORT1 + MULHERTOT + PAREDE + pesoRUR + pesotot + pesourb + PRENTRAB + RAZDEP + RDPC + SOBRE60 + T_ANALF15M + T_ATIV2529 + T_DENS + T_DES2529 + T_M10A14CF + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + TRABSC + T_SLUZ + UF - 1)

#this model has all the relevant variables minus the ones mentioned above 
model_2 <- lm(R1040 ~ AGUA_ESGOTO + E_ANOSESTUDO + ESPVIDA + FECTOT + HOMEMTOT + MORT1 + MULHERTOT + PAREDE + pesoRUR + RAZDEP + RDPC + T_ATIV2529 + T_DENS + T_DES2529 + T_NESTUDA_NTRAB_MMEIO + T_SLUZ - 1,
              singular.ok = FALSE)

#this model has all the relevant variables - AGUA_ESGOTO and T_SLUZ that were replaced by House_services
model_3 <- lm(R1040 ~ E_ANOSESTUDO + ESPVIDA + FECTOT + HOMEMTOT + MORT1 + MULHERTOT + PAREDE + pesoRUR + RAZDEP + RDPC + T_ATIV2529 + T_DENS + T_DES2529 + T_NESTUDA_NTRAB_MMEIO + House_services - 1,
              singular.ok = FALSE)

summary(model_1)
summary(model_2)
summary(model_3)

#AFTER I WILL WRITE AN EXPLANTION (, but you should inform what is the model that you are working with and analyze why including House_services to your old model is helpful or not and why. Make a connection to the theory behind OLS - explain in a way that your manager understands)

###The first model was used to see what variables were relevant. 
###On model 2 we see our model with only the relevant variables (in this model we use the separated variables (light and water) and not the house_services variable)
###On model 3 we see our model with the house_services variable instead of t he other 2. 
###We need to remove the other 2 because the variable that we created has a perfect multicollinearity with the initial variables.
###This means that we cant apply the OLS method because it requires variable independance.
###We force our linear model to apply the OLS method by setting singular.ok = FALSE and thats why when we try to use all the varibales it gives an error message, so we need to remove the other 2.
###We can see that the model fits better with the data when we have water and light separated instead of having house_services because our adjusted r2 is higher and our residual standard error is lower.

###########################################################################################################################
#1.2

install.packages("glmnet")
library(glmnet)


y <- R1040
X <- model.matrix(R1040 ~ E_ANOSESTUDO + ESPVIDA + FECTOT + HOMEMTOT + 
                    MORT1 + MULHERTOT + PAREDE + pesoRUR + RAZDEP + RDPC + 
                    T_ATIV2529 + T_DENS + T_DES2529 + T_NESTUDA_NTRAB_MMEIO + 
                    House_services + AGUA_ESGOTO + T_SLUZ,
                  data = Team_2_Brazil_data_census)[,-1]

lasso_model <- glmnet(X, y, alpha = 1)
coef(lasso_model)

###The model works and i think that we basically say that the coef are closer to 0 in comparison to the first model.
###Not sure i gees we need to see the lecture slides 

###########################################################################################################################
#Question 2.1

set.seed(123)
train_indices <- sample(1:nrow(Team_2_Brazil_data_census), 3150)
train_set <- Team_2_Brazil_data_census[train_indices, ]
test_set <- Team_2_Brazil_data_census[-train_indices, ]

#Question 2.2

#LASSO

X2 <- model.matrix(R1040 ~ E_ANOSESTUDO + ESPVIDA + FECTOT + HOMEMTOT + 
                    MORT1 + MULHERTOT + PAREDE + pesoRUR + RAZDEP + RDPC + 
                    T_ATIV2529 + T_DENS + T_DES2529 + T_NESTUDA_NTRAB_MMEIO + 
                    House_services + AGUA_ESGOTO + T_SLUZ,
                   data = train_set)[,-1]

y2 <- train_set$R1040

cv_lasso <- cv.glmnet(X2, y2,
                      alpha = 1)

print(cv_lasso$lambda.min) #best cross validated lambda

print(cv_lasso$lambda.1se) #conservative estimate of best lambda 


lasso_bestlambda <- glmnet(X2, y2,
                           alpha = 1,
                           lambda = cv_lasso$lambda.min)

round(lasso_bestlambda$beta, 2)

#ELASTIC_NET with chatgpt

# Set up a sequence of alpha values to test
alpha_seq <- seq(0, 1, by = 0.1)
results <- data.frame(alpha = numeric(), lambda = numeric(), mse = numeric())

# Loop through the alpha values to perform cross-validation
for (a in alpha_seq) {
  enet_model <- cv.glmnet(x, y, alpha = a)  # Cross-validation for Elastic Net
  best_lambda <- enet_model$lambda.min  # Get the best lambda for this alpha
  mse <- min(enet_model$cvm)  # Minimum MSE for the best lambda
  results <- rbind(results, data.frame(alpha = a, lambda = best_lambda, mse = mse))
}

# Find the best alpha and corresponding lambda
best_row <- results[which.min(results$mse), ]
best_alpha_enet <- best_row$alpha
best_lambda_enet <- best_row$lambda

# Print the best parameters
cat("Best Elastic Net Lambda:", best_lambda_enet, "\n")
cat("Best Elastic Net Alpha:", best_alpha_enet, "\n")
