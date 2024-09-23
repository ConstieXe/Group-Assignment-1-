#Do not forget to download (Team_2_Brazil_data_census)

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
model <- lm(R1040 ~ AGUA_ESGOTO + CODMUN6 + E_ANOSESTUDO + ESPVIDA + FECTOT + HOMEMTOT + T_MED18M + MORT1 + MULHERTOT + PAREDE + pesoRUR + pesotot + pesourb + PRENTRAB + RAZDEP + RDPC + SOBRE60 + T_ANALF15M + T_ATIV2529 + T_DENS + T_DES2529 + T_M10A14CF + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + TRABSC + T_SLUZ + UF - 1)

#this model has all the relevant variables minus the ones mentioned above 
model_2 <- lm(R1040 ~ AGUA_ESGOTO + E_ANOSESTUDO + ESPVIDA + FECTOT + HOMEMTOT + MORT1 + MULHERTOT + PAREDE + pesoRUR + RAZDEP + RDPC + T_ATIV2529 + T_DENS + T_DES2529 + T_NESTUDA_NTRAB_MMEIO + T_SLUZ - 1,
              singular.ok = FALSE)

#this model has all the relevant variables - AGUA_ESGOTO and T_SLUZ that were replaced by House_services
model_3 <- lm(R1040 ~ E_ANOSESTUDO + ESPVIDA + FECTOT + HOMEMTOT + MORT1 + MULHERTOT + PAREDE + pesoRUR + RAZDEP + RDPC + T_ATIV2529 + T_DENS + T_DES2529 + T_NESTUDA_NTRAB_MMEIO + House_services - 1,
              singular.ok = FALSE)

summary(model)
summary(model_2)
summary(model_3)

#AFTER I WILL WRITE AN EXPLANTION (, but you should inform what is the model that you are working with and analyze why including House_services to your old model is helpful or not and why. Make a connection to the theory behind OLS - explain in a way that your manager understands)

###########################################################################################################################
#1.2

install.packages("glmnet")
library(glmnet)


y <- R1040
x <- model.matrix(R1040 ~ E_ANOSESTUDO + ESPVIDA + FECTOT + HOMEMTOT + 
                    MORT1 + MULHERTOT + PAREDE + pesoRUR + RAZDEP + RDPC + 
                    T_ATIV2529 + T_DENS + T_DES2529 + T_NESTUDA_NTRAB_MMEIO + 
                    House_services)[,-1]

lasso_model <- glmnet(x, y, alpha = 1)
coef(lasso_model)

###########################################################################################################################


#Question 2.1
set.seed(123)
train_indices <- sample(1:nrow(Team2), 3150)
train_set <- Team2[train_indices, ]
test_set <- Team2[-train_indices, ]
