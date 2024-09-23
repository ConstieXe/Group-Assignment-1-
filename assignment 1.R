Team_2_Brazil_data_census$House_services <- Team_2_Brazil_data_census$AGUA_ESGOTO + Team_2_Brazil_data_census$T_SLUZ

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

model <- lm(R1040 ~ AGUA_ESGOTO + CODMUN6 + E_ANOSESTUDO + ESPVIDA + FECTOT + HOMEMTOT + T_MED18M + MORT1 + MULHERTOT + PAREDE + pesoRUR + pesotot + pesourb + PRENTRAB + RAZDEP + RDPC + SOBRE60 + T_ANALF15M + T_ATIV2529 + T_DENS + T_DES2529 + T_M10A14CF + T_NESTUDA_NTRAB_MMEIO + T_OCUPDESLOC_1 + TRABSC + T_SLUZ + UF)


model_2 <- lm(R1040 ~ AGUA_ESGOTO + E_ANOSESTUDO + ESPVIDA + FECTOT + HOMEMTOT + MORT1 + MULHERTOT + PAREDE + pesoRUR + RAZDEP + RDPC + T_ATIV2529 + T_DENS + T_DES2529 + T_NESTUDA_NTRAB_MMEIO + T_SLUZ,
              singular.ok = FALSE)


model_3 <- lm(R1040 ~ E_ANOSESTUDO + ESPVIDA + FECTOT + HOMEMTOT + MORT1 + MULHERTOT + PAREDE + pesoRUR + RAZDEP + RDPC + T_ATIV2529 + T_DENS + T_DES2529 + T_NESTUDA_NTRAB_MMEIO + House_services,
              singular.ok = FALSE)
