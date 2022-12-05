##################### Paquetes requeridos #########################
#auctor Neson MOlina ##
# install.packages("ggplot2")
# install.packages("ggfortify")
# install.packages("tseries")
# install.packages("forecast")
# install.packages("vars")

#################################################
############# Librerias requeridas #######################

library(tseries)
library(ggfortify)
library(forecast)
library(vars)

# En los modelos VAR anteriores se han utilizado datos de las tasas de interes
# (Activas y Pasivas) de los bancos comerciales. Para este modelo VAR se utilizaran
# Datos del sistema financiero hondureño.

##################################### VAR #######################################
# Cargamos la base de datos 

Datos <- read.csv("Proyecto/Datos/BancoCH.csv")

Datos
# Cargamos primero los datos de Tasa de interes activa 
# Y Luego creamos el objeto ts
TasaAct.ts <- ts(Datos$Tasa_Acti, start = c(2005,5), end = c(2021,8), frequency = 12)
TasaAct.ts

# Cargamos los datos de Tasa de Interes Pasiva y creamos el objeto ts
TasaPas.ts <- ts(Datos$Tasa_Pas, start = c(2005,5), end = c(2021,8), frequency = 12)
TasaPas.ts

# Cargamos los datos de Tasa de Politica Monetaria y creamos el objeto ts
TPM.ts <- ts(Datos$TPM, start = c(2005,5), end = c(2021,8), frequency = 12)
TPM.ts

# Cargamos los datos de Indice de Precios al Consumidor y creamos el objeto ts
IPC.ts <- ts(Datos$IPC, start = c(2005,5), end = c(2021,8), frequency = 12)
IPC.ts


# Graficamos las series en conjunto 
ts.plot(TasaAct.ts,TasaPas.ts,TPM.ts,IPC.ts, xlab = "Tiempo", ylab = "Datos",
        col = c("#E5654A","#4AE5E5","#804AE5","#FCF762"), main = "Representacion de las Series",type="s",lwd = 1:6)
legend(2005, 350, legend=c("TIA","TIP","TPM", "IPC"),
       col= c("#E5654A","#4AE5E5","#804AE5","#FCF762"), lty=1:6, cex=0.8)

# Veamos si las series son estacionarias utilizando el test de Dickey-Fuller, \alpha = 5%
# H_{0}: La serie no es estacionaria. p > 0 .05
# H_{1}: La serie es estacionaria p < 0.05

# Para la Tasa de Interes Activa 
Adf <- adf.test(TasaAct.ts)
Adf$p.value
# La serie no es estacionaria. Ya que p = 0.7958637

# Para Tasa Interes Pasiva 
Pdf <- adf.test(TasaPas.ts)
Pdf$p.value
# La serie no es estacionaria. Ya que p = 0.6644017

# Para Tasa de Politica Monetaria
Mdf <- adf.test(TPM.ts)
Mdf$p.value
# La serie no es estacionaria. Ya que p = 0.2054491

# Para Indice de Precios al Consumidor
Idf <- adf.test(IPC.ts)
Idf$p.value
# La serie no es estacionaria. Ya que p =0.4039159

##############3 Aplicamos Logaritmo ##########################
TasaAct_log <- log(TasaAct.ts)

TasaPas_log <- log(TasaPas.ts)

TPM_log <- log(TPM.ts)

IPC_log <- log(IPC.ts)

####################### Aplicamos una diferencia ##################
TasaAct_dif1 <- diff(TasaAct_log)
TasaPas_dif1 <- diff(TasaPas_log)
TPM_dif1 <- diff(TPM_log)
IPC_dif1 <- diff(IPC_log)

# Graficamos las series con una diferencia
ts.plot(TasaAct_dif1,TasaPas_dif1,TPM_dif1,IPC_dif1, xlab = "Tiempo", ylab = "Datos",
        col = c("#E5654A","#4AE5E5","#804AE5","#FCF762"), lty=1:6, main = "Representacion de las Series con una Diferencia")
legend(2005, -0.1, legend=c("TIA","TIP","TPM", "IPC"),
       col=c("#E5654A","#4AE5E5","#804AE5","#FCF762"), lty=1:6, cex=0.8)

# Veamos si las series son estacionarias utilizando el test de Dickey-Fuller, \alpha = 5%
# Despues de haber aplicado una diferencia
# H_{0}: La serie no es estacionaria. p > 0 .05
# H_{1}: La serie es estacionaria p < 0.05

# Para la Tasa de Interes Activa 
Adf1 <- adf.test(TasaAct_dif1)
Adf1$p.value
# La serie es estacionaria. 

# Para Tasa Interes Pasiva 
Pdf1 <- adf.test(TasaPas_dif1)
Pdf1$p.value
# La serie es estacionaria. 

# Para Tasa de Politica Monetaria
Mdf1 <- adf.test(TPM_dif1)
Mdf1$p.value
# La serie es estacionaria.

# Para Indice de Precios al Consumidor
Idf1 <- adf.test(IPC_dif1)
Idf1$p.value
# La serie es estacionaria


# Se crea un nuevo data frame para la identificacion del VAR
####################### Creando un nuevo data frame ###################3333
A <- TasaAct_dif1
P <- TasaPas_dif1
M <- TPM_dif1
I <- IPC_dif1
Datos_VAR2 <- data.frame(A,P,M,I)

# Identificacion de nuestro modelo VAR
VARselect(Datos_VAR2, type = "const")

# Seleccionamos nuestro modelo con 3 retardos
Var4 <- VAR(Datos_VAR2,p=3)
Var4
summary(Var4)

# Pruebas de Especificacion 
# Prueba de Autocorrelacion Serial en los Residuales
# H_{0}: Los Residuales no estan correlacionados, > 0.05
# H_{1}: Los residuales si estan correlacionados, p < 0.05
serial4 <- serial.test(Var4, lags.pt = 3, type = "PT.asymptotic")
serial4$serial
# Concluimos que si hay una correlacion serial

# ****************************************************#
# Prueba de Normalidad de los Residuales
# H_{0}: Los Residuales se distribuyen normal > 0.05
# H_{1}: Los residuales no se distribuyen normal < 0.05
NorVar4 <- normality.test(Var4)
NorVar4$jb.mul
# ****************************************************#

# Prueba de Homocedasticidad de la Varianza de los residuales
# H_{0}: La Varianza de los residuales es constante >0.05
# H_{1}: L Varianza de los residuales no es constante <0.05
arch1 <- arch.test(Var4, lags.multi = 12)
arch1$arch.mul
# Se concluye que la varianza no es constante


########################## Causalidad de Grnager ##########################
# Aplicando Los tests de Causalidad de Ganger concluimos que:
grangertest(M~A, order = 4)
grangertest(A~M, order = 1)
grangertest(P~M, order = 1)
grangertest(A~I, order = 1)
grangertest(P~I, order = 5)
grangertest(M~I, order = 1)
grangertest(A~P, order = 1)
grangertest(M~P, order = 3)

impulso <- irf(Var4)
impulso
plot(impulso)

