# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Carga de paquetes/librerías necesarias.
function_install_packages <- function(x)
{
  for(i in x)
  {
    if(!require(i, character.only = TRUE))
    {
      install.packages(i, dependencies = TRUE)
      require(i, character.only = TRUE)
    }
  }
}

function_install_packages(c("dplyr", "ggplot2", "corrplot", "ggcorrplot", "e1071", "GGally", "tidyverse", 
                            "ggpubr", "base", "car", "MASS", "leaps", "hier.part", "gvlma", "lmtest", "performance"))

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Importación de los datos.
DATOSP2 <- state.x77

DATOSP2 <- as.data.frame(state.x77)

#Carga de Datos
data(DATOSP2)
head(DATOSP2)
view(DATOSP2)


# Layout de variables.
# Murder:     ratio de asesinatos y homicidios involuntarios por cada 100.000 habitantes.
# Population: población del estado.
# Illiteracy: porcentaje de población analfabeta.
# Income:     ingresos per cápita.
# Frost:      número medio de días con temperatura por debajo del nivel de congelación.


summary(DATOSP2)
names(DATOSP2)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

#Creacion de la variable "ESTADO"(denominación del estado en función de lo indicado por el nombre de la fila de la tabla).
DATOSP2$ESTADO <- row.names(DATOSP2)

# Creación de la variable que indica la división (NORTHEAST, MIDWEST, SOUTH, WEST) a la que pertenece cada uno de los estados.
DATOSP2$DIVISION <- ifelse(DATOSP2$ESTADO %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", 
                                             "Vermont", "New Jersey", "New York", "Pennsylvania"),                                       "NORTHEAST", 
                         
                         ifelse(DATOSP2$ESTADO %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", 
                                                    "Missouri", "Nebraska", "North Dakota", "South Dakota"),                                    "MIDWEST", 
                                
                                ifelse(DATOSP2$ESTADO %in% c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", 
                                                           "Virginia", "District of Columbia", "West Virginia", "Alabama", "Kentucky", 
                                                           "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas"),                  "SOUTH", 
                                       
                                       ifelse(DATOSP2$ESTADO %in% c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", 
                                                                  "Alaska", "California", "Hawaii", "Oregon", "Washington"),                                  "WEST", 
                                              
                                              "NULL"))))

table(DATOSP2$DIVISION, useNA = "always")         # comprobamos que cada estado ha quedado asignado a una única división.
sum(table(DATOSP2$DIVISION)) == nrow(DATOSP2)     #Un numero es igual a otro por lo que  se cumple que el valor que tengo a la izquierda es igual al valor que tengo a la derecha.
                                                  #es la sumatoria del total de estados.
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Correlaciones entre parejas de variables.
cor(DATOSP2[ , c(1:8)])
ggcorrplot(cor(DATOSP2[ , c(1:8)], use = "complete.obs"), hc.order = TRUE, type = "lower", lab = TRUE)

#El Gráfico nos permite observar a priori una alta relacion entre illiteracy y la variable murder, aunque esto no quiere decir que illiteracy cause un incremento directo
#de la variable murder.
#Ademas, obervamos tambien a priori una alta relacion entre HsGrad y Life Exp, y entre HsGrad e Income.
#Luego, obervamos una alta correlacion negativa entre la variable murder y LifeExp.


-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Análisis avanzado de Estadística Descriptiva y Correlaciones entre variables (desglose según división).
ggpairs(DATOSP2[ , c(1:8)], aes(colour = DATOSP2$DIVISION, alpha = 0.5), upper = list(continuous = wrap("cor", alignPercent = 0.5)))

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------name

names(DATOSP2)[4] <- "LifeExp"
names(DATOSP2)[6] <- "HsGrad"

# Regresiones múltiples lineales que relacionan la variable de asesinatos con la población, el analfabetismo, los ingresos y los días con congelamiento.
#En el análisis tomamos como validas las variables con p valor < 0.05, lo que quiere decir que la variable es significativa, y por
#lo tanto explica el modelo.
#p-valor por debajo de 0.05 nos acredita rechazar H0.

#H0: la variable no contribuye a la predicción en el modelo. En términos matemáticos, el coeficiente que lleva en el modelo es cero.
REGRESION_MULTIPLE_1 <- lm(LifeExp ~ Population + Illiteracy + Income + Frost + Area + HsGrad + Murder, data = DATOSP2)
summary(REGRESION_MULTIPLE_1)
#En este caso nos dan significativas en el modelo la variable Hs Grad y Murder,debido a que observamos un p valor
#menor a 0.05.
#A su vez, el R ajustado es de 0.69 lo que quiere decir que el modelo se ajusta pero aun no esta tan cerca de 1, que es cuando las varables
#se ajustan perfectamente para explicar el modelo. Es decir, que el R cuadrado ajustado nos permite observar
#que porcentaje de variacion de la variable dependiente es explicado colectivamente por todas las variables dependientes.


# H1: la variable Illiteracy no contribuye a la predicción en el modelo. En términos matemáticos, el coeficiente que lleva en el modelo es cero.
REGRESION_MULTIPLE_2 <- lm(LifeExp ~ Population + Frost + HsGrad + Murder, data = DATOSP2)
summary(REGRESION_MULTIPLE_2)
#En este modelo, sacamos la variable Illiteracy ya que no resulta significativa en el modelo anterior
#En este caso, la variable Population vemos que el P valor es mayor a 0.05, por lo que no resulta significativa y no explica el modelo.
#Ademas,obervamos que el R cuadrado ajustado mejora con respecto al modelo anterior y llega a 0.7126.

# H2: la variable Population no contribuye a la predicción en el modelo. En términos matemáticos, el coeficiente que lleva en el modelo es cero.
REGRESION_MULTIPLE_3 <- lm(LifeExp ~ + Frost + HsGrad + Murder, data = DATOSP2)
summary(REGRESION_MULTIPLE_3)
#En este caso, todas las variables y el intercepto son significativas.
#El R cuadrado ajustado empeora algo su valor y llega a 0.6939.

# H3: la variable Frost no contribuye a la predicción en el modelo. En términos matemáticos, el coeficiente que lleva en el modelo es cero.
REGRESION_MULTIPLE_4 <- lm(LifeExp ~ HsGrad + Murder, data = DATOSP2)
summary(REGRESION_MULTIPLE_4)

# H4: la variable no contribuye a la predicción en el modelo. En términos matemáticos, el coeficiente que lleva en el modelo es cero.
REGRESION_MULTIPLE_5 <- lm(LifeExp ~ HsGrad + Murder -1, data = DATOSP2)
summary(REGRESION_MULTIPLE_5)

# En este modelo, obervamos que el modelo se explica con HsGrad y Murder.
#Sin el intercepto las dos variables(HsGrad y Murder) explican el modelo y el R1 mejora hasta alcanzar practicamente a 1.
#En este caso, para explicar LifeExp tiene una calta correlacion con las variables HsGrad y Murder.
#A su vez, vemos que el R cuadrado ajustado mejora notablemente hasta llegar a 0.9873, lo que nos da un valor
#cercano a uno. Lo que nos demuestra que las variables explican perfectamente el modelo.
#Es decir, que obtenemos p valores muy pequeños en las variables explicativas, y a su vez, un R cuadrado ajustado cercano a 1.

#Como conclusion podemos decir que LifeExp, se ve explicado por las variables HsGrad y Murder.
#Vemos ademas que sin el intercepto el modelo mejora notablemente.

#Modelo Valido: LifeExp = 1.1247*HsGrad  +  1.39381* Murder 

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Proceso step que sintetiza la información obtenida en los pasos anteriores (seleccionando vía p-valor).
#EN ESTE CASO PLANTEAMOS TODAS LAS OPCIONES DE MODELOS STEP SEGUN LOS MODELOS PLANTEADOS MAS ARRIBA

step(REGRESION_MULTIPLE_1, direction = "backward")

step(REGRESION_MULTIPLE_3, direction = "backward")

step(REGRESION_MULTIPLE_5, direction = "backward")  
#EN ESTE CASO DEL MODELO STEP ULTIMO, DEMOSTRAMOS QUE LIFEEXP TIENE ALTA CORRELACION
#CON LAS VARIAVBLES HSGRAD Y MURDER.



# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Intervalos de confianza al 95% para los coeficientes de las variables predictoras más importantes. CUALES SON LAS MAS IMPORTANTE?
confint(lm(LifeExp ~ Murder + HsGrad -1, data = DATOSP2))

#En este caso al tener el intervalo de confianza al 95%, quiere decir que, de cada 100 veces que hiciera estos calculos
#95% de ellas arrojarian resultados que estan en los intervalos descritos debajo:
2.5 %   97.5 %
#  Murder 0.8636776 1.923936
#HsGrad 1.0434523 1.205995

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Resumen de la evolución sufrida por el R-cuadrado ajustado.
REGRESION_MULTIPLE_5 <-regsubsets(LifeExp  ~ HsGrad + Murder -1 , data = DATOSP2, nbest = 4)
plot(REGRESION_MULTIPLE_5, scale = "adjr2")

#EN ESTE CASO EN EL EJE VERTICAL TENEMOS LA RAIZ DE LOS RESIDUOS, CON EL FIN DE NORMALIZAR DE OTRA FORMA.
#ES UNA DEMOSTRACION DE LO EXPLICADO MAS ARRIBA, EN DONDE VEMOS QUE LA MAYOR CANTIDAD DE
#RESULTADOS SE ENCUENTRAN CONCENTRADOS EN LA PARTE CENTRAL DE LOS FITTED VALUES.

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Importancia relativa de cada variable(de entre las más representativas): aportación al R-cuadrado ajustado final.

(relat.imp.RMSPE = hier.part(DATOSP2$Murder, DATOSP2[ , c("Murder", "HsGrad")], family = "gaussian", gof = "RMSPE", barplot = TRUE))
# En este caso observamos que el efecto de Murder es mucho mas representativo para el modelo que HsGrad
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
#DIAGNOSTICO DEL MODELO.
# Analisis de homocedasticidad Vs heterocedasticidad, normalidad de residuos y valores anómalos en la predicción.

par(mfrow = c(2,2))
plot(REGRESION_MULTIPLE_1)
#EN ESTE CASO, VEMOS QUE EL MODELO PODRIA SER HOMOCEDASTICO, YA QUE EN EL GRAFICO DE RESIDUOS
#OBSERVAMOS QUE LOS RESIDUOS SON IGUALES A LO LARGO DE TODO EL ESPECTRO.
#POR LO QUE NO SE OBSERVA HETEROCEDASTICIDAD(SITUACION EN LA CUAL EL MODELO TIENE VARIANZA EN LOS
#RESIDUOS)
#EN DEFINITIVA, DECIMOS ENTONCES QUE AL HABER HOMOCEDASTICIDAD EN EL MODELO(AUSENCIA DE PATRON), DECIMOS QUE EL MODELO
#ACIERTA POR IGUAL, INDEPENDIENTEMENTE DEL VALOR DE LA PREDICCION QUE ARROJE.

# Test de Breusch-Pagan.
bptest(REGRESION_MULTIPLE_1) # El test nos arroja  p-valor > 0.05: no se rechaza la hipótesis nula de homocedasticidad(MODELO HOMOSCEDÁSTICO).

bptest(REGRESION_MULTIPLE_3) # El test nos arroja  p-valor > 0.05: no se rechaza la hipótesis nula de homocedasticidad(MODELO HOMOSCEDÁSTICO).

# Test de Shapiro-Wilk.
shapiro.test(REGRESION_MULTIPLE_1$residuals)    # p-valor > 0.05: no se rechaza la hipótesis nula de normalidad. RESIDUOS CON DISTRIBUCIÓN NORMAL.
qqPlot(REGRESION_MULTIPLE_1, labels = row.names(DATOSP2), id.method = "identify", simulate = TRUE, main = "Q-Q Plot")

# Test de Durbin-Watson.
dwtest(REGRESION_MULTIPLE_1)    # p-valor > 0.05:por lo que no se rechaza la hipótesis nula de no existencia de autocorrelación.

gvlma(REGRESION_MULTIPLE_1) # Se observa cumplimiento de las condiciones del modelo.
gvlma(REGRESION_MULTIPLE_3) # Se observa cumplimiento de las condiciones del modelo.

vif(REGRESION_MULTIPLE_1) # A modo de prueba realizaoms analsis con REGRESION_MULTIPLE_1
vif(REGRESION_MULTIPLE_3)    # Observamos valores cercanos a 1, por lo tanto vemos ausencia de Colinealidad(A partir de 5 podemos
#pensar que existe MUlticolinealidad es decir un valor cercano a 0.80 de indice de correlacion)

outlierTest(REGRESION_MULTIPLE_1)   #Analsis de Outliers
outlierTest(REGRESION_MULTIPLE_3)   #Analsis de Outliers.


# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

check_model(REGRESION_MULTIPLE_1)
check_model(REGRESION_MULTIPLE_3)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------




