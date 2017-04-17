rm(list=ls(all=T))
library(nnet)
library(car)
library(mfx)
library(lmtest)

##Cargando la base
base=read.csv("finalfinal.csv",sep=";",header=T)
names(base)

##Preparando variables categóricas
base$decision=as.factor(base$decision)
base$tipo_delito=as.factor(base$tipo_delito)
base$abreviado=as.factor(base$abreviado)
base$tribcoleg=as.factor(base$tribcoleg)
base$extranjero=as.factor(base$extranjero)
base$imputado_mujer=as.factor(base$imputado_mujer)
base$genero_victima=as.factor(base$genero_victima)
base$defensor_pub=as.factor(base$defensor_pub)
base$defensor_mujer=as.factor(base$defensor_muj)
base$fiscal_mujer=as.factor(base$fiscal_mujer)
base$juez_mujer=as.factor(base$juez_mujer)

##Cambiando el la categoría de referencia de la variable tipo de delito
base$tipo_delito<- C(base$tipo_delito, contr.treatment, base=10)
contrasts(base$tipo_delito)


####Modelo 2 (Variable dependiente = Tipo decision)

library(mfx)
test4 <- multinom(decision ~ tipo_delito + tribcoleg + extranjero + imputado_mujer +
                    genero_victima + defensor_pub +
                    exp_defensor + exp_fiscal + fiscal_mujer+ juez_mujer+
                    tipo_delito*defensor_pub , data = base)

test4marginal=logitmfx(decision ~ tipo_delito + tribcoleg + extranjero + imputado_mujer +
                         genero_victima + defensor_pub +
                         exp_defensor + exp_fiscal + fiscal_mujer+ juez_mujer+
                         tipo_delito*defensor_pub , data = base)


sum=summary(test4)



#####Ahora a evaluar el modelo
###se estimarán modelos por separado
###Absolutoria / Condenatoria
base1=base[base$decision==1 | base$decision==0,]
modelof=glm(decision ~ tipo_delito + tribcoleg + extranjero + imputado_mujer +
              genero_victima + defensor_pub +
              exp_defensor + exp_fiscal + fiscal_mujer+ juez_mujer+
              tipo_delito*defensor_pub, data = base1, family="binomial")

source("Osius.R")
H=OR(modelof)

source("ROCplot.R")
pares=ROCplot(modelof,100)
pares$areaROC



###Absolutoria / Sobreseimiento
base2=base[base$decision==2 | base$decision==0,]
modelof=glm(decision ~ tipo_delito + tribcoleg + extranjero + imputado_mujer +
              genero_victima + defensor_pub +
              exp_defensor + exp_fiscal + fiscal_mujer+ juez_mujer +
              tipo_delito*defensor_pub, data = base2, family="binomial")


source("Osius.R")
H=OR(modelof)

source("ROCplot.R")
pares=ROCplot(modelof,100)
pares$areaROC



#################################
#segundo modelo
test1=glm(abreviado ~ tipo_delito + tribcoleg + extranjero + imputado_mujer +
            genero_victima + defensor_pub + defensor_mujer +
            exp_defensor + exp_fiscal + fiscal_mujer+ exp_juez + juez_mujer
          , data = base[base$periodo==0,], family="binomial")


modelof21=logitmfx(abreviado ~ tipo_delito + tribcoleg + extranjero + imputado_mujer +
                     genero_victima + defensor_pub + defensor_mujer +
                     exp_defensor + exp_fiscal + fiscal_mujer+ exp_juez + juez_mujer
                   , data = base[base$periodo==0,])


##Resultados
sum=summary(test1)
