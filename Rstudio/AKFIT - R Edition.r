#AKFIT R Edition
#Rafel Vanhoz Ribeiro

# rm(list = ls()) #FUNCAO QUE APAGA TODOS OS OBJETOS CRIADOS NO WORKSPACE ANTERIORMENTE
cat("\014") #FUNCAO QUE LIMPA TODAS AS INFORMACOES DO CONSOLE. NECESSARIO PARA NAO TRANSFERIR LIXO NO ARQUIVO DE SAIDA.

#INICIO
#escolhendo a biblioteca.

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#escolhendo os pacotes para a execucao do código

library(tidyverse)  #Tidtverse é uma coletânea de biblioteca para análise de dados
library(minpack.lm) #minpack é uma biblioteca para realização do levenberg - marquardt
library(matlib) #matlib é uma biblioteca que realiza calculos matriciais e inversões de matrizes
library(scales) #Scales é uma bibioteca que configura e configura, de modo facil, as notacoes cientificas

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

# abrindo o arquivo de dados dos elementos

data = readLines(file.choose()) #Abre janela pra escolher o arquivo e lê suas linhas
data = data[ - 1] #remove a primeira linha do arquivo (cabeçalho)
data = data[ - 1] #remove a primeira linha do arquivo após remover a primeira linha anterior (nome das colunas)
data = data.frame(data) #transforma o arquivo de dados e data frame

data = separate( #função para separar as colunas do arquivo de dados
  data, "data", c(
    'Isotopo', 
    'Egama', 
    'IN', 
    'Efic', 
    'sEfic', 
    'Eres', 
    'sEres', 
    'AspCd', 
    'sAspCd', 
    'Asp', 
    'sAsp', 
    'Rcd', 
    'sRcd', 
    'k0', 
    'sk0', 
    'FCd', 
    'sFCd', 
    'Gepi', 
    'sGepi', 
    'Gth', 
    'sGth', 
    'Q0', 
    'sQ0', 
    'idouro'),
  sep = '\\s{1,}'
)

#transformando cada columa como variáveis numéricas

Isotopo = as.character(data$Isotopo)
Egama = as.numeric(data$Egama)
IN = as.numeric(data$IN)
Efic = as.numeric(data$Efic)
sEfic = as.numeric(data$sEfic)
Eres = as.numeric(data$Eres)
sEres = as.numeric(data$sEres)
AspCd = as.numeric(data$AspCd)
sAspCd = as.numeric(data$sAspCd)
Asp = as.numeric(data$Asp)
sAsp = as.numeric(data$sAsp)
Rcd = as.numeric(data$Rcd)
sRcd = as.numeric(data$sRcd)
k0 = as.numeric(data$k0)
sk0 = as.numeric(data$sk0)
FCd = as.numeric(data$FCd)
sFCd = as.numeric(data$sFCd)
Gepi = as.numeric(data$Gepi)
sGepi = as.numeric(data$sGepi)
Gth = as.numeric(data$Gth)
sGth = as.numeric(data$sGth)
Q0 = as.numeric(data$Q0)
sQ0 = as.numeric(data$sQ0)
idouro = as.numeric(data$idouro)

# abrindo o arquivo de dados dos ouros
#mesmo procedimento utilizado anteriormente
dataau = readLines(file.choose())
dataau = dataau[ - 1]
dataau = dataau[ - 1]
dataau = data.frame(dataau)

dataau = separate(
  dataau, "dataau", c(
    'Isotopo', 
    'Egama', 
    'IN', 
    'Efic', 
    'sEfic', 
    'Eres', 
    'sEres', 
    'AspCd', 
    'sAspCd', 
    'Asp', 
    'sAsp', 
    'Rcd', 
    'sRcd', 
    'k0', 
    'sk0', 
    'FCd', 
    'sFCd', 
    'Gepi', 
    'sGepi', 
    'Gth', 
    'sGth', 
    'Q0', 
    'sQ0'),
  sep = '\\s{1,}'
)

Isotopoau = as.character(dataau$Isotopo)
Egamaau = as.numeric(dataau$Egama)
INau = as.numeric(dataau$IN)
Eficau = as.numeric(dataau$Efic)
sEficau = as.numeric(dataau$sEfic)
Eresau = as.numeric(dataau$Eres)
sEresau = as.numeric(dataau$sEres)
AspCdau = as.numeric(dataau$AspCd)
sAspCdau = as.numeric(dataau$sAspCd)
Aspau = as.numeric(dataau$Asp)
sAspau = as.numeric(dataau$sAsp)
Rcdau = as.numeric(dataau$Rcd)
sRcdau = as.numeric(dataau$sRcd)
k0au = as.numeric(dataau$k0)
sk0au = as.numeric(dataau$sk0)
FCdau = as.numeric(dataau$FCd)
sFCdau = as.numeric(dataau$sFCd)
Gepiau = as.numeric(dataau$Gepi)
sGepiau = as.numeric(dataau$sGepi)
Gthau = as.numeric(dataau$Gth)
sGthau = as.numeric(dataau$sGth)
Q0au = as.numeric(dataau$Q0)
sQ0au = as.numeric(dataau$sQ0)

#O comentário abaixo refere-se ao novo padrão de arquivo de dados para o novo cálculo dos parâmetros no futuro, com inclusao dos parametros que compoem o Asp e AspCd

# data2 = readLines(file.choose())
# data2 = data2[ - 1]
# data2 = data2[ - 1]
# data2 = data.frame(data2)
# 
# data2 = separate(
#   data2, "data2", c(
#   'Isotopo',
#   'N', 
#   'sN',
#   'NCd',
#   'sNCd',
#   'fz',
#   'sfz',
#   'fzCd',
#   'sfzCd',
#   'fa',
#   'sfa',
#   'faCd',
#   'sfaCd',
#   'D',
#   'sD',
#   'DCd',
#   'sDCd',
#   'C',
#   'Sc',
#   'CCd',
#   'sCCd',
#   'S',
#   'sS',
#   'SCd',
#   'sSCd',
#   'w',
#   'sw',
#   'wCd',
#   'swCd',
#   'Egama', 
#   'IN', 
#   'Efic', 
#   'sEfic', 
#   'Eres', 
#   'sEres', 
#   'k0', 
#   'sk0', 
#   'Fcd', 
#   'sFCd', 
#   'Gepi', 
#   'sGepi', 
#   'Gth', 
#   'sGth', 
#   'Q0', 
#   'sigma',
#   'idouro'),
#   sep = '\\s{1,}'
# )
# 
# # data2au = readLines(file.choose())
# # data2au = data2au[ - 1]
# # data2au = data2au[ - 1]
# # data2au = data.frame(data2au)
# 
# data2au = separate(
#   data2au, "data2au", c(
#   'Isotopo',
#   'N', 
#   'sN',
#   'NCd',
#   'sNCd',
#   'fz',
#   'sfz',
#   'fzCd',
#   'sfzCd',
#   'fa',
#   'sfa',
#   'faCd',
#   'sfaCd',
#   'D',
#   'sD',
#   'DCd',
#   'sDCd',
#   'C',
#   'Sc',
#   'CCd',
#   'sCCd',
#   'S',
#   'sS',
#   'SCd',
#   'sSCd',
#   'w',
#   'sw',
#   'wCd',
#   'swCd',
#   'Egama', 
#   'IN', 
#   'Efic', 
#   'sEfic', 
#   'Eres', 
#   'sEres', 
#   'k0', 
#   'sk0', 
#   'Fcd', 
#   'sFCd', 
#   'Gepi', 
#   'sGepi', 
#   'Gth', 
#   'sGth', 
#   'Q0', 
#   'sigma'),
#   sep = '\\s{1,}'
# )

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#Calcula incerteza da Enegia de Ressonancia (sEres) e k0 de referencia

sEres = sEres * Eres / 100
sEresau = sEresau * Eresau / 100
sk0 = k0*(sk0/100)

#Define uma variavel que conta o total de linhas do arquivo de dados, necessario para calculos

N = nrow(data)

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#Calcula o Y Superior da matrix Y

Ys = expression(log(AspCd / Q0 / Efic / FCd / Gepi))
Ysuperior = eval(Ys)

#Calcula o Y Inferior da matrix Y

Yi = expression(log(((Asp - AspCd / FCd) / (Aspau - AspCdau / FCdau)) * (Gthau / Gth * Eficau / Efic)))
Yinferior = 0
for(i in 1:N){
  Yinferior[i] = log(((Asp[i] - AspCd[i] / FCd[i]) / (Aspau[idouro[i]] - AspCdau[idouro[i]] / FCdau[idouro[i]])) * (Gthau[idouro[i]] / Gth[i] * Eficau[idouro[i]] / Efic[i]))
}

Y = c(Ysuperior,Yinferior) #define o veto Y, composto por Ysuperior e Yinferior

#derivadas parciais de Ysuperior e Yinferior

#Ysuperior
dYsAspCd = eval(D(Ys,'AspCd'))
dYsQ0 = eval(D(Ys,'Q0'))
dYsEfic = eval(D(Ys,'Efic'))
dYsFCd = eval(D(Ys,'FCd'))
dYsGepi = eval(D(Ys,'Gepi'))

#Yinferior
dYiAsp = 0
dYiAspCd = 0
dYiFCd = 0
dYiGth = 0
dYiEfic = 0
dYiAspau = 0
dYiAspCdau = 0
dYiFCdau = 0
dYiGthau = 0
dYiEficau = 0
for (i in 1 : N) {
  dYiAsp[i] = 1/(Aspau[idouro[i]] - AspCdau[idouro[i]]/FCdau[idouro[i]]) * (Gthau[idouro[i]]/Gth[i] * Eficau[idouro[i]]/Efic[i])/(((Asp[i] - AspCd[i]/FCd[i])/(Aspau[idouro[i]] - AspCdau[idouro[i]]/FCdau[idouro[i]])) * (Gthau[idouro[i]]/Gth[i] * Eficau[idouro[i]]/Efic[i]))
  dYiAspCd[i] = -(1/FCd[i]/(Aspau[idouro[i]] - AspCdau[idouro[i]]/FCdau[idouro[i]]) * (Gthau[idouro[i]]/Gth[i] * Eficau[idouro[i]]/Efic[i])/(((Asp[i] - AspCd[i]/FCd[i])/(Aspau[idouro[i]] - AspCdau[idouro[i]]/FCdau[idouro[i]])) * (Gthau[idouro[i]]/Gth[i] * Eficau[idouro[i]]/Efic[i])))
  dYiFCd[i] = AspCd[i]/FCd[i]^2/(Aspau[idouro[i]] - AspCdau[idouro[i]]/FCdau[idouro[i]]) * (Gthau[idouro[i]]/Gth[i] * Eficau[idouro[i]]/Efic[i])/(((Asp[i] - AspCd[i]/FCd[i])/(Aspau[idouro[i]] - AspCdau[idouro[i]]/FCdau[idouro[i]])) * (Gthau[idouro[i]]/Gth[i] * Eficau[idouro[i]]/Efic[i]))
  dYiGth[i] = -(((Asp[i] - AspCd[i]/FCd[i])/(Aspau[idouro[i]] - AspCdau[idouro[i]]/FCdau[idouro[i]])) * (Gthau[idouro[i]]/Gth[i]^2 * Eficau[idouro[i]]/Efic[i])/(((Asp[i] - AspCd[i]/FCd[i])/(Aspau[idouro[i]] - AspCdau[idouro[i]]/FCdau[idouro[i]])) * (Gthau[idouro[i]]/Gth[i] * Eficau[idouro[i]]/Efic[i])))
  dYiEfic[i] = -(((Asp[i] - AspCd[i]/FCd[i])/(Aspau[idouro[i]] - AspCdau[idouro[i]]/FCdau[idouro[i]])) * (Gthau[idouro[i]]/Gth[i] * Eficau[idouro[i]]/Efic[i]^2)/(((Asp[i] - AspCd[i]/FCd[i])/(Aspau[idouro[i]] - AspCdau[idouro[i]]/FCdau[idouro[i]])) * (Gthau[idouro[i]]/Gth[i] * Eficau[idouro[i]]/Efic[i])))
  dYiAspau[i] = - ((Asp[i] - AspCd[i] / FCd[i]) / (Aspau[idouro[i]] - AspCdau[idouro[i]] / FCdau[idouro[i]])^2 * (Gthau[idouro[i]] / Gth[i] * Eficau[idouro[i]] / Efic[i]) / (((Asp[i] - AspCd[i] / FCd[i]) / (Aspau[idouro[i]] - AspCdau[idouro[i]] / FCdau[idouro[i]])) * (Gthau[idouro[i]] /Gth[i] * Eficau[idouro[i]] / Efic[i])))
  dYiAspCdau[i] = (Asp[i] - AspCd[i] / FCd[i]) * (1 / FCdau[idouro[i]]) / (Aspau[idouro[i]] - AspCdau[idouro[i]] / FCdau[idouro[i]])^2 * (Gthau[idouro[i]] / Gth[i] * Eficau[idouro[i]] / Efic[i]) / (((Asp[i] - AspCd[i] / FCd[i]) / (Aspau[idouro[i]] - AspCdau[idouro[i]] /FCdau[idouro[i]])) * (Gthau[idouro[i]] / Gth[i] * Eficau[idouro[i]] / Efic[i]))
  dYiFCdau[i] = - ((Asp[i] - AspCd[i] / FCd[i]) * (AspCdau[idouro[i]] / FCdau[idouro[i]]^2) / (Aspau[idouro[i]] - AspCdau[idouro[i]] / FCdau[idouro[i]])^2 * (Gthau[idouro[i]] / Gth[i] * Eficau[idouro[i]] / Efic[i]) / (((Asp[i] - AspCd[i] / FCd[i]) / (Aspau[idouro[i]] - AspCdau[idouro[i]] / FCdau[idouro[i]])) * (Gthau[idouro[i]] / Gth[i] * Eficau[idouro[i]] / Efic[i])))
  dYiGthau[i] = ((Asp[i] - AspCd[i] / FCd[i]) / (Aspau[idouro[i]] - AspCdau[idouro[i]] / FCdau[idouro[i]])) * (1 / Gth[i] * Eficau[idouro[i]] / Efic[i]) / (((Asp[i] - AspCd[i] / FCd[i]) / (Aspau[idouro[i]] - AspCdau[idouro[i]] / FCdau[idouro[i]])) * (Gthau[idouro[i]] / Gth[i] * Eficau[idouro[i]] / Efic[i]))
  dYiEficau[i] = ((Asp[i] - AspCd[i] / FCd[i]) / (Aspau[idouro[i]] - AspCdau[idouro[i]] / FCdau[idouro[i]])) * (Gthau[idouro[i]] / Gth[i] / Efic[i]) / (((Asp[i] - AspCd[i] / FCd[i]) / (Aspau[idouro[i]] - AspCdau[idouro[i]] / FCdau[idouro[i]])) * (Gthau[idouro[i]] / Gth[i] * Eficau[idouro[i]] / Efic[i]))
}

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

# #Calculo da Variancia e Matriz de covariancia

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

varsup = (dYsAspCd * sAspCd)^2 + (dYsQ0 * sQ0)^2 + (dYsEfic * sEfic)^2 + (dYsFCd * sFCd)^2 + (dYsGepi * sGepi)^2

varinf = 0
for (i in 1 : N) {
  varinf[i] = (dYiAsp[i] * sAsp[i])^2 + (dYiAspCd[i] * sAspCd[i])^2 + (dYiFCd[i] * sFCd[i])^2 + (dYiGth[i] * sGth[i])^2 + (dYiEfic[i] * sEfic[i])^2 +
    (dYiAspau[idouro[i]] * sAspau[idouro[i]])^2 + (dYiAspCdau[idouro[i]] * sAspCdau[idouro[i]])^2 + (dYiFCdau[idouro[i]] * sFCdau[idouro[i]])^2 +
    (dYiGthau[idouro[i]] * sGthau[idouro[i]])^2 + (dYiEficau[idouro[i]] * sEficau[idouro[i]])^2
}

varY = c(varsup,varinf)
vY = diag(varY)

for(i in 1:N) {
  for(j in i + 1:N) {
    if(isTRUE(Isotopo[i] == Isotopo[j]) & isTRUE(idouro[i] == idouro[j])) {
      vY[i,j] = dYsQ0[i] * dYsQ0[j] * sQ0[i]^2 + dYsFCd[i] * dYsFCd[j] * sFCd[i]^2 + dYsGepi[i] * dYsGepi[j] * sGepi[i]^2
      vY[j,i] = vY[i,j]
    }
  }
}

for(i in 1:N) {
  for(j in (N + 1):(N * 2)) {
    if(isTRUE(Egama[i]==Egama[j - N]) & isTRUE(idouro[i]==idouro[j - N])) {
      k = j - N
      vY[i,j] = dYsAspCd[i] * dYiAspCd[k] * sAspCd[i]^2 + dYsFCd[i] * dYiFCd[k] * sFCd[i]^2 + dYsEfic[i] * dYiEfic[k] * sEfic[i]^2 
      vY[j,i] = vY[i,j]
    }
  }
}

for(i in 1:N) {
  for(j in (N + 1):(N * 2)) {
    if(isTRUE(Isotopo[i] == Isotopo[j - N]) & isFALSE(Egama[i] == Egama[j - N]) & isTRUE(idouro[i] == idouro[j - N])) {
      k = j - N
      vY[i,j] = dYsFCd[i] * dYiFCd[k] * sFCd[i]^2
      vY[j,i] = vY[i,j]
    }
  }
}

for(i in (N + 1):(N * 2)) {
  for(j in (i + 1):(N * 2)) {
    if(isTRUE(Isotopo[i - N] == Isotopo[j - N]) & isFALSE(Egama[i - N] == Egama[j - N]) & isTRUE(idouro[i - N]==idouro[j - N])) {
      k = i - N
      l = j - N
      vY[i,j] = dYiAspCdau[idouro[k]] * dYiAspCdau[idouro[l]] * sAspCdau[idouro[k]]^2 + dYiAspau[idouro[k]] * dYiAspau[idouro[l]] *
        sAspau[idouro[k]]^2 + dYiFCdau[idouro[k]] * dYiFCdau[idouro[l]] * sFCdau[idouro[k]]^2 + dYiGthau[idouro[k]] *
        dYiGthau[idouro[l]] * sGthau[idouro[k]]^2 + dYiEficau[idouro[k]] * dYiEficau[idouro[l]] * sEficau[idouro[k]]^2 +
        dYiGth[k] * dYiGth[l] * sGth[k]^2 + dYiFCd[k] * dYiFCd[l] * sFCd[k]^2
      vY[j,i] = vY[i,j]
    }
  }
}

for(i in (N + 1):(N * 2)) {
  for(j in (i + 1):(N * 2)) {
    if(isTRUE(Isotopo[i - N] != Isotopo[j - N]) & isTRUE(idouro[i - N] == idouro[j - N])) {
      k = i - N
      l = j - N
      vY[i,j] = dYiAspCdau[idouro[k]] * dYiAspCdau[idouro[l]] * sAspCdau[idouro[k]]^2 + dYiAspau[idouro[k]] * dYiAspau[idouro[l]] * 
        sAspau[idouro[k]]^2 + dYiFCdau[idouro[k]] * dYiFCdau[idouro[l]] * sFCdau[idouro[k]]^2 + dYiGthau[idouro[k]] * 
        dYiGthau[idouro[l]] * sGthau[idouro[k]]^2 + dYiEficau[idouro[k]] * dYiEficau[idouro[l]] * sEficau[idouro[k]]^2
      vY[j,i] = vY[i,j]
    }
  }
}

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#construindo a matrix de planejamento

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#
cols = rep(1, N)
coli = rep(0, N)
col1 = c(cols, coli)

cols2 = 0
for(i in 1:N) {
  cols2[i] = ((Q0[i] + 0.429) * log(Eres[i]) - 0.602) / Q0[i]
}

coli2 = rep(0, N)
col2 = c(cols2, coli2)

Ntotal = N * 2
X = matrix(c(0),nrow=Ntotal, ncol = N)

for(i in 1:N) {
  X[i,i] = 1
  X[(N + i),i] = 1
}

X = cbind(col1, col2, X)

numpar = ncol(X)
vpar = inv(t(X) %*% inv(vY) %*% X)
par = vpar %*% t(X) %*% inv (vY) %*% Y
Yaju = X %*% par
dif = Y - Yaju
A = matrix(c(par[c(1,2)], exp(par[c( - 1, - 2)])))

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#qui quadrado

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#
qui2 = t(dif) %*% inv(vY) %*% dif

#qui quadrado reduzido
gl = Ntotal - numpar
qui2red = qui2 / gl
porbqui = (1-pchisq(qui2,gl))*100

#criando os valores de cada parametro separadamente
a = A[1]
sa = sqrt(vpar[1,1])
alfa = A[2]
salfa = sqrt(vpar[2,2])
k0aju = A[c( - 1, - 2)]
sk0aju = k0aju * sqrt(diag(vpar)[c( - 1, - 2)])

#Correlacao entre a e alfa
coralfa=vpar[1,2]/(sa*salfa)

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#comecando o levenberg

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

Aa = runif(1, min = 15, max = 30)
Aalfa = runif(1, min = 0.00001, max = 0.1)
A2 = c(Aa, Aalfa)

for (i in 1 : N) {
  A2[i+2] = log(i + runif(1, min = 1.001, max = 5))
}

Yexp = matrix(Y)
Yajusup = 0
Yajuinf = 0

R = t(X) %*% inv(vY) %*% X

#valores de lambda e parametros para o loop na hora de fazer a analise
chidif = 1
lambda = 0.000000001
X2 = 0
chi2 = -1
recalcularY = 1
chi2novo = 0

while(1) {
  if(recalcularY == 1) {
    a2 = A2[1]
    alfa2 = A2[2]
    for (i in 1 : N) {
      k0b = A2[(i+2)]
      Yajusup[i] = a2 + ((alfa2 / Q0[i]) * ((Q0[i] + 0.429) * log(Eres[i]) - 0.602)) + log(exp(k0b))
      Yajuinf[i] = log(exp(k0b))
    }
    Yaju2 = matrix(c(Yajusup,Yajuinf))
    datb = Yexp - Yaju2
    Yy = Yexp - Yaju2
    ss = cbind(Yexp, Yaju2)
    if (chi2 == -1) {
      chi2 = t(datb) %*% inv(vY) %*% datb
    }
  }
  Rlambda = matrix(0, nrow = N+2, ncol = N+2)
  for (i in 1: (N+2)) {
    for (j in 1: (N+2)) {
      if (isTRUE(i == j)) {
        Rlambda[i,j] = (1+lambda) * R[i,j]
      } else {
        Rlambda[i,j] = R[i,j]
      }
    }
  }
  DA = inv(Rlambda) %*% t(X) %*% inv(vY) %*% Yy
  Ynovo = X %*% DA
  DD = Yy - Ynovo
  chi2novo = t(DD) %*% inv(vY) %*% DD
  chidif = chi2novo - chi2
  if (chidif >= 0) {
    lambda = lambda * 10
    recalcularY = 1
  } else {
    lambda = lambda / 10
    Anovo = A2 + DA
    A2 = Anovo
    chi2 = chi2novo
    recalcularY = 1
  }
  if (isTRUE(abs(chidif) < 0.00001)) {# & (isTRUE(abs(chidif) > 0.000000001))) {
    break
  }
}

print("saiiiiiiiiiiiii")

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#Calcula o Y Inferior de Q0 da matrix Y

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

Yi2 = expression(log(((FCdau * (Aspau / AspCdau) - 1) * (Gth * Gepiau)) / ((FCd * (Asp / AspCd) - 1) * (Gthau * Gepi)) * Q0au))
Yinferior2 = 0
for(i in 1:N){
  Yinferior2[i] = log(((FCdau[idouro[i]] * (Aspau[idouro[i]] / AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]])) / ((FCd[i] * (Asp[i] / AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]])
}
Y2 = c(Ysuperior,Yinferior2)

#Yinferior
dYiAsp2 = 0
dYiAspCd2 = 0
dYiFCd2 = 0
dYiGth2 = 0
dYiGepi2 = 0
dYiQ0au2 = 0
dYiAspau2 = 0
dYiAspCdau2 = 0
dYiFCdau2 = 0
dYiGthau2 = 0
dYiGepiau2 = 0
for (i in 1 : N) {
  dYiAsp2[i] = -(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]])) * (FCd[i] * (1/AspCd[i]) * (Gthau[idouro[i]] * Gepi[i]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i]))^2 * Q0au[idouro[i]]/(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]]))
  dYiAspCd2[i] = ((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]])) * (FCd[i] * (Asp[i]/AspCd[i]^2) * (Gthau[idouro[i]] * Gepi[i]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i]))^2 * Q0au[idouro[i]]/(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]])
  dYiFCd2[i] = -(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]])) * ((Asp[i]/AspCd[i]) * (Gthau[idouro[i]] * Gepi[i]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i]))^2 * Q0au[idouro[i]]/(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]]))
  dYiGth2[i] = (FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * Gepiau[idouro[i]]/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]]/(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]])
  dYiGepi2[i] = -(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]])) * ((FCd[i] * (Asp[i]/AspCd[i]) - 1) * Gthau[idouro[i]])/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i]))^2 * Q0au[idouro[i]]/(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]]))
  dYiQ0au2[i] = ((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i]))/(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]])
  dYiAspau2[i] = FCdau[idouro[i]] * (1/AspCdau[idouro[i]]) * (Gth[i] * Gepiau[idouro[i]])/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]]/(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]])
  dYiAspCdau2[i] = -(FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]^2) * (Gth[i] * Gepiau[idouro[i]])/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]]/(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]]))
  dYiFCdau2[i] = (Aspau[idouro[i]]/AspCdau[idouro[i]]) * (Gth[i] * Gepiau[idouro[i]])/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]]/(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]])
  dYiGthau2[i] = -(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]])) * ((FCd[i] * (Asp[i]/AspCd[i]) - 1) * Gepi[i])/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i]))^2 * Q0au[idouro[i]]/(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]]))
  dYiGepiau2[i] = (FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * Gth[i]/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]]/(((FCdau[idouro[i]] * (Aspau[idouro[i]]/AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]]))/((FCd[i] * (Asp[i]/AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])) * Q0au[idouro[i]])
}

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#Calculado a nova variancia e nova matrix de covariancia com Q0

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

varsup2 = (dYsAspCd * sAspCd)^2 + (dYsQ0 * sQ0)^2 + (dYsEfic * sEfic)^2 + (dYsFCd * sFCd)^2 + (dYsGepi * sGepi)^2

varinf2 = 0
for (i in 1 : N) {
  varinf2[i] = (dYiAsp2[i] * sAsp[i])^2 + (dYiAspCd2[i] * sAspCd[i])^2 + (dYiFCd2[i] * sFCd[i])^2 + (dYiGth2[i] * sGth[i])^2 +
    (dYiAspau2[idouro[i]] * sAspau[idouro[i]])^2 + (dYiAspCdau2[idouro[i]] * sAspCdau[idouro[i]])^2 + (dYiFCdau2[idouro[i]] * sFCdau[idouro[i]])^2 +
    (dYiGthau2[idouro[i]] * sGthau[idouro[i]])^2 + (dYiQ0au2[idouro[i]] * sQ0au[idouro[i]])^2
}


varY2 = c(varsup2,varinf2)
vY2 = diag(varY2)

for(i in 1:N) {
  for(j in i + 1:N) {
    if(isTRUE(Isotopo[i] == Isotopo[j]) & isTRUE(idouro[i] == idouro[j])) {
      vY2[i,j] = dYsQ0[i] * dYsQ0[j] * sQ0[i]^2 + dYsFCd[i] * dYsFCd[j] * sFCd[i]^2 + dYsGepi[i] * dYsGepi[j] * sGepi[i]^2
      vY2[j,i] = vY2[i,j]
    }
  }
}

for(i in 1:N) {
  for(j in (N + 1):(N * 2)) {
    if(isTRUE(Egama[i]==Egama[j - N]) & isTRUE(idouro[i]==idouro[j - N])) {
      k = j - N
      vY2[i,j] = dYsAspCd[i] * dYiAspCd2[k] * sAspCd[i]^2 + dYsFCd[i] * dYiFCd2[k] * sFCd[i]^2 
      vY2[j,i] = vY2[i,j]
    }
  }
}

for(i in 1:N) {
  for(j in (N + 1):(N * 2)) {
    if(isTRUE(Isotopo[i] == Isotopo[j - N]) & isFALSE(Egama[i] == Egama[j - N]) & isTRUE(idouro[i] == idouro[j - N])) {
      k = j - N
      vY2[i,j] = dYsFCd[i] * dYiFCd2[k] * sFCd[i]^2
      vY2[j,i] = vY2[i,j]
    }
  }
}

for(i in (N + 1):(N * 2)) {
  for(j in (i + 1):(N * 2)) {
    if(isTRUE(Isotopo[i - N] == Isotopo[j - N]) & isFALSE(Egama[i - N] == Egama[j - N]) & isTRUE(idouro[i - N]==idouro[j - N])) {
      k = i - N
      l = j - N
      vY2[i,j] = dYiAspCdau2[idouro[k]] * dYiAspCdau2[idouro[l]] * sAspCdau[idouro[k]]^2 + dYiAspau2[idouro[k]] * dYiAspau2[idouro[l]] *
        sAspau[idouro[k]]^2 + dYiFCdau2[idouro[k]] * dYiFCdau2[idouro[l]] * sFCdau[idouro[k]]^2 + dYiGthau2[idouro[k]] *
        dYiGthau2[idouro[l]] * sGthau[idouro[k]]^2 +  dYiFCdau2[idouro[k]] * dYiQ0au2[idouro[l]] * sQ0au[idouro[k]]^2 +
        dYiGth2[k] * dYiGth2[l] * sGth[k]^2 + dYiFCd2[k] * dYiFCd2[l] * sFCd[k]^2
      vY2[j,i] = vY2[i,j]
    }
  }
}

for(i in (N + 1):(N * 2)) {
  for(j in (i + 1):(N * 2)) {
    if(isTRUE(Isotopo[i - N] != Isotopo[j - N]) & isTRUE(idouro[i - N] == idouro[j - N])) {
      k = i - N
      l = j - N
      vY2[i,j] = dYiAspCdau2[idouro[k]] * dYiAspCdau2[idouro[l]] * sAspCdau[idouro[k]]^2 + dYiAspau2[idouro[k]] * dYiAspau2[idouro[l]] * 
        sAspau[idouro[k]]^2 + dYiFCdau2[idouro[k]] * dYiFCdau2[idouro[l]] * sFCdau[idouro[k]]^2 + dYiGthau2[idouro[k]] * 
        dYiGthau2[idouro[l]] * sGthau[idouro[k]]^2
      vY2[j,i] = vY2[i,j]
    }
  }
}

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#construindo a nova matrix de planejamento

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

cols2 = rep(1, N)
coli2 = rep(0, N)
col12 = c(cols, coli)

cols22 = 0
for(i in 1:N) {
  cols22[i] = ((Q0[i] + 0.429) * log(Eres[i]) - 0.602) / Q0[i]
}

coli22 = rep(0, N)
col22 = c(cols2, coli2)

Ntotal = N * 2
X2 = matrix(c(0),nrow=Ntotal, ncol = N)

for(i in 1:N) {
  X2[i,i] = (alfa2/Q0[i]) * log(Eres[i]) - alfa2/(((FCdau[idouro[i]]*(Aspau[idouro[i]]/AspCdau[idouro[i]])-1)/(FCd[i]*(Asp[i]/AspCd[i])-1))*((Gth[i]*Gepiau[idouro[i]])/((Gthau[idouro[i]]*Gepi[i])))*((Q0au[idouro[i]] - 0.429)/(Eresau[idouro[i]]^-alfa2)+0.429/((2*alfa2+1)*(0.55^alfa2))))^2 * (((((FCdau[idouro[i]]*(Aspau[idouro[i]]/AspCdau[idouro[i]])-1)/(FCd[i]*(Asp[i]/AspCd[i])-1))*((Gth[i]*Gepiau[idouro[i]])/((Gthau[idouro[i]]*Gepi[i])))*((Q0au[idouro[i]] - 0.429)/(Eresau[idouro[i]]^-alfa2)+0.429/((2*alfa2+1)*(0.55^alfa2)))) + 0.429) * log(Eres[i]) - 0.602)
  X2[(N + i),i] = 1/Eres[i]^-alfa2
}

X2 = cbind(col1, col2, X2)

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#comecando o levenberg para Q0

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

Yajusup2 = 0
Yajuinf2 = 0
Yexp = matrix(Y2)

R = t(X2) %*% inv(vY2) %*% X2

#valores de lambda e parametros para o loop na hora de fazer a analise
chidif = 1
lambda = 0.000000001
chi2 = -1
recalcularY = 1
chi2novo = 0

A3 = A2
k0b = A3[c(-1,-2)]

Q0b = 0
for (i in 1 : N) {
  Q0b[i] = log(runif(1, min = 1.9, max = 2))
}

A3 = c(A3[c(1, 2)], Q0b)

Q0alfac = 0
for (i in 1:N) {
  Q0alfac[i] =  ((Q0au[idouro[i]] - 0.429)/(Eres[i]*alfa2)+0.429/((2*alfa2+1)*(0.55**alfa2)))
}

while(1) {
  if(recalcularY == 1) {
    for (i in 1 : N) {
      Q0c = Q0b[i]
      Yajusup2[i] = a2 + ((alfa2 / exp(Q0c)) * ((exp(Q0c) + 0.429) * log(Eres[i]) - 0.602)) + log(exp(k0b[i]))
      Yajuinf2[i] = log((exp(Q0c) - 0.429/((2*alfa2+1)*(0.55^alfa2)))/Eres[i]^-alfa2 + 0.429)
    }
    Yaju3 = matrix(c(Yajusup2,Yajuinf2))
    datb = Yexp - Yaju3
    Yy = Yexp - Yaju3
    ss2 = cbind(Yexp, Yaju3)
    if (chi2 == -1) {
      chi2 = t(datb) %*% inv(vY2) %*% datb
    }
  }
  Rlambda2 = matrix(0, nrow = N+2, ncol = N+2)
  for (i in 1: (N+2)) {
    for (j in 1: (N+2)) {
      if (isTRUE(i == j)) {
        Rlambda2[i,j] = (1+lambda) * R[i,j]
      } else {
        Rlambda2[i,j] = R[i,j]
      }
    }
  }
  DA = inv(Rlambda2) %*% t(X2) %*% inv(vY2) %*% Yy
  Ynovo = X2 %*% DA
  DD = Yy - Ynovo
  chi2novo = t(DD) %*% inv(vY2) %*% DD
  chidif = chi2novo - chi2
  if (chidif >= 0) {
    lambda = lambda * 10
    recalcularY = 1
  } else {
    lambda = lambda / 10
    Anovo = A3 + DA
    A3 = Anovo
    chi2 = chi2novo
    recalcularY = 1
  }
  if (isTRUE(abs(chidif) < 0.000001)) { #} & (isTRUE(abs(chidif) > 0.0000000000000001))) {
    break
  }
}

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#Definindo os ajustes finais e selecionando os parametros e suas incertezas

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

Afinal = A2

for (i in 1 : N) {
  Afinal[i+2] = exp(Afinal[i+2])
}

options(scipen = 1)

Afinal2 = round(matrix(c(Afinal[c(-1,-2)], exp(A3[c(-1,-2)]))), digits = 9)

Q0alfa = 0
for (i in 1 : N) {
  Q0alfa[i] = Afinal2[i+N]
}

f = round(median(((FCd*(Asp/AspCd)-1) * Q0alfa * Gepi)/Gth), digits = 5)

aalfa = round(matrix(Afinal[c(1, 2)]), digits = 5)
aalfaf = c(aalfa, f)

sY = sqrt(diag(vY))
sAfinal = sqrt(abs(diag(inv(Rlambda))))
sAfinalk0 = Afinal * sqrt(abs(diag(inv(Rlambda+Rlambda2))))
sAfinalk0 = matrix(sAfinalk0[c(-1,-2)])
sAfinalQ0 = exp(A3) * sqrt(abs(diag(inv(Rlambda2))))
sAfinalQ0 = matrix(sAfinalQ0[c(-1,-2)])
saalfa = round(sAfinal[c(1,2)], digits = 5)
quirednovo = chi2novo/gl
probchi2 = (1-pchisq(chi2novo,gl))*100
sAfinal2 = matrix(c(sAfinalk0, sAfinalQ0))

sfexp = expression(((FCd*(Asp/AspCd)-1) * Q0alfa * Gepi)/Gth)
dsfFcd = eval(D(sfexp, 'FCd'))
dsfAsp = eval(D(sfexp, 'Asp'))
dsfAspCd = eval(D(sfexp, 'AspCd'))
dsfQ0alfa = eval(D(sfexp, 'Q0alfa'))
dsfGepi = eval(D(sfexp, 'Gepi'))
dsfGth = eval(D(sfexp, 'Gth'))
dsf = sqrt(dsfFcd^2*sFCd^2 +  dsfAsp^2*sAsp^2 + dsfAspCd^2*sAspCd^2 + dsfGepi^2*sGepi^2 + dsfGth^2*sGth^2 +dsfQ0alfa^2*sAfinalQ0^2)
sf = median(dsf)/sqrt(N)

param = matrix(c(Isotopo, Isotopo))

parQ0 = 0
for (i in 1 : N) {
  parQ0[i] = 'Q0'
}

park0 = 0
for (i in 1 : N) {
  park0[i] = 'k0'
}

param2 = cbind(param, c(park0, parQ0), scientific(Afinal2, digits = 5), scientific(sAfinal2, digits = 5), scientific(c(k0,Q0), digits = 5), scientific(c(sk0,sQ0), digits = 5))
param3 = c('a', 'alfa', 'f')
param4 = cbind(param3, aalfaf, '-', c(saalfa, round(sf, digits = 5)), "-", '-')

paramfinal = rbind(param4, param2)
colnames(paramfinal) = c('Alvos','Valores', 'Valores', 'Incerteza', 'Referencia', 'Incerteza')

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#gerando graficos

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

vetorx=0
for (i in 1 : Ntotal) {
  vetorx[i] = 0.95+i*1.005
}

xplot=0
for (i in 1:Ntotal) {
  xplot[i] = i
}

pointname = c(Isotopo, Isotopo)
pointname2 = c(Egama, Egama)

plot(xplot, (DD/sY), main = 'RESIDUOS PONDERADOS PELO DESVIO PADRAO - k0 e Q0', ylab = 'Y-Yaju-sigmaY', xlab = 'elementos', ylim = c(-7, 7))
text(xplot, (DD/sY), labels = pointname, cex = 0.7, pos = 3)
text(xplot, (DD/sY), labels = pointname2, cex = 0.7, pos = 1)
abline(2, 0, lty=3)
abline(-2, 0, lty=3)

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#Gravando os resultados em um arquivo de texto

sink('Resultados.txt')
'AkFIT - R EDITION'
'CODIGO CRIADO POR - RAFAEL VANHOZ'
'VALORES DOS PARAMETROS'
'----------------------------------------------------------------'
'----------------------------------------------------------------'
print(paramfinal)
sink()

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

#Comentarios para calculos de teste, sem utilidade para o programa, apenas para utilidade do desenvolvedor

#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#

# Q0ia = 0
#  for(i in 1:N){
#    Q0ia[i] = (((FCdau[idouro[i]]*(Aspau[idouro[i]]/AspCdau[idouro[i]])-1)/(FCd[i]*(Asp[i]/AspCd[i])-1))*((Gth[i]*Gepiau[idouro[i]])/((Gthau[idouro[i]]*Gepi[i])))*((Q0au[idouro[i]] - 0.429)/(Eresau[idouro[i]]^-alfa2)+0.429/((2*alfa2+1)*(0.55^alfa2))))
#  }
# 
# D(expression((((FCdau*(Aspau/AspCdau)-1)/(FCd*(Asp/AspCd)-1))*((Gth*Gepiau)/((Gthau*Gepi)))*((Q0au - 0.429)/(Eresau^-alfa2)+0.429/((2*alfa2+1)*(0.55^alfa2))))),'alfa2')
# 
# 1/Eres^-alfa2/(((((FCdau[idouro[i]]*(Aspau[idouro[i]]/AspCdau[idouro[i]])-1)/(FCd[i]*(Asp[i]/AspCd[i])-1))*((Gth[i]*Gepiau[idouro[i]])/((Gthau[idouro[i]]*Gepi[i])))*((Q0au[idouro[i]] - 0.429)/(Eresau[idouro[i]]^-alfa2)+0.429/((2*alfa2+1)*(0.55^alfa2)))) - 0.429/((2 * alfa2 + 1) * (0.55^alfa2)))/Eres^-alfa2 + 0.429)
# Q0i = ((((FCdau[idouro[i]]*(Aspau[idouro[i]]/AspCdau[idouro[i]])-1)/(FCd[i]*(Asp[i]/AspCd[i])-1))*((Gth[i]*Gepiau[idouro[i]])/((Gthau[idouro[i]]*Gepi[i])))*((Q0au[idouro[i]] - 0.429)/(Eresau[idouro[i]]^-alfa2)+0.429/((2*alfa2+1)*(0.55^alfa2)))) - 0.429/((2*alfa2+1)*(0.55^alfa2)))/Eres^-alfa2 + 0.429
# Q0i = (Q0ia - 0.429/((2*alfa2+1)*(0.55^alfa2)))/Eres^-alfa2 + 0.429
# Q0d = ((((FCdau[idouro[i]]*(Aspau[idouro[i]]/AspCdau[idouro[i]])-1)/(FCd[i]*(Asp[i]/AspCd[i])-1))*((Gth[i]*Gepiau[idouro[i]])/((Gthau[idouro[i]]*Gepi[i])))*((Q0au[idouro[i]] - 0.429)/(Eresau[idouro[i]]^-alfa2)+0.429/((2*alfa2+1)*(0.55^alfa2)))) - 0.429/((2*alfa2+1)*(0.55^alfa2)))/Eres^-alfa2 + 0.429
