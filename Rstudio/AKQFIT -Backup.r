#AKFIT R Edition
#Rafel Vanhoz Ribeiro

#INICIO
#escolhendo a biblioteca.

library(tidyverse)  #Tidtverse é uma coletânea de biblioteca para análise de dados
library(minpack.lm) #minpack é uma biblioteca para realização do levenberg - marquardt
library(matlib) #matlib é uma biblioteca que realiza calculos matriciais e inversões de matrizes

nls

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

#O comentário abaixo refere - se ao novo padrão de arquivo de dados para o novo cálculo dos parâmetros

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

#Calcula erro da Enegia de Ressonancia (sEres)
sEres = sEres * Eres / 100
sEresau = sEresau * Eresau / 100

#Define uma variavel que conta o total de linhas do arquivo de dados
N = nrow(data)

#Calcula o Y Superior da matrix Y
Ys = expression(log(AspCd / (Efic * FCd * Gepi)))
Ysuperior = eval(Ys)

#Calcula o Y Medio da Matrix Y
Ym = expression((log((FCdau * (Aspau / AspCdau) - 1) * (Gth * Gepiau)) / ((FCd * (Asp / AspCd) - 1) * (Gthau * Gepi))))
Ymedio = 0
for (i in 1:N) {
  Ymedio[i] = log(((FCdau[idouro[i]] * (Aspau[idouro[i]] / AspCdau[idouro[i]]) - 1) * (Gth[i] * Gepiau[idouro[i]])) / ((FCd[i] * (Asp[i] / AspCd[i]) - 1) * (Gthau[idouro[i]] * Gepi[i])))
}

#Calcula o Y Inferior da matrix Y
Yi = expression(log(((Asp - AspCd / FCd) / (Aspau - AspCdau / FCdau)) * (Gthau / Gth * Eficau / Efic)))
Yinferior = 0
for(i in 1:N){
  Yinferior[i] = log(((Asp[i] - AspCd[i] / FCd[i]) / (Aspau[idouro[i]] - AspCdau[idouro[i]] / FCdau[idouro[i]])) * (Gthau[idouro[i]] / Gth[i] * Eficau[idouro[i]] / Efic[i]))
}
Y = c(Ysuperior, Ymedio, Yinferior)

#derivadas parciais de Ysuperior e Yinferior

#Ysuperior
dYsAspCd = eval(D(Ys,'AspCd'))
dYsEfic = eval(D(Ys,'Efic'))
dYsFCd = eval(D(Ys,'FCd'))
dYsGepi = eval(D(Ys,'Gepi'))

#Ymedio sem ouro
dYmAsp = eval(D(Ym, 'Asp'))
dYmAspCd = eval(D(Ym, 'AspCd'))
dYmFCd = eval(D(Ym, 'FCd'))
dYmGth = eval(D(Ym, 'Gth'))
dYmGepi = eval(D(Ym, 'Gepi'))

#Ymedio com ouro
dYmAspauexp = D(expression(log(((FCdau * (Aspau / AspCdau) - 1) * (Gth * Gepiau)) / ((FCd * (Asp / AspCd) - 1) * (Gthau * Gepi)))+log(Q0au)), 'Aspau')
dYmAspau = eval(FCdau[idouro] * (1/AspCdau[idouro]) * (Gth * Gepiau[idouro])/((FCd * (Asp/AspCd) - 1) * (Gthau[idouro] * Gepi))/(((FCdau[idouro] * (Aspau[idouro]/AspCdau[idouro]) - 1) * (Gth * Gepiau[idouro]))/((FCd * (Asp/AspCd) - 1) * (Gthau[idouro] * Gepi))))

dYmAspCdauexp = D(expression(log(((FCdau * (Aspau / AspCdau) - 1) * (Gth * Gepiau)) / ((FCd * (Asp / AspCd) - 1) * (Gthau * Gepi)))+log(Q0au)), 'AspCdau')
dYmAspCdau = eval(-(FCdau[idouro] * (Aspau[idouro]/AspCdau[idouro]^2) * (Gth * Gepiau[idouro])/((FCd * (Asp/AspCd) - 1) * (Gthau[idouro] * Gepi))/(((FCdau[idouro] * (Aspau[idouro]/AspCdau[idouro]) - 1) * (Gth * Gepiau[idouro]))/((FCd * (Asp/AspCd) - 1) * (Gthau[idouro] * Gepi)))))

dYmFCdauexp = D(expression(log(((FCdau * (Aspau / AspCdau) - 1) * (Gth * Gepiau)) / ((FCd * (Asp / AspCd) - 1) * (Gthau * Gepi)))+log(Q0au)), 'FCdau')
dYmFCdau = eval((Aspau[idouro]/AspCdau[idouro]) * (Gth * Gepiau[idouro])/((FCd * (Asp/AspCd) - 1) * (Gthau[idouro] * Gepi))/(((FCdau[idouro] * (Aspau[idouro]/AspCdau[idouro]) - 1) * (Gth * Gepiau[idouro]))/((FCd * (Asp/AspCd) - 1) * (Gthau[idouro] * Gepi))))

dYmGthauexp = D(expression(log(((FCdau * (Aspau / AspCdau) - 1) * (Gth * Gepiau)) / ((FCd * (Asp / AspCd) - 1) * (Gthau * Gepi)))+log(Q0au)), 'Gthau')
dYmGthau = eval(-(((FCdau[idouro] * (Aspau[idouro]/AspCdau[idouro]) - 1) * (Gth * Gepiau[idouro])) * ((FCd * (Asp/AspCd) - 1) * Gepi)/((FCd * (Asp/AspCd) - 1) * (Gthau[idouro] * Gepi))^2/(((FCdau[idouro] * (Aspau/AspCdau[idouro]) - 1) * (Gth * Gepiau[idouro]))/((FCd * (Asp/AspCd) - 1) * (Gthau[idouro] * Gepi)))))

dYmGepiauexp = D(expression(log(((FCdau * (Aspau / AspCdau) - 1) * (Gth * Gepiau)) / ((FCd * (Asp / AspCd) - 1) * (Gthau * Gepi)))+log(Q0au)), 'Gepiau')
dYmGepiau = eval((FCdau[idouro] * (Aspau[idouro]/AspCdau[idouro]) - 1) * Gth/((FCd * (Asp/AspCd) - 1) * (Gthau[idouro] * Gepi))/(((FCdau[idouro] * (Aspau[idouro]/AspCdau[idouro]) - 1) * (Gth * Gepiau[idouro]))/((FCd * (Asp/AspCd) - 1) * (Gthau[idouro] * Gepi))))


#Yinferior sem ouro
dYiAsp = eval(D(Yi, 'Asp'))
dYiAspCd = eval(D(Yi, 'AspCd'))
dYiFCd = eval(D(Yi, 'FCd'))
dYiGth = eval(D(Yi, 'Gth'))
dYiEfic = eval(D(Yi, 'Efic'))

#Yinferior com ouro
dYiAspauexp = D(expression(log(((Asp - AspCd / FCd) / (Aspau - AspCdau / FCdau)) * (Gthau / Gth * Eficau / Efic))), 'Aspau')
dYiAspau = eval( - ((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])^2 * (Gthau[idouro] / Gth *Eficau[idouro] / Efic) / (((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])) * (Gthau[idouro] /Gth * Eficau[idouro] / Efic))))

dYiAspCdauexp = D(expression(log(((Asp - AspCd / FCd) / (Aspau - AspCdau / FCdau)) * (Gthau / Gth * Eficau / Efic))), 'AspCdau')
dYiAspCdau = eval((Asp - AspCd / FCd) * (1 / FCdau[idouro]) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])^2 *(Gthau[idouro] / Gth * Eficau[idouro] / Efic) / (((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] /FCdau[idouro])) * (Gthau[idouro] / Gth * Eficau[idouro] / Efic)))

dYiFCdauexp = D(expression(log(((Asp - AspCd / FCd) / (Aspau - AspCdau / FCdau)) * (Gthau / Gth * Eficau / Efic))), 'FCdau')
dYiFCdau = eval( - ((Asp - AspCd / FCd) * (AspCdau[idouro] / FCdau[idouro]^2) / (Aspau[idouro] - AspCdau[idouro] /FCdau[idouro])^2 * (Gthau[idouro] / Gth * Eficau[idouro] / Efic) / (((Asp - AspCd / FCd) / (Aspau[idouro] -AspCdau[idouro] / FCdau[idouro])) * (Gthau[idouro] / Gth * Eficau[idouro] / Efic))))

dYiGthauexp = D(expression(log(((Asp - AspCd / FCd) / (Aspau - AspCdau / FCdau)) * (Gthau / Gth * Eficau / Efic))), 'Gthau')
dYiGthau = eval(((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])) * (1 / Gth * Eficau[idouro] / Efic) /(((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])) * (Gthau[idouro] / Gth * Eficau[idouro] / Efic)))

dYiEficauexp = D(expression(log(((Asp - AspCd / FCd) / (Aspau - AspCdau / FCdau)) * (Gthau / Gth * Eficau / Efic))), 'Eficau')
dYiEficau = eval(((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])) * (Gthau[idouro] / Gth / Efic) /(((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])) * (Gthau[idouro] / Gth * Eficau[idouro] / Efic)))
  
#Calculo da Variancia

varsup = (dYsAspCd * sAspCd)^2 + (dYsEfic * sEfic)^2 + (dYsFCd * sFCd)^2 + (dYsGepi * sGepi)^2

varmed = (dYmAsp * sAsp)^2 + (dYmAspCd * sAspCd)^2 + (dYmFCd * sFCd)^2 + (dYmGth * sGth)^2 + (dYmGepi * sGepi)^2 + 
         (dYmAspau * sAspau[idouro])^2 + (dYmAspCdau * sAspCdau[idouro])^2 + (dYmFCdau * sFCdau[idouro])^2 + 
         (dYmGthau * sGthau[idouro])^2 + (dYmGepiau * sGepiau[idouro])^2

varinf = (dYiAsp * sAsp)^2 + (dYiAspCd * sAspCd)^2 + (dYiFCd * sFCd)^2 + (dYiGth * sGth)^2 + (dYiEfic * sEfic)^2 +
         (dYiAspau * sAspau[idouro])^2 + (dYiAspCdau * sAspCdau[idouro])^2 + (dYiFCdau * sFCdau[idouro])^2 +
         (dYiGthau * sGthau[idouro])^2 + (dYiEficau * sEficau[idouro])^2

varY = c(varsup,varmed, varinf)
vY = diag(varY)

#covariancias entre YSuperior e YSuperior
for(i in 1:N) {
  for(j in i + 1:N) {
    if(isTRUE(Isotopo[i] == Isotopo[j]) & isTRUE(idouro[i] == idouro[j])) {
      vY[i,j] = dYsFCd[i] * dYsFCd[j] * sFCd[i]^2 + dYsGepi[i] * dYsGepi[j] * sGepi[i]^2
      vY[j,i] = vY[i,j]
    }
  }
}

#covariancias entre YSuperior e YMedio com Egama igual
for(i in 1:N) {
  for(j in (N + 1):(N * 2)) {
    if(isTRUE(Egama[i]==Egama[j - N]) & isTRUE(idouro[i]==idouro[j - N])) {
      k = j - N
      vY[i,j] = dYsAspCd[i] * dYmAspCd[k] * sAspCd[i]^2 + dYsFCd[i] * dYmFCd[k] * sFCd[i]^2 + dYsGepi[i] * dYmGepi[k] * sGepi[i]^2 
      vY[j,i] = vY[i,j]
    }
  }
}

#covariancias entre YSuperior e YMedio com Egama diferente
for(i in 1:N) {
  for(j in (N +1):(N * 2)) {
    if(isTRUE(Isotopo[i] == Isotopo[j - N]) & isFALSE(Egama[i] == Egama[j - N]) & isTRUE(idouro[i] == idouro[j - N])) {
      k = j - N
      vY[i,j] = dYsFCd[i] * dYmFCd[k] * sFCd[i]^2 + dYsGepi[i] * dYmGepi[k] * sGepi[i]^2
      vY[j,i] = vY[i,j]
    }
  }
}

#covariancias entre YSuperior e YInferior com Egama igual
for(i in 1:N) {
  for(j in (N * 2 + 1):(N * 3)) {
    if(isTRUE(Egama[i]==Egama[j - N * 2]) & isTRUE(idouro[i]==idouro[j - N* 2])) {
      k = j - N * 2
      vY[i,j] = dYsAspCd[i] * dYiAspCd[k] * sAspCd[i]^2 + dYsFCd[i] * dYiFCd[k] * sFCd[i]^2 + dYsEfic[i] * dYiEfic[k] * sEfic[i]^2 
      vY[j,i] = vY[i,j]
    }
  }
}

#covariancias entre YSuperior e YInferior com Egama diferente
for(i in 1:N) {
  for(j in (N * 2 + 1):(N * 3)) {
    if(isTRUE(Isotopo[i] == Isotopo[j - N * 2]) & isFALSE(Egama[i] == Egama[j - N * 2]) & isTRUE(idouro[i] == idouro[j - N * 2])) {
      k = j - N * 2
      vY[i,j] = dYsFCd[i] * dYiFCd[k] * sFCd[i]^2
      vY[j,i] = vY[i,j]
    }
  }
}


#covariancias entre YMedio e YMedio com Isotopo Igual
for(i in (N + 1):(N * 2)) {
  for(j in (i + 1):(N * 2)) {
    if(isTRUE(Isotopo[i - N] == Isotopo[j - N]) & isFALSE(Egama[i - N] == Egama[j - N]) & isTRUE(idouro[i - N] == idouro[j - N])) {
      k = i - N
      l = j - N
      vY[i,j] = dYmAsp[k] * dYmAsp[l] * sAsp[k]^2 + dYmAspCd[k] * dYmAspCd[l] * sAspCd[k]^2 + dYmFCd[k] * dYmFCd[l] * sFCd[k]^2 + dYmGth[k] *
                dYmGth[l] * sGth[k]^2 + dYmGepi[k] * dYmGepi[l] * sGepi[k]^2 + dYmAspau[idouro[k]] * dYmAspau[idouro[l]] * sAspau[idouro[k]]^2 +
                dYmAspCdau[idouro[k]] * dYmAspCdau[idouro[l]] * sAspCdau[idouro[k]]^2 + dYmFCdau[idouro[k]] * dYmFCdau[idouro[l]] * sFCdau[idouro[k]]^2 +
                dYmGthau[idouro[k]] * dYmGthau[idouro[l]] * sGthau[idouro[k]]^2 + dYmGepiau[idouro[k]] * dYmGepiau[idouro[l]] * sGepiau[idouro[k]]^2
      vY[j,i] = vY[i,j]
    }
  }
}

#covariancias entre YMedio e YMedio com Isotopo diferente
for(i in (N + 1):(N * 2)) {
  for(j in (i + 1):(N * 2)) {
    if(isTRUE(Isotopo[i - N] != Isotopo[j - N]) & isTRUE(idouro[i - N] == idouro[j - N])) {
      k = i - N
      l = j - N
      vY[i,j] = dYmAspau[idouro[k]] * dYmAspau[idouro[l]] * sAspau[idouro[k]]^2 + dYmAspCdau[idouro[k]] * 
        dYmAspCdau[idouro[l]] * sAspCdau[idouro[k]]^2 + dYmFCdau[idouro[k]] * dYmFCdau[idouro[l]] * sFCdau[idouro[k]]^2 +
        dYmGthau[idouro[k]] * dYmGthau[idouro[l]] * sGthau[idouro[k]]^2 + dYmGepiau[idouro[k]] * dYmGepiau[idouro[l]] * 
        sGepiau[idouro[k]]^2
      vY[j,i] = vY[i,j]
    }
  }
}

#covariancias entre YMedio e YInferior com Isotopo Igual
for(i in (N + 1):(N * 2)) {
  for(j in (N * 2):(N * 3)) {
    if(isTRUE(Isotopo[i - N] == Isotopo[j - N * 2]) & isFALSE(Egama[i - N] == Egama[j - N * 2]) & isTRUE(idouro[i - N] == idouro[j - N * 2])) {
      k = i - N
      l = j - N * 2
      vY[i,j] = dYmAsp[k] * dYiAsp[l] * sAsp[k]^2 + dYmAspCd[k] * dYiAspCd[l] * sAspCd[k]^2 + dYmFCd[k] * dYiFCd[l] * sFCd[k]^2 + dYmGth[k] *
        dYiGth[l] * sGth[k]^2 + dYmAspau[idouro[k]] * dYiAspau[idouro[l]] * sAspau[idouro[k]]^2 +
        dYmAspCdau[idouro[k]] * dYiAspCdau[idouro[l]] * sAspCdau[idouro[k]]^2 + dYmFCdau[idouro[k]] * dYiFCdau[idouro[l]] * sFCdau[idouro[k]]^2 +
        dYmGthau[idouro[k]] * dYiGthau[idouro[l]] * sGthau[idouro[k]]^2
      vY[j,i] = vY[i,j]
    }
  }
}

#covariancias entre YMedio e YInferior com Isotopo Diferente
for(i in (N + 1):(N * 2)) {
  for(j in (N * 2):(N * 3)) {
    if(isTRUE(Isotopo[i - N] != Isotopo[j - N * 2]) & isTRUE(idouro[i - N] == idouro[j - N * 2])) {
      k = i - N
      l = j - N * 2
      vY[i,j] = dYmAspau[idouro[k]] * dYiAspau[idouro[l]] * sAspau[idouro[k]]^2 +
        dYmAspCdau[idouro[k]] * dYiAspCdau[idouro[l]] * sAspCdau[idouro[k]]^2 + dYmFCdau[idouro[k]] * dYiFCdau[idouro[l]] * sFCdau[idouro[k]]^2 +
        dYmGthau[idouro[k]] * dYiGthau[idouro[l]] * sGthau[idouro[k]]^2
      vY[j,i] = vY[i,j]
    }
  }
}


#covariancias entre YInferior e YInferior com Isotopo Igual
for(i in (N * 2 + 1):(N * 3)) {
  for(j in (i + 1):(N * 3)) {
    if(isTRUE(Isotopo[i - N * 2] == Isotopo[j - N * 2]) & isFALSE(Egama[i - N * 2] == Egama[j - N * 2]) & isTRUE(idouro[i - N * 2]==idouro[j - N * 2])) {
      k = i - N * 2
      l = j - N * 2
      vY[i,j] = dYiAspCdau[idouro[k]] * dYiAspCdau[idouro[l]] * sAspCdau[idouro[k]]^2 + dYiAspau[idouro[k]] * dYiAspau[idouro[l]] *
                sAspau[idouro[k]]^2 + dYiFCdau[idouro[k]] * dYiFCdau[idouro[l]] * sFCdau[idouro[k]]^2 + dYiGthau[idouro[k]] *
                dYiGthau[idouro[l]] * sGthau[idouro[k]]^2 + dYiEficau[idouro[k]] * dYiEficau[idouro[l]] * sEficau[idouro[k]]^2 +
                dYiGth[k] * dYiGth[l] * sGth[k]^2 + dYiFCd[k] * dYiFCd[l] * sFCd[k]^2
      vY[j,i] = vY[i,j]
    }
  }
}

#covariancias entre YInferior e YInferior com Isotopo diferente
for(i in (N * 2 + 1):(N * 3)) {
  for(j in (i + 1):(N * 3)) {
    if(isTRUE(Isotopo[i - N * 2] != Isotopo[j - N * 2]) & isTRUE(idouro[i - N * 2] == idouro[j - N * 2])) {
      k = i - N * 2
      l = j - N * 2
      vY[i,j] = dYiAspCdau[idouro[k]] * dYiAspCdau[idouro[l]] * sAspCdau[idouro[k]]^2 + dYiAspau[idouro[k]] * dYiAspau[idouro[l]] * 
                sAspau[idouro[k]]^2 + dYiFCdau[idouro[k]] * dYiFCdau[idouro[l]] * sFCdau[idouro[k]]^2 + dYiGthau[idouro[k]] * 
                dYiGthau[idouro[l]] * sGthau[idouro[k]]^2 + dYiEficau[idouro[k]] * dYiEficau[idouro[l]] * sEficau[idouro[k]]^2
      vY[j,i] = vY[i,j]
    }
  }
}

#construindo a matrix de planejamento
#Definindo as variaveis
Aa = runif(1, min = 22, max = 25)
Aalfa = runif(1, min = 0.001, max = 0.005)
Alfa1 = 0.002
Ab = 25.5

 AQ0 = 0
 for (i in 1 : N) {
   AQ0[i] = log(runif(1, min = 1, max = 6))
 }
 
 Ak0 = 0
 for (i in 1 : N) {
   Ak0[i] = log(runif(1, min = 1, max = 5))
 }
 
#AQ0 = log(Q0)

Ak02 = k0
 
A2 = matrix(c(Aa, Aalfa, AQ0, Ak0))

#colsexp = expression(Aa + 2 * Aalfa * log(Eres) + log((((Q0 - 0.429) / Eres^Aalfa) + (0.429 / ((2 * Aalfa + 1) * 0.55^Aalfa)))) + log(k0))
#cols = D(expression(Aa + 2 * Aalfa * log(Eres) * log((((exp(AQ0) - 0.429) / Eres^Aalfa) + (0.429 / ((2 * Aalfa + 1) * 0.55^Aalfa)))) + log(exp(Ak0))), 'Aa')  
colsexp = expression(Ab + 2 * Alfa1 * log(Eres) + Alfa1 * log(Q0) + log(Ak02))
colmexp = expression((Alfa1 * log(Q0))/(Alfa1 * log(Q0au)))

cols = rep(1, N)
colm = rep(0, N)
coli = rep(0, N)
col1 = c(cols, colm, coli)

#cols2 = eval(D(expression(exp(Aa) * Eres^(2* Aalfa) * exp(Ak0) * (((exp(AQ0) - 0.429) / Eres^Aalfa) + (0.429 / ((2 * Aalfa + 1) * 0.55^Aalfa)))), 'Aalfa'))  
cols2 = eval(D(colsexp, 'Alfa1'))
colm2 = eval(D(colmexp, 'Alfa1'))
#cols2b = 2 * log(Eres)
#cols2b = 2 * log(Eres) - (0.429 * (2 * 0.55^Alfa1 + (2 * Alfa1 + 1) * (0.55^Alfa1 * log(0.55)))/((2 * Alfa1 + 1) * 0.55^Alfa1)^2 + (Q0 - 0.429) * (Eres^Alfa1 * log(Eres))/(Eres^Alfa1)^2)/(((Q0 - 0.429)/Eres^Alfa1) + (0.429/((2 * Alfa1 + 1) * 0.55^Alfa1)))
#colm2exp = expression((((Q0 - 0.429) / Eres^Aalfa) + (0.429 / ((2 * Aalfa + 1) * 0.55^Aalfa))) / (((Q0au - 0.429) / Eresau^Aalfa) + (0.429 / ((2 * Aalfa + 1) * 0.55^Aalfa))))
#colm2 = rep(0, N) #eval(D(colm2exp, 'Aalfa'))
#colm2b = -((0.429 * (2 * 0.55^Aalfa + (2 * Aalfa + 1) * (0.55^Aalfa * log(0.55)))/((2 * Aalfa + 1) * 0.55^Aalfa)^2 + (Q0 - 0.429) * (Eres^Aalfa * log(Eres))/(Eres^Aalfa)^2)/(((Q0au[idouro] - 0.429)/Eresau[idouro]^Aalfa) + (0.429/((2 * Aalfa + 1) * 0.55^Aalfa))) - (((Q0 - 0.429)/Eres^Aalfa) + (0.429/((2 * Aalfa + 1) * 0.55^Aalfa))) * (0.429 * (2 * 0.55^Aalfa + (2 * Aalfa + 1) * (0.55^Aalfa * log(0.55)))/((2 * Aalfa + 1) * 0.55^Aalfa)^2 + (Q0au[idouro] - 0.429) * (Eresau[idouro]^Aalfa * log(Eresau[idouro]))/(Eresau[idouro]^Aalfa)^2)/(((Q0au[idouro] - 0.429)/Eresau[idouro]^Aalfa) + (0.429/((2 * Aalfa + 1) * 0.55^Aalfa)))^2)
coli2 = coli = rep(0, N)
col2 = c(cols2, colm2, coli2)

cols3 = eval(D(colsexp, 'Q0'))
cols3b = 1/Eres^Alfa1/(((Q0 - 0.429)/Eres^Alfa1) + (0.429/((2 * Alfa1 + 1) * 0.55^Alfa1)))
colm3 = eval(D(colmexp, 'Q0'))
colm3b = 1/Eres^Aalfa/(((Q0au[idouro] - 0.429)/Eresau[idouro]^Aalfa) + (0.429/((2 * Aalfa + 1) * 0.55^Aalfa)))

# cols4 = eval(D(colsexp, 'Ak0'))
# cols4b = rep(1, N) #/exp(Ak0)

Ntotal = N * 3
XQ0 = matrix(c(0),nrow=Ntotal, ncol = N)

for(i in 1:N) {
  XQ0[i,i] = cols3[i]
  XQ0[(N + i),i] = colm3[i]
}

Xk0 = matrix(c(0),nrow=Ntotal, ncol = N)

for(i in 1:N) {
  Xk0[i,i] = 1
  Xk0[(N * 2 + i),i] = 1
}
  
X = cbind(col1, col2, XQ0, Xk0)

#come�ando o levenberg
#Vetor A com as variaveis


Yexp = matrix(Y)
Yajusup = 0
Yajumed = 0
Yajuinf = 0

R = t(X) %*% inv(vY) %*% X

#valores de lambda e parametros para o loop na hora de fazer a analise
chidif = 1
lambda = 0.00001
X2 = 0
chi2 = -1
recalcularY = 1
chi2novo = 0

while(1) {
  if(recalcularY == 1) {
    a2 = A2[1]
    alfa2 = A2[2]
    for (i in 1 : N) {
      Q0b = A2[i + 2]
      k0b = A2[(i + N + 2)]
      #Yajusup[i] = a2 + 2 * alfa2 * log(Eres[i]) + log((((exp(Q0b) - 0.429) / Eres[i]^alfa2) + (0.429 / ((2 * alfa2 + 1) * 0.55^alfa2)))) + log(exp(k0b))
      Yajusup[i] = a2 + 2 * alfa2 * log(Eres[i]) + alfa2 * log(exp(Q0b)) + log(exp(k0b))
      Yajumed[i] = (alfa2 * log(exp(Q0b)))/(alfa2 * log(Q0au[idouro[i]]))
      #Yajumed[i] = (((exp(Q0b) - 0.429) / Eres[i]^alfa2) + (0.429 / ((2 * alfa2 + 1) * 0.55^alfa2))) / (((Q0au[idouro[i]] - 0.429) / Eresau[idouro[i]]^alfa2) + (0.429 / ((2 * alfa2 + 1) * 0.55^alfa2)))
      Yajuinf[i] = log(exp(k0b))
    }
    Yaju2 = matrix(c(Yajusup, Yajumed, Yajuinf))
    datb = Yexp - Yaju2
    Yy = Yexp - Yaju2
    ss = cbind(Yexp, Yaju2)
    if (chi2 == -1) {
    chi2 = t(datb) %*% inv(vY) %*% datb
    }
  }
  Rlambda = matrix(0, nrow = N*2+2, ncol = N*2+2)
  for (i in 1: (N*2+2)) {
    for (j in 1: (N*2+2)) {
      if (isTRUE(i == j)) {
        Rlambda[i,j] = (1+lambda) * R[i,j]
      } else {
          Rlambda[i,j] = R[i,j]
        }
    }
  }
  
  DA = Ginv(Rlambda) %*% t(X) %*% inv(vY) %*% Yy
  Ynovo = X %*% DA
  DD = Yy - Ynovo
  chi2novo = t(DD) %*% inv(vY) %*% DD
  chidif = chi2novo - chi2
  if (chidif >= 0) {
    lambda = lambda * 10000
    recalcularY = 0
  } else {
      #lambda = lambda / 10
      Anovo = A2 + DA
      A2 = Anovo
      chi2 = chi2novo
      recalcularY = 1
  }
  if (isTRUE(abs(chidif) < 1.0) & (isTRUE(abs(chidif) > 0.001))) {
    break
  }
}

Afinal = A2

for (i in 1 : N*2) {
  Afinal[i+2] = exp(Afinal[i+2])
}

sAfinal = sqrt(diag(inv(Rlambda)))
sAfinal2 = Afinal * sqrt(diag(inv(Rlambda)))
sAfinal2 = matrix(sAfinal2[c(-1,-2)])
quirednovo = chi2novo/gl
probchi2 = (1-pchisq(chi2novo,gl))*100

print("saiiiiiiiiiiiii")
sY = sqrt(diag(vY))

#gerando gr�ficos
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

plot(xplot, (DD/sY), main = 'RESIDUOS PONDERADOS PELO DESVIO PADR�O', ylab = 'Y-Yaju-sigmaY', xlab = 'elementos', ylim = c(-7, 7))
text(xplot, (DD/sY), labels = pointname, cex = 0.7, pos = 3)
text(xplot, (DD/sY), labels = pointname2, cex = 0.7, pos = 1)

# sink('Resultados.txt', append = FALSE, split = FALSE)
# 'VALORES DOS PARAMETROS'
# '----------------------------------------------------------------'
# print(c('a    = ', round(Afinal[1], 5), round(sAfinal[1], 5)))
# print(c('Alfa =  ', round(Afinal[2], 5), round(sAfinal[2], 5)))
# print(c('QUI-QUADRADO', round(chi2novo, 2)))
# print(c('QUI-QUADRADO REDUZIDO', round(quirednovo, 2)))
# '----------------------------------------------------------------'
# 'VALORES DOS K0S'
# k0res = 0
# for (i in 1 : N) {
#   k0res[i] = round(Afinal[i+2], 11)
# }
# 
# k0res = format(matrix(k0res, ncol=1), scientific = TRUE)
# colnames(k0res) = '     k0'
# Isotopo = matrix(Isotopo, ncol=1)
# colnames(Isotopo) = 'Isotopo'
# Egama = matrix(Egama, ncol=1)
# colnames(Egama) = 'E. gamma'
# tabela = format(matrix(k0, ncol=1), scientific = TRUE)
# colnames(tabela) = 'Referencia'
# #sk0res = sAfinal2
# sk0res = round(sAfinal2, 11)
# sk0res = format(matrix(sk0res, ncol=1), scientific = TRUE)
# colnames(sk0res) = '  Incerteza'
# 
# cbind(Isotopo, Egama, k0res, sk0res, tabela)
# sink()


#invf = Gth / ((FCd * (Asp / AspCd) - 1) * Q0 * Gepi)
#f = 1 / invf
#(exp(b) / Efic^ - alfa) * (Efic^alfa / Efic^ - alfa)

aaa=diag(vY)
aaaa=sqrt(aaa)
aas=aaaa[c(1, 2, 3, 4, 5, 6, 7,8, 9, 10, 11, 12)]
a3 = a2
alfa3 = alfa2
k01 = runif(1, min = 1.001, max = 5)
k02 = runif(1, min = 1.001, max = 5)
k03 = runif(1, min = 1.001, max = 5)
k04 = runif(1, min = 1.001, max = 5)
k05 = runif(1, min = 1.001, max = 5)
k06 = runif(1, min = 1.001, max = 5)
k07 = runif(1, min = 1.001, max = 5)
k08 = runif(1, min = 1.001, max = 5)
k09 = runif(1, min = 1.001, max = 5)
k010 = runif(1, min = 1.001, max = 5)
k011 = runif(1, min = 1.001, max = 5)
k012 = runif(1, min = 1.001, max = 5)
Ysuperior
Yajusup2 = a3 + alfa3/Q0 * ((Q0 + 0.429) * log(Eres - 0.602)) + log(exp(Ak02))
teste = function(a3, alfa3, k0) a3 + alfa3/Q0 * ((Q0 + 0.429) * log(Eres - 0.602)) + log(exp(k0))
resid = Ysuperior - Yajusup2
pp = list(a3 = a2, alfa3 = alfa2, k01, k02, k03, k04, k05, k06, k07, k08, k09, k010, k011, k012)
simDNoisy <- teste(pp,Ysuperior) + rnorm(length(Ysuperior),sd=.1)
nls.lm(pp, resid, Yaju2, Ysuperior)

