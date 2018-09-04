#AKFIT R Edition
#Rafel Vanhoz Ribeiro

#INICIO
#escolhendo a biblioteca.

library(tidyverse)  #Tidtverse é uma coletânea de biblioteca para análise de dados
library(minpack.lm) #minpack é uma biblioteca para realização do levenberg - marquardt
library(matlib) #matlib é uma biblioteca que realiza calculos matriciais e inversões de matrizes

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
Ys = expression(log(AspCd / Q0 / Efic / FCd / Gepi))
Ysuperior = eval(Ys)

#Calcula o Y Inferior da matrix Y
Yi = expression(log(((Asp - AspCd / FCd) / (Aspau - AspCdau / FCdau)) * (Gthau / Gth * Eficau / Efic)))
Yinferior = 0
for(i in 1:N){
  Yinferior[i] = log(((Asp[i] - AspCd[i] / FCd[i]) / (Aspau[idouro[i]] - AspCdau[idouro[i]] / FCdau[idouro[i]])) * (Gthau[idouro[i]] / Gth[i] * Eficau[idouro[i]] / Efic[i]))
}
Y = c(Ysuperior,Yinferior)

#derivadas parciais de Ysuperior e Yinferior

#Ysuperior
dYsAspCd = eval(D(Ys,'AspCd'))
dYsQ0 = eval(D(Ys,'Q0'))
dYsEfic = eval(D(Ys,'Efic'))
dYsFCd = eval(D(Ys,'FCd'))
dYsGepi = eval(D(Ys,'Gepi'))

#Yinferior sem ouro
dYiAsp = eval(D(Yi, 'Asp'))
dYiAspCd = eval(D(Yi, 'AspCd'))
dYiFCd = eval(D(Yi, 'FCd'))
dYiGth = eval(D(Yi, 'Gth'))
dYiEfic = eval(D(Yi, 'Efic'))

#Yinferior com ouro
dYiAspauexp = D(expression(log(((Asp - AspCd / FCd) / (Aspau - AspCdau / FCdau)) * (Gthau / Gth * Eficau / Efic))), 'Aspau')
dYiAspau = eval( - ((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])^2 * (Gthau[idouro] / Gth *
               Eficau[idouro] / Efic) / (((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])) * (Gthau[idouro] /
               Gth * Eficau[idouro] / Efic))))
    
dYiAspCdauexp = D(expression(log(((Asp - AspCd / FCd) / (Aspau - AspCdau / FCdau)) * (Gthau / Gth * Eficau / Efic))), 'AspCdau')
dYiAspCdau = eval((Asp - AspCd / FCd) * (1 / FCdau[idouro]) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])^2 *
             (Gthau[idouro] / Gth * Eficau[idouro] / Efic) / (((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] /
             FCdau[idouro])) * (Gthau[idouro] / Gth * Eficau[idouro] / Efic)))
    
dYiFCdauexp = D(expression(log(((Asp - AspCd / FCd) / (Aspau - AspCdau / FCdau)) * (Gthau / Gth * Eficau / Efic))), 'FCdau')
dYiFCdau = eval( - ((Asp - AspCd / FCd) * (AspCdau[idouro] / FCdau[idouro]^2) / (Aspau[idouro] - AspCdau[idouro] /
           FCdau[idouro])^2 * (Gthau[idouro] / Gth * Eficau[idouro] / Efic) / (((Asp - AspCd / FCd) / (Aspau[idouro] -
           AspCdau[idouro] / FCdau[idouro])) * (Gthau[idouro] / Gth * Eficau[idouro] / Efic))))
    
dYiGthauexp = D(expression(log(((Asp - AspCd / FCd) / (Aspau - AspCdau / FCdau)) * (Gthau / Gth * Eficau / Efic))), 'Gthau')
dYiGthau = eval(((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])) * (1 / Gth * Eficau[idouro] / Efic) /
           (((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])) * (Gthau[idouro] / Gth * Eficau[idouro] / Efic)))

dYiEficauexp = D(expression(log(((Asp - AspCd / FCd) / (Aspau - AspCdau / FCdau)) * (Gthau / Gth * Eficau / Efic))), 'Eficau')
dYiEficau = eval(((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])) * (Gthau[idouro] / Gth / Efic) /
            (((Asp - AspCd / FCd) / (Aspau[idouro] - AspCdau[idouro] / FCdau[idouro])) * (Gthau[idouro] / Gth * Eficau[idouro] / Efic)))
  
#Calculo da Variancia

varsup = (dYsAspCd * sAspCd)^2 + (dYsQ0 * sQ0)^2 + (dYsEfic * sEfic)^2 + (dYsFCd * sFCd)^2 + (dYsGepi * sGepi)^2

varinf = (dYiAsp * sAsp)^2 + (dYiAspCd * sAspCd)^2 + (dYiFCd * sFCd)^2 + (dYiGth * sGth)^2 + (dYiEfic * sEfic)^2 +
         (dYiAspau * sAspau[idouro])^2 + (dYiAspCdau * sAspCdau[idouro])^2 + (dYiFCdau * sFCdau[idouro])^2 +
         (dYiGthau * sGthau[idouro])^2 + (dYiEficau * sEficau[idouro])^2

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

#construindo a matrix de planejamento
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

#qui quadrado
qui2 = t(dif) %*% inv(vY) %*% dif

#qui quadrado reduzido
gl = Ntotal - numpar
qui2red = qui2 / gl
porbqui = (1-pchisq(qui2,gl))*100

#criando os valores de cada par�metro separadamente
a = A[1]
sa = sqrt(vpar[1,1])
alfa = A[2]
salfa = sqrt(vpar[2,2])
k0aju = A[c( - 1, - 2)]
sk0aju = k0aju * sqrt(diag(vpar)[c( - 1, - 2)])

#Correla��o entre a e alfa
coralfa=vpar[1,2]/(sa*salfa)

#come�ando o levenberg

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
    recalcularY = 0
  } else {
      lambda = lambda / 10
      Anovo = A2 + DA
      A2 = Anovo
      chi2 = chi2novo
      recalcularY = 1
  }
  if (isTRUE(abs(chidif) < 1.0) & (isTRUE(abs(chidif) > 0.0001))) {
    break
  }
}

Afinal = A2

for (i in 1 : N) {
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

sink('Resultados.txt', append = FALSE, split = FALSE)
'VALORES DOS PARAMETROS'
'----------------------------------------------------------------'
print(c('a    = ', round(Afinal[1], 5), round(sAfinal[1], 5)))
print(c('Alfa =  ', round(Afinal[2], 5), round(sAfinal[2], 5)))
print(c('QUI-QUADRADO', round(chi2novo, 2)))
print(c('QUI-QUADRADO REDUZIDO', round(quirednovo, 2)))
'----------------------------------------------------------------'
'VALORES DOS K0S'
k0res = 0
for (i in 1 : N) {
  k0res[i] = round(Afinal[i+2], 11)
}

k0res = format(matrix(k0res, ncol=1), scientific = TRUE)
colnames(k0res) = '     k0'
Isotopo = matrix(Isotopo, ncol=1)
colnames(Isotopo) = 'Isotopo'
Egama = matrix(Egama, ncol=1)
colnames(Egama) = 'E. gamma'
tabela = format(matrix(k0, ncol=1), scientific = TRUE)
colnames(tabela) = 'Referencia'
#sk0res = sAfinal2
sk0res = round(sAfinal2, 11)
sk0res = format(matrix(sk0res, ncol=1), scientific = TRUE)
colnames(sk0res) = '  Incerteza'

cbind(Isotopo, Egama, k0res, sk0res, tabela)
sink()


#invf = Gth / ((FCd * (Asp / AspCd) - 1) * Q0 * Gepi)
#f = 1 / invf
#(exp(b) / Efic^ - alfa) * (Efic^alfa / Efic^ - alfa)
