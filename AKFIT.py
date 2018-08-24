# -*- coding: utf-8 -*-
"""
Spyder Editor

AKFIT V2.1
CRIADO POR RAFAEL VANHOZ RIBEIRO


entrada dos aquivos de dados
"""
import re
import numpy as np
        
entrada = open('2010.dat', 'r')
comentario = entrada.readline()
cabecalho = entrada.readline()
linha = entrada.readlines()

linhas = []
for x in linha:
    linhas.append(re.split(r" +", x))

g = np.array(linhas)
entradaAU = open('au.dat', 'r')
comentarioAU = entrada.readline()
cabecalhoAU = entrada.readline()

entrada.close()
entradaAU.close()

