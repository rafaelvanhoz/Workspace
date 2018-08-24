library("tidyverse")

data=readLines(file.choose())
data=data[-1]
data=data[-1]
data=data.frame(data)

data=separate(
  data, "data", c(
  'Isotopo', 
  'Egama', 
  'IN', 
  'Efic', 
  'sigma', 
  'Eres', 
  'sig%', 
  'Asp_Cd', 
  'sigma', 
  'Asp', 
  'sigma', 
  'Rcd', 
  'sigma', 
  'k0', 
  'sig%', 
  'Fcd', 
  'sig', 
  'Gepi', 
  'sigma', 
  'Gth', 
  'sigma', 
  'I0', 
  'sig%', 
  'sigma0', 
  'sig%', 
  'Q0', 
  'sigma', 
  'idouro'),
  sep = '\\s{1,}'
  )

dataau=readLines(file.choose())
dataau=dataau[-1]
dataau=dataau[-1]
dataau=data.frame(dataau)

dataau=separate(
  dataau, "dataau", c(
    'Isotopo', 
    'Egama', 
    'IN', 
    'Efic', 
    'sigma', 
    'Eres', 
    'sig%', 
    'Asp_Cd', 
    'sigma', 
    'Asp', 
    'sigma', 
    'Rcd', 
    'sigma', 
    'k0', 
    'sig%', 
    'Fcd', 
    'sig', 
    'Gepi', 
    'sigma', 
    'Gth', 
    'sigma', 
    'I0', 
    'sig%', 
    'sigma0', 
    'sig%', 
    'Q0', 
    'sigma'),
  sep = '\\s{1,}'
)
