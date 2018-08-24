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

# data2=readLines(file.choose())
# data2=data2[-1]
# data2=data2[-1]
# data2=data.frame(data2)
# 
# data2=separate(
#   data2, "data2", c(
#     'Isotopo',
#     'N', 
#     'sN',
#     'NCd',
#     'sNCd',
#     'fz',
#     'sfz',
#     'fzCd',
#     'sfzCd',
#     'fa',
#     'sfa',
#     'faCd',
#     'sfaCd',
#     'D',
#     'sD',
#     'DCd',
#     'sDCd',
#     'C',
#     'Sc',
#     'CCd',
#     'sCCd',
#     'S',
#     'sS',
#     'SCd',
#     'sSCd',
#     'w',
#     'sw',
#     'wCd',
#     'swCd',
#     'Egama', 
#     'IN', 
#     'Efic', 
#     'sEfic', 
#     'Eres', 
#     'sEres', 
#     'k0', 
#     'sk0', 
#     'Fcd', 
#     'sFCd', 
#     'Gepi', 
#     'sGepi', 
#     'Gth', 
#     'sGth', 
#     'I0', 
#     'sig%', 
#     'sigma0', 
#     'sig%', 
#     'Q0', 
#     'sigma',
#     'idouro'),
#   sep = '\\s{1,}'
# )
# 
# # data2au=readLines(file.choose())
# # data2au=data2au[-1]
# # data2au=data2au[-1]
# # data2au=data.frame(data2au)
# 
# data2au=separate(
#   data2au, "data2au", c(
#     'Isotopo',
#     'N', 
#     'sN',
#     'NCd',
#     'sNCd',
#     'fz',
#     'sfz',
#     'fzCd',
#     'sfzCd',
#     'fa',
#     'sfa',
#     'faCd',
#     'sfaCd',
#     'D',
#     'sD',
#     'DCd',
#     'sDCd',
#     'C',
#     'Sc',
#     'CCd',
#     'sCCd',
#     'S',
#     'sS',
#     'SCd',
#     'sSCd',
#     'w',
#     'sw',
#     'wCd',
#     'swCd',
#     'Egama', 
#     'IN', 
#     'Efic', 
#     'sEfic', 
#     'Eres', 
#     'sEres', 
#     'k0', 
#     'sk0', 
#     'Fcd', 
#     'sFCd', 
#     'Gepi', 
#     'sGepi', 
#     'Gth', 
#     'sGth', 
#     'I0', 
#     'sig%', 
#     'sigma0', 
#     'sig%', 
#     'Q0', 
#     'sigma'),
#   sep = '\\s{1,}'
# )

