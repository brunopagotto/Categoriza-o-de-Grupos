# -*- coding: utf-8 -*-
"""
Created on Tue May 30 00:05:49 2023

@author: Usuario
"""

import sys
print(sys.version)

pip install pyufbr
pip install openpyxl

from pyUFbr.baseuf import ufbr
import pandas as pd

print (ufbr.list_uf)
print(ufbr.list_cidades('AC'))

df = pd.DataFrame(columns=['Cidade', 'Estado'])
linha = 0

for i in range(len(ufbr.list_uf)):
    estado = ufbr.list_uf[i]
    for j in range(len(ufbr.list_cidades(estado))):
                   cidade = ufbr.list_cidades(estado)[j]
                   df.at[linha, 'Cidade'] = cidade
                   df.at[linha, 'Estado'] = estado
                   linha = linha + 1
    
# Especifique o caminho do arquivo Excel
caminho_arquivo_excel = 'municipios_brasil.xlsx'

# Exporte o DataFrame para um arquivo Excel
df.to_excel(caminho_arquivo_excel, index=False)

# Especifique o caminho do arquivo CSV
caminho_arquivo_csv = 'municipios_brasil.csv'

# Exporte o DataFrame para um arquivo CSV
df.to_csv(caminho_arquivo_csv, index=False)

# Verificar versão da biblioteca
pip show pyUFbr
