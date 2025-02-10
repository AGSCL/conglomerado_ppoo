# -*- coding: utf-8 -*-
"""
Created on Thu Apr  4 13:18:49 2024

@author: andre
"""


import gc
gc.collect()
import sys
sys.modules[__name__].__dict__.clear()

# 0. paquetes y setting ---------------------------------------------------

import os
os.system('pip install pandas')
os.system('pip install pyjanitor')
os.system('pip install pandas_flavor')
os.system('pip install pyarrow')

import numpy as np
import pandas as pd
from janitor import clean_names, remove_empty
import pyarrow.parquet as pq

# Obtener base de datos
dtX2023_12_05_DatosEgresosHosp_encrip = pq.read_table("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/10_Egresos Hospitalarios/20231205_hosp.parquet.gzip").to_pandas()

# 1. seleccionar personas que hayan indicado alguna vez pertenencia a PPOO-------


#función de glimpse
def glimpse(df):
    print(f"Rows: {df.shape[0]}")
    print(f"Columns: {df.shape[1]}")
    for col in df.columns:
        print(f"$ {col} <{df[col].dtype}> {df[col].head().values}")

glimpse(dtX2023_12_05_DatosEgresosHosp_encrip)

#agrupar

result_df = dtX2023_12_05_DatosEgresosHosp_encrip.groupby('run').agg(
    n_ppoo=('rsh', lambda x: (x == 1).sum(skipna=True)),
    n_ppoo_minsal=('pueblo_originario_01', lambda x: (x == 1).sum(skipna=True))
)

print(result_df)


result_df = dtX2023_12_05_DatosEgresosHosp_encrip.groupby('run').agg(
    n_ppoo=('rsh', lambda x: x.eq(1).sum(skipna=True)),
    n_ppoo_minsal=('pueblo_originario_01', lambda x: x.eq(1).sum(skipna=True))
)

print(result_df)

filtered_df = result_df[(result_df['n_ppoo'] > 0) | (result_df['n_ppoo_minsal'] > 0)]

print(filtered_df)
#181355, a 1687189

#run is indexed now
unique_runs = filtered_df.index.unique()  

print(unique_runs)

unique_runs_df = pd.DataFrame(unique_runs, columns=['run'])
unique_runs_df.to_csv('H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/filtered_df.csv.gz', compression='gzip') 
