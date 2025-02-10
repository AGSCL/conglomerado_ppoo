# -*- coding: utf-8 -*-
"""
Created on Wed Dec  6 11:25:34 2023

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

# 1. parse dates ---------------------------------------------------

from dateutil import parser

# Assuming dtX2023_12_05_DatosEgresosHosp_encrip is your DataFrame and
# fecha_ingreso and fecha_egreso are the columns to be parsed.

import pandas as pd
import numpy as np
from dateutil import parser

# if parsing fails (resulting in NaT), the function flags those attempts. 
# After trying all specified formats, it makes a final attempt to parse any 
# remaining unparsed dates using dateutil.parser.parse without a specific format, 
# which can handle a wide variety of date representations but might be slower 
# for large numbers of dates due to its flexibility.

def try_parse_date(date_str, fmt=None):
    """Attempt to parse a single date string. Returns None if parsing fails."""
    try:
        if fmt:
            return pd.to_datetime(date_str, format=fmt, errors='raise')
        else:
            return parser.parse(date_str, dayfirst=False, yearfirst=False)
    except (ValueError, TypeError):
        return None

def parse_dates_with_fallback_and_flag(date_series, formats):
    """Parse a series of dates with fallback formats and flag parsing errors."""
    # Initialize a result series with NaT and a flag series with False
    result_series = pd.Series(pd.NaT, index=date_series.index)
    parse_error_flag = pd.Series(False, index=date_series.index)
    
    # Vectorized attempt with predefined formats
    for fmt in formats:
        not_parsed = result_series.isna()  # Only attempt to parse dates that have not been parsed yet
        # Apply parsing attempt for each format
        temp_parsed = date_series[not_parsed].apply(try_parse_date, fmt=fmt)
        # Update the result series with successfully parsed dates
        result_series.update(temp_parsed.dropna())
        # Flag any new NaT values as errors (dates that could not be parsed with this format)
        parse_error_flag |= temp_parsed.isna() & not_parsed  # Only flag those that were attempted this round
    
    # Final attempt without specifying format (more flexible parsing with dateutil.parser)
    not_parsed = result_series.isna()  # Recheck which dates are still not parsed
    if not_parsed.any():
        temp_parsed = date_series[not_parsed].apply(try_parse_date)
        result_series.update(temp_parsed.dropna())
        # Final update on parse error flags
        parse_error_flag |= temp_parsed.isna() & not_parsed
    
    return result_series, parse_error_flag


formats = ["%d/%m/%Y", "%m/%d/%Y"]  # List your formats here

# Apply the parsing function to your date columns

dtX2023_12_05_DatosEgresosHosp_encrip['fecha_ingreso_rec24'], dtX2023_12_05_DatosEgresosHosp_encrip['fecha_ingreso_error'] = parse_dates_with_fallback_and_flag(
    dtX2023_12_05_DatosEgresosHosp_encrip['fecha_ingreso'], formats)


parsed_dates, parse_error_flags = parse_dates_with_fallback_and_flag(
    dtX2023_12_05_DatosEgresosHosp_encrip['fecha_egreso'], formats)

# Correctly assign each Series to its own column in the DataFrame
dtX2023_12_05_DatosEgresosHosp_encrip['fecha_egreso_rec24'] = parsed_dates
dtX2023_12_05_DatosEgresosHosp_encrip['fecha_egreso_error_flag'] = parse_error_flags


# 1. parse dates ---------------------------------------------------

import re

patterns = [
    (r"F14|R782|T405", "coc"),
    (r"F12|T407", "mar"),
    (r"F10|T510", "oh"),
    (r"F0[0-9]|F2[0-9]|F3[0-9]|F4[0-9]|F5[0-9]|F6[0-9]|F7[0-9]|F8[0-9]|F9[0-8]", "psy")  
]

def apply_patterns_vectorized(df, target_columns):
    for col in target_columns:  # Iterate only over target columns
        for pattern, value in patterns:
            mask = df[col].str.contains(pattern)
            df.loc[mask, col + '_rec3'] = value
        df[col + '_rec3'] = df[col + '_rec3'].fillna("") 
    return df

target_columns = ['diag1', 'diag2', 'diag3', 'diag4', 'diag5', 'diag6', 'diag7', 'diag8', 'diag9', 'diag10', 'diag11']

dtX2023_12_05_DatosEgresosHosp_encrip = apply_patterns_vectorized(dtX2023_12_05_DatosEgresosHosp_encrip, target_columns)  

print(dtX2023_12_05_DatosEgresosHosp_encrip.sample(5))

#dtX2023_12_05_DatosEgresosHosp_encrip.sample(5).to_parquet("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20231205_hosp_mod20240331_prueba.parquet.gzip", compression='gzip')


# dtX2023_12_05_DatosEgresosHosp_encrip = dtX2023_12_05_DatosEgresosHosp_encrip.to_pandas()

# Vectorized computation of sums and binary flags
substance_cols = [col for col in dtX2023_12_05_DatosEgresosHosp_encrip.columns if 'rec3' in col]
dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus'] = dtX2023_12_05_DatosEgresosHosp_encrip[substance_cols].isin(['coc', 'mar', 'oh']).sum(axis=1)
dtX2023_12_05_DatosEgresosHosp_encrip['sum_coc'] = dtX2023_12_05_DatosEgresosHosp_encrip[substance_cols].eq('coc').sum(axis=1)
dtX2023_12_05_DatosEgresosHosp_encrip['sum_mar'] = dtX2023_12_05_DatosEgresosHosp_encrip[substance_cols].eq('mar').sum(axis=1)
dtX2023_12_05_DatosEgresosHosp_encrip['sum_oh'] = dtX2023_12_05_DatosEgresosHosp_encrip[substance_cols].eq('oh').sum(axis=1)
dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus_psy'] = dtX2023_12_05_DatosEgresosHosp_encrip[substance_cols].isin(['coc', 'mar', 'oh', 'psy']).sum(axis=1)
# Assuming _conadi columns follow a similar naming convention
conadi_cols = [col for col in dtX2023_12_05_DatosEgresosHosp_encrip.columns if '_conadi' in col]
dtX2023_12_05_DatosEgresosHosp_encrip['sum_conadi'] = dtX2023_12_05_DatosEgresosHosp_encrip[conadi_cols].sum(axis=1)

# Compute binary flags
dtX2023_12_05_DatosEgresosHosp_encrip['recoded'] = (dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus'] > 0).astype(int)
dtX2023_12_05_DatosEgresosHosp_encrip['recoded2'] = (dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus_psy'] > 0).astype(int)
dtX2023_12_05_DatosEgresosHosp_encrip['ppo_conadi'] = (dtX2023_12_05_DatosEgresosHosp_encrip['sum_conadi'] > 0).astype(int)

# Calculate conditions for each category once
cond_coc = (dtX2023_12_05_DatosEgresosHosp_encrip['sum_coc'] > 0) & \
           (dtX2023_12_05_DatosEgresosHosp_encrip['sum_coc'] == dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus']) & \
           (dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus'] == dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus_psy'])

cond_mar = (dtX2023_12_05_DatosEgresosHosp_encrip['sum_mar'] > 0) & \
           (dtX2023_12_05_DatosEgresosHosp_encrip['sum_mar'] == dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus']) & \
           (dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus'] == dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus_psy'])

cond_oh = (dtX2023_12_05_DatosEgresosHosp_encrip['sum_oh'] > 0) & \
          (dtX2023_12_05_DatosEgresosHosp_encrip['sum_oh'] == dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus']) & \
          (dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus'] == dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus_psy'])


cond_cp = (
    (
        ((dtX2023_12_05_DatosEgresosHosp_encrip['sum_coc'] > 0).astype(int) + 
         (dtX2023_12_05_DatosEgresosHosp_encrip['sum_mar'] > 0).astype(int) + 
         (dtX2023_12_05_DatosEgresosHosp_encrip['sum_oh'] > 0).astype(int)
        ) == 1
    ) & \
    (dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus_psy'] > dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus'])
)
          
cond_psu = (
    (
        ((dtX2023_12_05_DatosEgresosHosp_encrip['sum_coc'] > 0).astype(int) + 
         (dtX2023_12_05_DatosEgresosHosp_encrip['sum_mar'] > 0).astype(int) + 
         (dtX2023_12_05_DatosEgresosHosp_encrip['sum_oh'] > 0).astype(int)
        ) >= 2
    ) & \
    (dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus'] == dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus_psy'])
)
    
    
cond_cp_psu = (
    (
        ((dtX2023_12_05_DatosEgresosHosp_encrip['sum_coc'] > 0).astype(int) + 
         (dtX2023_12_05_DatosEgresosHosp_encrip['sum_mar'] > 0).astype(int) + 
         (dtX2023_12_05_DatosEgresosHosp_encrip['sum_oh'] > 0).astype(int)
        ) >= 1
    ) & \
    (dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus_psy'] > dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus'])
)
    
    
cond_psy = (dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus_psy'] > 0) & \
           (dtX2023_12_05_DatosEgresosHosp_encrip['sum_tus'] == 0)

# Initialize the column with 'otro' to cover the default case
dtX2023_12_05_DatosEgresosHosp_encrip['alphabet'] = 'otro'

# Update the column based on conditions
dtX2023_12_05_DatosEgresosHosp_encrip.loc[cond_coc, 'alphabet'] = 'coc'
dtX2023_12_05_DatosEgresosHosp_encrip.loc[cond_mar, 'alphabet'] = 'mar'
dtX2023_12_05_DatosEgresosHosp_encrip.loc[cond_oh, 'alphabet'] = 'oh'
dtX2023_12_05_DatosEgresosHosp_encrip.loc[cond_cp, 'alphabet'] = 'cp'
dtX2023_12_05_DatosEgresosHosp_encrip.loc[cond_psu, 'alphabet'] = 'psu'
dtX2023_12_05_DatosEgresosHosp_encrip.loc[cond_cp_psu, 'alphabet'] = 'cp_psu'
dtX2023_12_05_DatosEgresosHosp_encrip.loc[cond_psy, 'alphabet'] = 'psy'

print(pd.crosstab([dtX2023_12_05_DatosEgresosHosp_encrip.sum_tus, 
             dtX2023_12_05_DatosEgresosHosp_encrip.alphabet], columns="count"))
# [5 rows x 79 columns]
# col_0                count
# sum_tus alphabet          
# 0       otro      20457809
#         psy         408666
# 1       coc          10128
#         cp_psu       10005
#         mar           4334
#         oh           61972
# 2       coc             84
#         cp_psu        1014
#         mar             30
#         oh            1085
#         psu           1598
# 3       coc              1
#         cp_psu          97
#         mar              1
#         oh              24
#         psu            136
# 4       cp_psu          11
#         psu              7
# 5       psu              2

#función de glimpse
def glimpse(df):
    print(f"Rows: {df.shape[0]}")
    print(f"Columns: {df.shape[1]}")
    for col in df.columns:
        print(f"$ {col} <{df[col].dtype}> {df[col].head().values}")

glimpse(dtX2023_12_05_DatosEgresosHosp_encrip)

#save and export 
dtX2023_12_05_DatosEgresosHosp_encrip.to_parquet("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20231205_hosp_mod20240404.parquet.gzip", compression='gzip')
#dtX2023_12_05_DatosEgresosHosp_encrip = pq.read_table("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20231205_hosp_mod20240331.parquet.gzip")
#dtX2023_12_05_DatosEgresosHosp_encrip = pq.read_table("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/10_Egresos Hospitalarios/20231205_hosp.parquet.gzip").to_pandas()