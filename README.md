# Labour market momentum measure (LMM)
This repository calculates the Kharroubi &amp; Koechlin labour market momentum measure based on labour market flows data and updates it automatically.

The R file **lmm_update.R** contains the script to calculate the gap, and produces a .csv file and some graphs which are saved in the output folder.

The file ***Kharroubi_Koechlin_lmm_data.csv*** in the output folder contains the unemployment gap as described in our paper. There are two variables - *KK_lmm* and *KK_lmm_ma6* - which are the Kharroubi-Koechlin labour market momentum and the 6-month backward looking moving average of that lmm (which is used in the paper).

#### Notes:
- October and Noveber 2025 data is missing due to missing CPS labor force flows data. 
