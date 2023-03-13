clear

use bond_dataset

format operation_date %td

encode ISIN, generate(isin2)

xtset isin2 operation_date 

xtsum percent_of_freefloat

