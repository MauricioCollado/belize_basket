Hi

In this readme file I describe the variables in species_belize:

- ad: country
- sci_name: scientific name of species i
- comm_name: common name of species i
- comm_name_foreign: common name of species i in local language
- indicator_spe: 1 if the species if the basket's indicator species, 0 otherwise
- basket: basket id
- group_name: basket's name according to unctad 2022
- priority: priority according to unctad 2022
- edf: 1 if the basket was prioritized by edf, 0 otherwise
- r_fishbase: intrinsic growth according to fish base
- r_cmsy: intrinsic growth according to cmsy (datalimited2)
- r_used: the intrinsic rate used in the mode. Priority = cmsy
- price: value/harvest tons. Source: Sea Around Us
- price source: SAU or taking average of the price of other basket species
- k_csmy: carrying capacity according to cmsy (datalimited2)
- k_regression: carrying capacity according to a regression against r and constant
- k_used: select k used in the model. The priority is k_csmy
- last_catch: last catch registered by SAU
- year: year of the last catch
- msy: maximum sustainable yield
- msy_source: 2 sources: cmsy (datalimited2) or msy=0.25*r*K
- q: catchability according to 1 generic technology
- q_source: 2 sources: own calculation if we have information available about catch or taking the minimum q of the species within the same basket and divide by 4.
- q/r: vulnerability index