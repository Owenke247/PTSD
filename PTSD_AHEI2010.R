
rm(list=ls())
library("readxl")

##### This script shows the the error distributions of training and test sets
setwd("/Users/shanlin/HMS_shanlin/Project/PTSD/Diet/diet_scores")

MBS_food = read_excel("MBS_food.xlsx",col_names = TRUE, col_types = "numeric")
MBS_food = replace(MBS_food, is.na(MBS_food), 0)
MBS_nutrient = read_excel("MBS_nutrient.xlsx",col_names = TRUE, col_types = "numeric")
MBS_nutrient = replace(MBS_nutrient, is.na(MBS_nutrient), 0)

overlaped = intersect(MBS_food$id, MBS_nutrient$id)
MBS_food = MBS_food[match(overlaped,MBS_food$id),]
MBS_nutrient = MBS_nutrient[match(overlaped,MBS_nutrient$id),]
# convert frequency to serving/day
MBS_food[MBS_food == 1] = 0.07
MBS_food[MBS_food == 2] = 0.14
MBS_food[MBS_food == 3] = 0.43
MBS_food[MBS_food == 4] = 0.79
MBS_food[MBS_food == 5] = 1
MBS_food[MBS_food == 6] = 2.5
MBS_food[MBS_food == 7] = 4.5
MBS_food[MBS_food == 8] = 6

##
ahei2010_veg = MBS_food$tom_m13 + MBS_food$sbean_m13 + MBS_food$brocc_m13 + MBS_food$cabbr_m13 + MBS_food$rcar_m13 + MBS_food$ccar_m13 + MBS_food$corn_m13 +
  MBS_food$peas_m13 + MBS_food$yam_m13 + MBS_food$cspin_m13 + MBS_food$bean_m13 + MBS_food$ysqua_m13 #Total vegetables

ahei2010_frt = MBS_food$appl_m13 + MBS_food$oran_m13 + MBS_food$peach_m13 + MBS_food$ban_m13 + MBS_food$otfr_m13 #Whole fruit

ahei2010_ssb = MBS_food$ojgr_m13 + MBS_food$punch_m13 + MBS_food$cola_m13 #Sugar-sweetened Beverages and fruit juice

ahei2010_etoh = MBS_food$beer_m13 + MBS_food$wine_m13 + MBS_food$liq_m13 #Alcohol

ahei2010_nut = MBS_food$bean_m13 + MBS_food$pbut_m13 + MBS_food$nut_m13 #Nuts and legumes

ahei2010_rmt = MBS_food$bfdog_m13 + MBS_food$bacon_m13 + MBS_food$procm_m13 + MBS_food$hamb_m13 + MBS_food$bmix_m13 + MBS_food$bmain_m13 #Red and/or Processed Meats

ahei2010_omega = MBS_nutrient$omega*1000 #Long-chain (ω-3) fats (EPA + DHA)

ahei2010_poly = ((MBS_nutrient$poly - MBS_nutrient$omega)*900)/MBS_nutrient$calor #?

ahei2010_ptran = MBS_nutrient$trn11*9/MBS_nutrient$calor

ahei2010_whgrn = MBS_nutrient$whgrn #Whole grains

##
N_subjects = dim(MBS_food)[1]

## vegetable
ahei2010_vega = ahei2010_veg
ahei2010_vegIa = ahei2010_veg
for (i in 1:N_subjects){
  if (ahei2010_vega[i] == 0){ahei2010_vegIa[i] = 0}
  else if(ahei2010_vega[i] > 0){ahei2010_vegIa[i] = ahei2010_vega[i]/0.5}
}
ahei2010_vegIa[ahei2010_vegIa<0] = 0
ahei2010_vegIa[ahei2010_vegIa>10] = 10

## Fruits
ahei2010_frta= ahei2010_frt
ahei2010_frtIa = ahei2010_frt
for (i in 1:N_subjects){
  if (ahei2010_frta[i] == 0){ahei2010_frtIa[i] = 0}
  else if(ahei2010_frta[i] > 0){ahei2010_frtIa[i] = ahei2010_frta[i]/0.4}
}
ahei2010_frtIa[ahei2010_frtIa<0] = 0
ahei2010_frtIa[ahei2010_frtIa>10] = 10

## SSB + Fruit juice
ahei2010_ssba= ahei2010_ssb
ahei2010_ssbIa = ahei2010_ssb
for (i in 1:N_subjects){
  if (ahei2010_ssba[i] == 0){ahei2010_ssbIa[i] = 10}
  else if(ahei2010_ssba[i] > 0){ahei2010_ssbIa[i] = 10 - ahei2010_ssba[i]/0.1}
}
ahei2010_ssbIa[ahei2010_ssbIa<0] = 0
ahei2010_ssbIa[ahei2010_ssbIa>10] = 10

## Nuts
ahei2010_nuta= ahei2010_nut
ahei2010_nutIa = ahei2010_nut
for (i in 1:N_subjects){
  if (ahei2010_nuta[i] == 0){ahei2010_nutIa[i] = 0}
  else if(ahei2010_nuta[i] > 0){ahei2010_nutIa[i] = ahei2010_nuta[i]/0.1}
}
ahei2010_nutIa[ahei2010_nutIa<0] = 0
ahei2010_nutIa[ahei2010_nutIa>10] = 10

## red meat
ahei2010_rmta= ahei2010_rmt
ahei2010_rmtIa = ahei2010_rmt
for (i in 1:N_subjects){
  if (ahei2010_rmta[i] == 0){ahei2010_rmtIa[i] = 0}
  else if(ahei2010_rmta[i] > 0){ahei2010_rmtIa[i] = (1.5 - ahei2010_rmta[i])/0.143}
}
ahei2010_rmtIa[ahei2010_rmtIa<0] = 0
ahei2010_rmtIa[ahei2010_rmtIa>10] = 10

## omega
ahei2010_omegaa= ahei2010_omega
ahei2010_omegaIa = ahei2010_omega
for (i in 1:N_subjects){
  if (ahei2010_omegaa[i] == 0){ahei2010_omegaIa[i] = 0}
  else if(ahei2010_omegaa[i] > 0){ahei2010_omegaIa[i] = ahei2010_omegaa[i]/25}
}
ahei2010_omegaIa[ahei2010_omegaIa<0] = 0
ahei2010_omegaIa[ahei2010_omegaIa>10] = 10

## poly fat
ahei2010_polya= ahei2010_poly
ahei2010_polyIa = ahei2010_poly
for (i in 1:N_subjects){
  if (ahei2010_polya[i] == 0){ahei2010_polyIa[i] = 0}
  else if(ahei2010_polya[i] > 0){ahei2010_polyIa[i] = (ahei2010_polya[i]-2)/0.8}
}
ahei2010_polyIa[ahei2010_polyIa<0] = 0
ahei2010_polyIa[ahei2010_polyIa>10] = 10

## trans fat
ahei2010_ptrana= ahei2010_ptran
ahei2010_ptranIa = ahei2010_ptran
for (i in 1:N_subjects){
  if (ahei2010_ptrana[i] == 0){ahei2010_ptranIa[i] = 0}
  else if(ahei2010_ptrana[i] > 0){ahei2010_ptranIa[i] = (0.04 - ahei2010_ptrana[i])/0.0035}
}
ahei2010_ptranIa[ahei2010_ptranIa<0] = 0
ahei2010_ptranIa[ahei2010_ptranIa>10] = 10


## alcohol
ahei2010_etoha= ahei2010_etoh
ahei2010_etohIa = ahei2010_etoh
for (i in 1:N_subjects){
  if (ahei2010_etoha[i] == 0){ahei2010_etohIa[i] = 2.5}
  else if(ahei2010_etoha[i] > 0 & ahei2010_etoha[i] < 0.5){ahei2010_etohIa[i] =5}
  else if(ahei2010_etoha[i] > 0 & ahei2010_etoha[i] < 0.5){ahei2010_etohIa[i] =10}
  else if(ahei2010_etoha[i] > 0.5 & ahei2010_etoha[i] < 1.5){ahei2010_etohIa[i] =5}
  else if(ahei2010_etoha[i] > 1.5 & ahei2010_etoha[i] < 2.5){ahei2010_etohIa[i] =2.5}
  else if(ahei2010_etoha[i] > 2.5){ahei2010_etohIa[i] =0}
}

## whole grains
ahei2010_whgrna= ahei2010_whgrn
ahei2010_whgrnIa = ahei2010_whgrn
for (i in 1:N_subjects){
  if (ahei2010_whgrna[i] == 0){ahei2010_whgrnIa[i] = 0}
  else if(ahei2010_whgrna[i] > 0){ahei2010_whgrnIa[i] = ahei2010_whgrna[i]/7.5}
}
ahei2010_whgrnIa[ahei2010_whgrnIa<0] = 0
ahei2010_whgrnIa[ahei2010_whgrnIa>10] = 10

## sodium
scores = findInterval(MBS_nutrient$sodium, seq(min(MBS_nutrient$sodium), max(MBS_nutrient$sodium), 250))
ahei2010_naIa = scores

## Final
final_ahei2010 = data.frame(ID = MBS_food$id, ahei2010_vegIa = ahei2010_vegIa, ahei2010_frtIa = ahei2010_frtIa, ahei2010_ssbIa = ahei2010_ssbIa,
                            ahei2010_etohIa = ahei2010_etohIa, ahei2010_nutIa = ahei2010_nutIa, ahei2010_rmtIa = ahei2010_rmtIa,
                            ahei2010_omegaIa = ahei2010_omegaIa, ahei2010_polyIa = ahei2010_polyIa, ahei2010_ptranIa = ahei2010_ptranIa,
                            ahei2010_whgrnIa = ahei2010_whgrnIa, ahei2010_naIa = ahei2010_naIa)
final_ahei2010[,'sum'] = rowSums(final_ahei2010[,2:12])

write.table(final_ahei2010,"PTSD_AHEI2010.txt",row.names = FALSE,sep="\t")
