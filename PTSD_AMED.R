
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
veg = MBS_food$tom_m13 + MBS_food$sbean_m13 + MBS_food$brocc_m13 + MBS_food$cabbr_m13 + MBS_food$rcar_m13 + MBS_food$ccar_m13 + MBS_food$corn_m13 +
  MBS_food$peas_m13 + MBS_food$yam_m13 + MBS_food$cspin_m13 + MBS_food$bean_m13 + MBS_food$ysqua_m13

legu = MBS_food$sbean_m13 + MBS_food$peas_m13 + MBS_food$bean_m13

fruit = MBS_food$appl_m13 + MBS_food$oran_m13 + MBS_food$peach_m13 + MBS_food$ban_m13 + MBS_food$otfr_m13 + MBS_food$ojgr_m13 + MBS_food$punch_m13

nut = MBS_food$bean_m13 + MBS_food$pbut_m13 + MBS_food$nut_m13

wgrain = MBS_food$dkbr_m13 + MBS_food$rice_m13 + MBS_food$cakr_m13 + MBS_food$cer_m13 + MBS_food$cerbr_m13

meat = MBS_food$bfdog_m13 + MBS_food$bacon_m13 + MBS_food$procm_m13 + MBS_food$hamb_m13 + MBS_food$bmix_m13 + MBS_food$bmain_m13

fish = MBS_food$ofish_m13 + MBS_food$chfish_m13

alcohol = MBS_nutrient$alco

ms = MBS_nutrient$monfat/MBS_nutrient$satfat

##
N_subjects = dim(MBS_food)[1]
eetoh = matrix(0 , N_subjects, 1)
emsrat = matrix(0 , N_subjects, 1)
efsh = matrix(0 , N_subjects, 1)
emt = matrix(0 , N_subjects, 1)
ewgr = matrix(0 , N_subjects, 1)
eleg = matrix(0 , N_subjects, 1)
efru = matrix(0 , N_subjects, 1)
evegg = matrix(0 , N_subjects, 1)
enut = matrix(0 , N_subjects, 1)

eetoh[alcohol>=5&alcohol<=15] = 1
emsrat[ms>1.12] = 1
efsh[fish>0.14] = 1
emt[meat<0.49] = 1
ewgr[wgrain>1.0] = 1
eleg[legu>0.28] = 1
efru[fruit>1.92] = 1
evegg[veg>2.49] = 1
enut[nut>0.43] = 1

## Final
final_amed = data.frame(ID = MBS_food$id, eetoh = eetoh, emsrat = emsrat, efsh = efsh,
                        emt = emt, ewgr = ewgr, eleg = eleg,
                        efru = efru, evegg = evegg, enut = enut)
final_amed[,'sum'] = rowSums(final_amed[,2:10])
#save res
write.table(final_amed,"PTSD_final_amed.txt",row.names = FALSE,sep="\t")
