

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
wgrain = MBS_food$dkbr_m13 + MBS_food$rice_m13 + MBS_food$cakr_m13 + MBS_food$cer_m13 + MBS_food$cerbr_m13

fruit = MBS_food$appl_m13 + MBS_food$oran_m13 + MBS_food$peach_m13 + MBS_food$ban_m13 + MBS_food$otfr_m13 + MBS_food$ojgr_m13 + MBS_food$punch_m13

veg = MBS_food$tom_m13 + MBS_food$sbean_m13 + MBS_food$brocc_m13 + MBS_food$cabbr_m13 + MBS_food$rcar_m13 + MBS_food$ccar_m13 + MBS_food$corn_m13 +
  MBS_food$peas_m13 + MBS_food$yam_m13 + MBS_food$cspin_m13 + MBS_food$bean_m13 + MBS_food$ysqua_m13

nut_legu = MBS_food$sbean_m13 + MBS_food$peas_m13 + MBS_food$bean_m13 + MBS_food$pbut_m13 + MBS_food$nut_m13

dai = MBS_food$skim_m13 + MBS_food$yog_m13

meat = MBS_food$bfdog_m13 + MBS_food$bacon_m13 + MBS_food$procm_m13 + MBS_food$hamb_m13 + MBS_food$bmix_m13 + MBS_food$bmain_m13

sof = MBS_food$cola_m13 + MBS_food$sugar_m13 + MBS_food$punch_m13

##
qgroup = function(numvec, n = 6){
  
  qtile = quantile(numvec, probs = seq(0, 1, 1/n))
  out = sapply(numvec, function(x) sum(x >= qtile[-(n+1)]))
  
  return(out)
}

N_subjects = dim(MBS_food)[1]
final_dash = qgroup(wgrain) + qgroup(veg) + qgroup(nut_legu)+ qgroup(wgrain) + qgroup(dai) +
  6 - qgroup(meat) + 6 - qgroup(sof) + 6 - qgroup(MBS_nutrient$sodium)

## Final
final_dash = data.frame(ID = MBS_food$id, sum = final_dash)

#save res
write.table(final_dash,"PTSD_final_dash.txt",row.names = FALSE,sep="\t")
