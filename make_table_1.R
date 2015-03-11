befor.psm <- readRDS('mito_data_before_PSM')
after.psm <- readRDS('caliper_matched_data')

befor.pred <- befor.psm[befor.psm$treatment == 0, ]
befor.mito <- befor.psm[befor.psm$treatment == 1, ]
after.pred <- after.psm[after.psm$treatment == 0, ]
after.mito <- after.psm[after.psm$treatment == 1, ]

datasets <- list(befor.pred, befor.mito, after.pred, after.mito)

#### N ####
sapply(datasets, nrow)

#### age ####
sapply(datasets, function(dt) round(mean(dt$age), 1))
sapply(datasets, function(dt) paste('(', paste0(range(dt$age), collapse = ' '), ')'))

t.test(befor.pred$age, befor.mito$age)$p.value
t.test(after.pred$age, after.mito$age)$p.value

#### race ####
races = c('WHITE', 'BLACK', 'ASIAN', 'OTHER')
for (race in races) {
   print(race)
   print(sapply(datasets, function(dt) paste('n:', sum(dt$race == race),
                                             '%:', round(sum(dt$race == race)/nrow(dt), 2))))
   
   print(prop.test(x = c(sum(befor.pred$race == race), sum(befor.mito$race == race)),
             n = c(nrow(befor.pred), nrow(befor.mito)))$p.value)
   print(prop.test(x = c(sum(after.pred$race == race), sum(after.mito$race == race)),
             n = c(nrow(after.pred), nrow(after.mito)))$p.value)
}

#### BMI ####
sapply(datasets, function(dt) paste('median:', round(median(dt$bmi), 1),
                                    'range:', paste(round(range(dt$bmi), 1), collapse = ' ')))

t.test(befor.pred$bmi, befor.mito$bmi)$p.value
t.test(after.pred$bmi, after.mito$bmi)$p.value


#### alkp ####
sapply(datasets, function(dt) paste('median:', round(median(dt$alkp), 1),
                                    'range:', paste(round(range(dt$alk), 1), collapse = ' ')))

t.test(befor.pred$alkp, befor.mito$alkp)$p.value
t.test(after.pred$alkp, after.mito$alkp)$p.value


#### PSA ####
sapply(datasets, function(dt) paste('median:', round(median(dt$psa), 1),
                                    'range:', paste(round(range(dt$psa), 1), collapse = ' ')),
       simplify = FALSE)

t.test(befor.pred$psa, befor.mito$psa)$p.value
t.test(after.pred$psa, after.mito$psa)$p.value

#### ecog ####
sapply(datasets, function(dt) paste('n = 0:', sum(dt$ecog == 0),
                                    '   % = 0:', round(sum(dt$ecog == 0)/nrow(dt), 2),
                                    '   n = 1:', sum(dt$ecog == 1),
                                    '   % = 1:', round(sum(dt$ecog == 1)/nrow(dt)), 2),
       simplify = FALSE)

prop.test(x = c(sum(befor.pred$ecog == 0), sum(befor.mito$ecog == 0)),
          n = c(nrow(befor.pred), nrow(befor.mito)))$p.value
prop.test(x = c(sum(after.pred$ecog == 0), sum(after.mito$ecog == 0)),
          n = c(nrow(after.pred), nrow(after.mito)))$p.value

#### hgb ####
sapply(datasets, function(dt) c(median(dt$hgb),
                                range(dt$hgb)),
       simplify = FALSE)

t.test(befor.pred$hgb, befor.mito$hgb)$p.value
t.test(after.pred$hgb, after.mito$hgb)$p.value

#### recent doce ####
sapply(datasets, function(dt) paste('n = 0:', sum(dt$recent.doce == 0),
                                    '   % = 0:', round(sum(dt$recent.doce == 0)/nrow(dt), 2),
                                    '   n = 1:', sum(dt$recent.doce == 1),
                                    '   % = 1:', round(sum(dt$recent.doce == 1)/nrow(dt)), 2),
       simplify = FALSE)

prop.test(x = c(sum(befor.pred$recent.doce == 0), sum(befor.mito$recent.doce == 0)),
          n = c(nrow(befor.pred), nrow(befor.mito)))$p.value
prop.test(x = c(sum(after.pred$recent.doce == 0), sum(after.mito$recent.doce == 0)),
          n = c(nrow(after.pred), nrow(after.mito)))$p.value

#### bone mets ####

sapply(datasets, function(dt) paste('n = F:', sum(dt$bone == FALSE),
                                    '   % = F:', round(100*sum(dt$bone == 0)/nrow(dt)),
                                    '   n = T:', sum(dt$bone == TRUE),
                                    '   % = T:', round(100*sum(dt$bone == 1)/nrow(dt))),
       simplify = FALSE)

prop.test(x = c(sum(befor.pred$visc == 0), sum(befor.mito$visc == 0)),
          n = c(nrow(befor.pred), nrow(befor.mito)))$p.value
prop.test(x = c(sum(after.pred$visc == 0), sum(after.mito$visc == 0)),
          n = c(nrow(after.pred), nrow(after.mito)))$p.value


#### visceral mets ####

sapply(datasets, function(dt) paste('n = F:', sum(dt$visc == FALSE),
                                    '   % = F:', round(100*sum(dt$visc == FALSE)/nrow(dt)),
                                    '   n = T:', sum(dt$visc == TRUE),
                                    '   % = T:', round(100*sum(dt$visc == TRUE)/nrow(dt))),
       simplify = FALSE)

prop.test(x = c(sum(befor.pred$visc == 0), sum(befor.mito$visc == 0)),
          n = c(nrow(befor.pred), nrow(befor.mito)))$p.value
prop.test(x = c(sum(after.pred$visc == 0), sum(after.mito$visc == 0)),
          n = c(nrow(after.pred), nrow(after.mito)))$p.value



#### patients with both bone and visc  mets ####

sapply(datasets, function(dt) paste('n = F:', sum(dt$visc == FALSE | dt$bone == FALSE),
                                    '   % = F:', round(100*sum(dt$visc == FALSE | dt$bone == FALSE)/nrow(dt)),
                                    '   n = T:', sum(dt$visc == TRUE & dt$visc == TRUE),
                                    '   % = T:', round(100*sum(dt$visc == TRUE & dt$bone == TRUE)/nrow(dt))),
       simplify = FALSE)

prop.test(x = c(sum(befor.pred$visc == 0), sum(befor.mito$visc == 0)),
          n = c(nrow(befor.pred), nrow(befor.mito)))$p.value
prop.test(x = c(sum(after.pred$visc == 0), sum(after.mito$visc == 0)),
          n = c(nrow(after.pred), nrow(after.mito)))$p.value
