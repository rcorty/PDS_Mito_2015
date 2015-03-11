library(CortyKit)

################################################################################
#                         COMPILE DATA FOR STUDY 81 DATASET                    #
################################################################################
 
########## SURVIVAL DATA ##########

s81.srv <- data.table(read.csv('../../S81/SRV_P.CSV'),
                      key = 'PID_A')

survival.81 <- s81.srv[, list(days.in.trial = as.integer(ifelse(!any(SRVCSS == 'DEAD') & all(is.na(DEATHDAY)),
                                                                max(SRVDAY, na.rm = TRUE),
                                                                max(DEATHDAY, na.rm = TRUE))),
                              surv.censor = !any(SRVCSS == 'DEAD') & all(is.na(DEATHDAY))),
                       by = PID_A]

study.81.raw <- survival.81

print('survival')
dim(survival.81)

########## TOXICITY DATA ##########

s81.tox <- data.table(read.csv('../../S81/ADVERSE.CSV'),
                      key = 'PID_A')

#### TOX OVERVIEW ####
s81.g3.tox <- s81.tox[AEGRADE == 3, .N, by = PREFTEXT]
write.table(s81.g3.tox[order(-N)], file = 'pred_g3_tox.txt')

s81.g4.tox <- s81.tox[AEGRADE == 4, .N, by = PREFTEXT]
write.table(s81.g4.tox[order(-N)], file = 'pred_g4_tox.txt')

s81.g5.tox <- s81.tox[AEGRADE == 5, .N, by = PREFTEXT]
write.table(s81.g5.tox[order(-N)], file = 'pred_g5_tox.txt')

num.g3p.81 <- s81.tox[AEFDAY > 0 & !is.na(AEFDAY),
                      list(num.g3p = sum(AEGRADE == 3)),
                      by = PID_A]

study.81.raw <- merge(study.81.raw, num.g3p.81, all.x = TRUE)
study.81.raw$num.g3p[is.na(study.81.raw$num.g3p)] <- 0

num.g4p.81 <- s81.tox[AEFDAY > 0 & !is.na(AEFDAY),
                      list(num.g4p = sum(AEGRADE == 4)),
                      by = PID_A]

study.81.raw <- merge(study.81.raw, num.g4p.81, all.x = TRUE)
study.81.raw$num.g4p[is.na(study.81.raw$num.g4p)] <- 0

num.g5p.81 <- s81.tox[AEFDAY > 0 & !is.na(AEFDAY),
                      list(num.g5p = sum(AEGRADE == 5)),
                      by = PID_A]

study.81.raw <- merge(study.81.raw, num.g5p.81, all.x = TRUE)
study.81.raw$num.g5p[is.na(study.81.raw$num.g5p)] <- 0

# # first grade 2+ toxicity
# first.grade2p.81 <- s81.tox[AEFDAY > 0 & !is.na(AEFDAY),
#                             list(tox.g2p.day = sort(AEFDAY[AEGRADE >= 2])[1]),
#                            by = PID_A]
# 
# 
# study.81.raw <- merge(study.81.raw, first.grade2p.81, all.x = TRUE)
# study.81.raw$tox.g2p.censor <- FALSE
# study.81.raw[is.na(tox.g2p.day), tox.g2p.censor := TRUE]
# study.81.raw[is.na(tox.g2p.day), tox.g2p.day := days.in.trial]
# 
# 
# # what are the g2p toxicities?
# g2p.81 <- sort(table(s81.tox[AEFDAY > 0 & !is.na(AEFDAY) & AEGRADE >= 2,
#                              PREFTEXT]))
# 
# # first grade 3+ toxicity
# first.grade3p.81 <- s81.tox[AEFDAY > 0 & !is.na(AEFDAY),
#                             list(tox.g3p.day = sort(AEFDAY[AEGRADE >= 3])[1]),
#                             by = PID_A]
# 
# 
# study.81.raw <- merge(study.81.raw, first.grade3p.81, all.x = TRUE)
# study.81.raw$tox.g3p.censor <- FALSE
# study.81.raw[is.na(tox.g3p.day), tox.g3p.censor := TRUE]
# study.81.raw[is.na(tox.g3p.day), tox.g3p.day := days.in.trial]
# 
# # what are the g3p toxicities?
# g3p.81 <- sort(table(s81.tox[AEFDAY > 0 & !is.na(AEFDAY) & AEGRADE >= 3,
#                              PREFTEXT]))
# 
# grade2.81 <- s81.tox[, list(day = AEFDAY[AEGRADE == 2]),
#                       by = PID_A]
# 
# grade3.81 <- s81.tox[, list(day = AEFDAY[AEGRADE == 3]),
#                      by = PID_A]
# 
# grade2p.81 <- s81.tox[, list(day = AEFDAY[AEGRADE >= 2]),
#                       by = PID_A]
# 
# grade3p.81 <- s81.tox[, list(day = AEFDAY[AEGRADE >= 3]),
#                        by = PID_A]

########## DEMOGRAPHIC DATA ##########

s81.demog <- data.table(read.csv('../../S81/DEMOG.CSV'),
                        key = 'PID_A')

demog.81 <- s81.demog[, list(PID_A, age = AGE, race = toupper(RACESC), bmi = 1e4*WT/(HT^2))]

study.81.raw <- merge(study.81.raw, na.omit(demog.81))

print('demog')
dim(demog.81)

########## MEDICATION HISTORY ##########

s81.condrug <- data.table(read.csv('../../S81/CONDRUG.CSV'),
                          key = 'PID_A')

# drop this six unintelligible records
s81.condrug <- s81.condrug[!'YES']
s81.condrug <- s81.condrug[!'']


# docetaxel
doce.81 <- s81.condrug[CDRGCOPT == 'DOCETAXEL',
                       list(recent.doce = any(CDTDAY > -182 & CDTDAY < 0, na.rm = TRUE)),
                       by = PID_A]

study.81.raw <- merge(study.81.raw, doce.81)

print('doce')
dim(doce.81)
# hormonal.drugs <- c('ABIRATERONE ACETATE' , 'BICALUTAMIDE' , 'BUSERELIN ACETATE',
#                     'CYPROTERONE', 'CYPROTERONE ACETATE', 'DIETHYLSTILBESTROL',
#                     'ESTRADURIN ', 'ESTRAMUSTINE', 'ESTRAMUSTINE PHOSPHATE',
#                     'ESTRAMUSTINE PHOSPHATE SODIUM', 'FLUTAMIDE', 'GONADORELIN',
#                     'GOSERELIN', 'GOSERELIN ACETATE', 'KETOCONAZOLE', 'LEUPRORELIN',
#                     'LEUPRORELIN ACETATE', 'MEGESTROL ACETATE', 'NILUTAMIDE',
#                     'TAMOXIFEN', 'TRIPTORELIN', 'TRIPTORELIN ACETATE', 
#                     'TRIPTORELIN EMBONATE', 'BUSERELIN', 'ESTRADURIN',
#                     'ANDROSTANOLONE', 'GONADOTROPIN-RELEASING HORMONES',
#                     'GONADOTROPHIN RELEASING HORMONE ANALOGUES')
# 
# 
# HormUseFunc <- function(df) {
#    return(nrow(df))
# }
# 
# dur.horm.use.81 <- ddply(s81.condrug[s81.condrug$CDRGCOPT %in% hormonal.drugs,], # & !is.na(s81.condrug$CDFDAY) & !is.na(s81.condrug$CDTDAY),],
#                          .(PID_A),
#                          HormUseFunc)
# names(dur.horm.use.81) <- c('USUBJID', 'num_horm_tx')

# check for abiraterone use
abirat.81 <- s81.condrug[CDRGCOPT %like% 'ABIRATERONE',
                         list(PID_A, abirat = TRUE)]

study.81.raw <- study.81.raw[!abirat.81]

print('abirat')
dim(abirat.81)

########## CLINICAL MEASURES ##########

s81.pfmp <- data.table(read.csv('../../S81/PFM_P.CSV'),
                       key = 'PID_A')

# ecog
ecog.81 <- s81.pfmp[!is.na(PFMECOG),
                    list(ecog = PFMECOG[which.min(EFDAY)]),
                    by = PID_A]

study.81.raw <- merge(study.81.raw, ecog.81)

print('ecog')
dim(ecog.81)

########## LABORATORY MEASURES ##########

s81.labs <- data.table(read.csv('../../S81_2/LAB_SAFE_NUM.csv'),
                       key = 'PID_A')

# # albmumin NOT AVAIL IN STUDY79
# alb.81.raw <- s81.labs[LBTEST == 'ALBUMIN' & !is.na(LABVALUE_NUM),
#                        list(alb = LABVALUE_NUM[COLLDAY <= 1][which.max(COLLDAY[COLLDAY <= 1])],
#                             units = toupper(LABUNITR[COLLDAY <= 1][which.max(COLLDAY[COLLDAY <= 1])])),
#                        by = PID_A]
# 
# alb.81 <- alb.81.raw[units == 'G/DL', alb := alb * 10]
# alb.81.raw$units <- NULL
# 
# study.81.raw <- merge(study.81.raw, alb.81)

# hgb
hgb.81.raw <- s81.labs[LBTEST == 'HEMOGLOBIN' & !is.na(LABVALUE_NUM),
                       list(hgb = LABVALUE_NUM[COLLDAY <= 1][which.max(COLLDAY[COLLDAY <= 1])],
                            units = toupper(LABUNITR[COLLDAY <= 1][which.max(COLLDAY[COLLDAY <= 1])])),
                       by = PID_A]

# one record appears to have an erroneously-recorded unit of 'G%'
# this does not need to be fixed bc the measurement appears to be in G/DL

# standardize units
hgb.81 <- hgb.81.raw[units == 'G/L',
                     `:=`(hgb = hgb/10, units = 'G/DL')]
hgb.81$units <- NULL

study.81.raw <- merge(study.81.raw, hgb.81)

print('hgb')
dim(hgb.81)

# PSA
psa.81.raw <- s81.labs[LBTEST == 'PROSTATIC SPECIFIC ANTIGEN (PSA)' & !is.na(LABVALUE_NUM),
                       list(psa = LABVALUE_NUM[COLLDAY <= 1][which.max(COLLDAY[COLLDAY <= 1])],
                            units = toupper(LABUNITR[COLLDAY <= 1][which.max(COLLDAY[COLLDAY <= 1])])),
                       by = PID_A]

# note that ng/mL = ug/L and these are the only two units, so no need to change anything
psa.81 <- psa.81.raw[, list(PID_A, psa)]

study.81.raw <- merge(study.81.raw, psa.81)

print('psa')
dim(psa.81)


alkp.81.raw <- s81.labs[LBTEST == 'ALKALINE PHOSPHATASE' & !is.na(LABVALUE_NUM),
                       list(alkp = LABVALUE_NUM[COLLDAY <= 1][which.max(COLLDAY[COLLDAY <= 1])]),
                       by = PID_A]

study.81.raw <- merge(study.81.raw, alkp.81.raw)

########## RADIOLOGIC MEASURES ##########


s81.tmmp <- data.table(read.csv('../../S81/TMM_P.CSV'),
                       key = 'PID_A')

meas.81.raw <- s81.tmmp[EFDAY <= 1,
                        list(spiral.meas = any(TMMDIA[TMMPROC == 'SPIRAL CT SCAN'] > 10, na.rm = TRUE),
                             ct.meas = any(TMMDIA[TMMPROC == 'CONVENTIONAL CT SCAN'] > 20, na.rm = TRUE)),
                        by = PID_A]

meas.81 <- meas.81.raw[, list(PID_A, meas = (spiral.meas | ct.meas))]

study.81.raw <- merge(study.81.raw, meas.81)


met.81 <- s81.tmmp[EFDAY <= 1,
                   list(bone = any(TMMDIS == 'BONE'),
                        visc = any(TMMDIS %in% unique(s81.tmmp$TMMDIS)[-c(1,2,3,6)])),
                   by = PID_A]

study.81.raw <- merge(study.81.raw, met.81)

# ########## PAIN MEASURE ##########
# 
# 
# temp <- read.csv('../../S81/PREVDIS.CSV')


############# DATA PROCESSING ################

study.81 <- study.81.raw

# these pts had prednisone only
study.81$treatment <- 0

print(dim(study.81))
saveRDS(object = study.81, file = 'study81.dt')