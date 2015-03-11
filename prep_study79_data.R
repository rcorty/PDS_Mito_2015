library(CortyKit)

################################################################################
#                         COMPILE DATA FOR STUDY 79 DATASET                    #
################################################################################

########## SURVIVAL DATA ##########

s79.ds <- data.table(read.csv('../../S79/DS.CSV'),
                     key = 'USUBJID')

survival.79 <- s79.ds[,
                      list(days.in.trial = 7*(DSSTWK[DSSCAT == 'END OF STUDY'] - DSSTWK[DSSCAT == 'RANDOMIZATION']),
                           surv.censor = DSDECOD[DSSCAT == 'END OF STUDY'] != 'DEATH'),
                      by = USUBJID]

study.79.raw <- survival.79

print('survival')
dim(survival.79)

########## TOXICITY DATA ##########

s79.tox <- data.table(read.csv('../../S79/AE.CSV'),
                      key = 'USUBJID')

#### TOX OVERVIEW ####
s79.g3.tox <- s79.tox[AETOXGR == 3, .N, by = AEDECOD]
write.table(s79.g3.tox[order(-N)], file = 'mitopred_g3_tox.txt')

s79.g4.tox <- s79.tox[AETOXGR == 4, .N, by = AEDECOD]
write.table(s79.g4.tox[order(-N)], file = 'mitopred_g4_tox.txt')

s79.g5.tox <- s79.tox[AETOXGR == 5, .N, by = AEDECOD]
write.table(s79.g5.tox[order(-N)], file = 'mitopred_g5_tox.txt')

num.g3p.79 <- s79.tox[!is.na(AESTWK) & AESTWK >= 0,
                            list(num.g3p = sum(AETOXGR == 3)),
                            by = USUBJID]

study.79.raw <-  merge(study.79.raw, num.g3p.79, all.x = TRUE)
study.79.raw$num.g3p[is.na(study.79.raw$num.g3p)] <- 0

num.g4p.79 <- s79.tox[!is.na(AESTWK) & AESTWK >= 0,
                      list(num.g4p = sum(AETOXGR == 4)),
                      by = USUBJID]

study.79.raw <-  merge(study.79.raw, num.g4p.79, all.x = TRUE)
study.79.raw$num.g4p[is.na(study.79.raw$num.g4p)] <- 0

num.g5p.79 <- s79.tox[!is.na(AESTWK) & AESTWK >= 0,
                      list(num.g5p = sum(AETOXGR == 5)),
                      by = USUBJID]

study.79.raw <-  merge(study.79.raw, num.g5p.79, all.x = TRUE)
study.79.raw$num.g5p[is.na(study.79.raw$num.g5p)] <- 0


# first.grade2p.79 <- s79.tox[!is.na(AESTWK) & AESTWK >= 0,
#                             list(tox.g2p.day = 7*sort(AESTWK[AETOXGR >= 2])[1]),
#                             by = USUBJID]
# 
# study.79.raw <- merge(study.79.raw, first.grade2p.79, all.x = TRUE)
# study.79.raw$tox.g2p.censor <- FALSE
# study.79.raw[is.na(tox.g2p.day), tox.g2p.censor := TRUE]
# study.79.raw[is.na(tox.g2p.day), tox.g2p.day := days.in.trial]
# 
# # what are the grade2p toxicities?
# g2p.79 <- sort(table(s79.tox[!is.na(AESTWK) & AESTWK >=0 & AETOXGR >=2,
#                              AETERM]))
# 
# first.grade3p.79 <- s79.tox[!is.na(AESTWK) & AESTWK >= 0,
#                             list(tox.g3p.day = 7*sort(AESTWK[AETOXGR >= 3])[1]),
#                             by = USUBJID]
# 
# study.79.raw <- merge(study.79.raw, first.grade3p.79, all.x = TRUE)
# study.79.raw$tox.g3p.censor <- FALSE
# study.79.raw[is.na(tox.g3p.day), tox.g3p.censor := TRUE]
# study.79.raw[is.na(tox.g3p.day), tox.g3p.day := days.in.trial]
# 
# # what are the grade3p toxicities?
# g3p.79 <- sort(table(s79.tox[!is.na(AESTWK) & AESTWK >=0 & AETOXGR >=3,
#                              AETERM]))

########## DEMOGRAPHIC DATA ##########

s79.dm <- data.table(read.csv('../../S79/DM.CSV'),
                     key = 'USUBJID')

# age and race
demog.79 <- data.table(USUBJID = s79.dm$USUBJID,
                     age = as.character(s79.dm$AGEGRP),
                     race = as.character(s79.dm$RACE))

demog.79[age == '>=85', age := '85']
demog.79$age = as.numeric(demog.79$age)
demog.79[race == 'BLACK OR AFRICAN AMERICAN', race := 'BLACK']

study.79.raw <-  merge(study.79.raw, demog.79)

print('demog')
dim(demog.79)

########## MEDICATION HISTORY ##########

s79.cm <- data.table(read.csv('../../S79/CM.csv'),
                     key = 'USUBJID')

# duration of hormonal drug use...removed due to unclear definition
# dur.horm.use.79 <- s79.cm[s79.cm$CMCAT == 'PRIOR TREATMENT HORMONAL THERAPY', # & !is.na(s79.cm$CMSTDY) & !is.na(s79.cm$CMENDY) & s79.cm$CMSTDY != s79.cm$CMENDY, 
#                         c('USUBJID', 'CMSTDY', 'CMENDY')]
# dur.horm.use.79$days <- dur.horm.use.79$CMENDY - dur.horm.use.79$CMSTDY
# dur.horm.use.79 <- subset(dur.horm.use.79, select=-c(CMSTDY, CMENDY))
# dur.horm.use.79 <- aggregate(dur.horm.use.79$days, by=list(dur.horm.use.79$USUBJID), FUN=length)
# names(dur.horm.use.79) <- c('USUBJID', 'num_horm_tx')


# recent docetaxel (within last 182 days)
dated.doce.79 <- s79.cm[(grepl('DOCETAXEL', CMDECOD, ignore.case=TRUE) | 
                            grepl('DOCETAXEL', CMTRT, ignore.case=TRUE)) & 
                           !is.na(CMENDY),
                        list(USUBJID, CMENDY)]
recent.doce.79 <- dated.doce.79[, list(recent.doce = any(CMENDY > -182 & CMENDY < 0)), 
                                keyby = 'USUBJID']

study.79.raw <- merge(study.79.raw, recent.doce.79)

print('doce')
dim(recent.doce.79)

# don't include pts who took or may have taken abiraterone
abirat.79 <- s79.cm[(grepl('ABIRAT', CMDECOD, ignore.case=TRUE) |
                        grepl('ABIRAT', CMTRT, ignore.case=TRUE)),
                    list(USUBJID, abirat = TRUE),
                    key = 'USUBJID']
abirat.79 <- unique(abirat.79)

study.79.raw <- study.79.raw[!abirat.79]

print('abirat')
dim(abirat.79)

# no pts took enzalutamide in this study
#enzalut.79 <- s79.cm[(grepl('ENZALUT', s79.cm$CMDECOD, ignore.case=TRUE) | grepl('ENZALUT', s79.cm$CMTRT, ignore.case=TRUE)),
#                    c('USUBJID'), drop=FALSE]
#enzalut.79$enzalut <- TRUE
#enzalut.79 <- enzalut.79[!duplicated(enzalut.79),]

########## CLINICAL MEASURES ##########

s79.vs <- data.table(read.csv('../../S79/VS.CSV'),
                     key = 'USUBJID')

# bmi
bmi.79 <- s79.vs[VSTESTCD == 'BMI' & VISIT == 'BASELINE' & !is.na(VSSTRESN),
                 list(bmi = mean(VSSTRESN)),
                 by = USUBJID]

print('bmi')
dim(bmi.79)

study.79.raw <- merge(study.79.raw, bmi.79)

# ecog
ecog.79 <- s79.vs[VSTESTCD=='ECOG' & VISIT == 'BASELINE' & !is.na(VSSTRESN),
                  list(ecog = min(VSSTRESN)),
                  by = USUBJID]

study.79.raw <- merge(study.79.raw, ecog.79)

print('ecog')
dim(ecog.79)

########## LABORATORY MEASURES ##########

s79.lb <- data.table(read.csv('../../S79/LB.CSV'),
                     key = 'USUBJID')

# alb NOT AVAIL
# alb.79.raw <- s79.lb[LBTEST == 'HEMOGLOBIN' & VISIT == 'BASELINE' & !is.na(LBSTRESN),
#                      list(hgb = LBSTRESN[which.max(LBDY)], units = LBSTRESU[which.max(LBDY)]),
#                      by = USUBJID]


# hgb
hgb.79.raw <- s79.lb[LBTEST == 'HEMOGLOBIN' & VISIT == 'BASELINE' & !is.na(LBSTRESN),
                     list(hgb = LBSTRESN[which.max(LBDY)], units = LBSTRESU[which.max(LBDY)]),
                     by = USUBJID]

# all units are 'g/L', so divide all by 10 to get standard units
hgb.79 <- hgb.79.raw[,list(USUBJID = USUBJID, hgb = hgb/10)]

study.79.raw <- merge(study.79.raw, hgb.79)

print('hgb')
dim(hgb.79)

# PSA
psa.79.raw <- s79.lb[LBTEST == 'PROSTATE SPECIFIC ANTIGEN' & VISIT == 'BASELINE' & !is.na(LBSTRESN),
                     list(psa = LBSTRESN[which.max(LBDY)], units = LBSTRESU[which.max(LBDY)]),
                     by = USUBJID]

# no changes to units
psa.79 <- psa.79.raw[, list(USUBJID, psa)]

study.79.raw <- merge(study.79.raw, psa.79)

print('psa')
dim(psa.79)


# alkp
alkp.79.raw <- s79.lb[LBTEST == 'ALKALINE PHOSPHATASE' & VISIT == 'BASELINE' & !is.na(LBSTRESN),
                     list(alkp = LBSTRESN[which.max(LBDY)]),
                     by = USUBJID]

study.79.raw <- merge(study.79.raw, alkp.79.raw)

########## RADIOLOGY MEASURES ##########

s79.ls <- data.table(read.csv('../../S79/LS.CSV'),
                     key = 'USUBJID')

# measurable disease
meas.79.raw <- s79.ls[VISIT == 'BASELINE' & !is.na(LSSTRESN),
                  list(spiral.meas = any(LSSTRESN[LSMETHOD == 'SPIRAL CT SCAN'] >= 10, na.rm = TRUE),
                       ct.meas = any(LSSTRESN[LSMETHOD == 'CT SCAN'] >= 20, na.rm = TRUE)),
                  by = USUBJID]

meas.79 <- meas.79.raw[, list(USUBJID, meas = (spiral.meas | ct.meas))]

study.79.raw <- merge(study.79.raw, meas.79, all.x = TRUE)

# lack of measurable disease is a FALSE
study.79.raw[is.na(meas), meas := FALSE]


# bone and visceral mets
met.79 <- s79.ls[VISIT == 'BASELINE',
                 list(bone = any(LSLOC == 'BONE'),
                      visc = any(LSLOC %in% unique(s79.ls$LSLOC)[-c(2,3,4,12)])),
                 by = USUBJID]

study.79.raw <- merge(study.79.raw, met.79)

# ########## PAIN MEASURE ##########
# 
# s79.pn <- read.csv('../../S79/PN.CSV')
# 
# pre.pain.79 <- s79.pn[s79.pn$VISITNUM == 1 & s79.pn$PNTESTCD=='PAININT',
#                       c('USUBJID', 'PNSTRESN', 'VISIT', 'PNDTC')]
# pre.pain.79$PNDTC <- as.Date(pre.pain.79$PNDTC)
# 
# PainFunc <- function(df) {
#    last.date <- max(df$PNDTC)
#    df <- df[df$PNDTC >= last.date - 7,]
#    return(data.frame(mean=mean(df$PNSTRESN)>=2,
#                      median=median(df$PNSTRESN)>=2))
# }
# 
# pain.79 <- ddply(pre.pain.79,
#                  .(USUBJID),
#                  PainFunc)


############# DATA PROCESSING ################

study.79 <- study.79.raw

# these pts had mitoxantrone plus prednisone
study.79$treatment <- 1

# exclude patients with ecog > 1
study.79 <- study.79[ecog <= 1]

print(dim(study.79))

saveRDS(object = study.79, file = 'study79.dt')
