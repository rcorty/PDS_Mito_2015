library(CortyKit)

s79.tox <- data.table(read.csv('../../S79/AE.CSV'),
                      key = 'USUBJID')
s81.tox <- data.table(read.csv('../../S81/ADVERSE.CSV'),
                      key = 'PID_A')

data.after.psm <- readRDS('caliper_matched_data')
s79.IDs <- data.after.psm[treatment == 1, USUBJID]
s81.IDs <- data.after.psm[treatment == 0, USUBJID]


# subset tox data to patients who matched
s79.psm.tox <- s79.tox[USUBJID %in% s79.IDs,]

# demonstrate that one individual can have the same tox many times
s79.all.indiv.tox <- s79.psm.tox[, list(.N, visit = paste(VISIT, collapse = ' ')), by = list(AEDECOD, USUBJID)]
write.table(s79.all.indiv.tox, file= 'mitopred_all_tox_by_indiv.txt', row.names = FALSE)

s79.psm.tox[USUBJID == '006193-038-002-401' & AEDECOD == 'DYSPNOEA', ]

# we want to consider only the first time each patient has a given toxicity
s79.first.this.tox.of.pt <- s79.psm.tox[,.SD[1,], by = list(AEDECOD, USUBJID)]

# confirm that each pt only has each tox once
all(s79.first.this.tox.of.pt[, list(.N, visit = paste(VISIT, collapse = ' ')), by = list(AEDECOD, USUBJID)]$N == 1)

# see if any of the first occurences were after day 875 (cutoff)
# data not available
# also this study had the shorter max-surviving patient, so don't need to subset this one

# subset toxicities in clinically meaningful ways
s79.all.tox <- s79.first.this.tox.of.pt[, .N, by = AEDECOD]
write.table(s79.all.tox[order(-N)], file = 'mitopred_all_tox.txt', row.names = FALSE)

s79.g2.tox <- s79.first.this.tox.of.pt[AETOXGR == 2, .N, by = AEDECOD]
write.table(s79.g2.tox[order(-N)], file = 'mitopred_g2_tox.txt', row.names = FALSE)

s79.g3p.tox <- s79.first.this.tox.of.pt[AETOXGR >= 3, .N, by = AEDECOD]
write.table(s79.g3p.tox[order(-N)], file = 'mitopred_g3p_tox.txt', row.names = FALSE)

# subsets of toxicities that we decided aren't important
# s79.g3.tox <- s79.psm.tox[AETOXGR == 3, .N, by = AEDECOD]
# write.table(s79.g3.tox[order(-N)], file = 'mitopred_g3_tox.txt')
# 
# s79.g4.tox <- s79.psm.tox[AETOXGR == 4, .N, by = AEDECOD]
# write.table(s79.g4.tox[order(-N)], file = 'mitopred_g4_tox.txt')
# 
# s79.g5.tox <- s79.psm.tox[AETOXGR == 5, .N, by = AEDECOD]
# write.table(s79.g5.tox[order(-N)], file = 'mitopred_g5_tox.txt')


# subset tox data to patients who matched
s81.psm.tox <- s81.tox[PID_A %in% s81.IDs,]

# subset tox data to only the first of each tox for each pt
s81.first.this.tox.of.pt <- s81.psm.tox[, .SD[1,], by = list(PREFTEXT, PID_A)]

# confirm that each pt only has each tox once
all(s81.first.this.tox.of.pt[, .N, by = list(PREFTEXT, PID_A)]$N == 1)

# check if any first occurences of toxicity were after study79 closed (875 days)
str(s81.first.this.tox.of.pt)
s81.first.this.tox.of.pt
sum(s81.first.this.tox.of.pt$AEFDAY > 875, na.rm = TRUE)
sum(is.na(s81.first.this.tox.of.pt$AEFDAY))

# remove the one tox that started after day 875
s81.first.this.tox.of.pt <- s81.first.this.tox.of.pt[-which(s81.first.this.tox.of.pt$AEFDAY > 875),]

# subset tox data in clinically meaningful ways
s81.all.tox <- s81.first.this.tox.of.pt[, .N, by = PREFTEXT]
write.table(s81.all.tox[order(-N)], file = 'pred_all_tox.txt', row.names = FALSE)

s81.g2.tox <- s81.first.this.tox.of.pt[AEGRADE == 2, .N, by = PREFTEXT]
write.table(s81.g2.tox[order(-N)], file = 'pred_g2_tox.txt', row.names = FALSE)

s81.g3p.tox <- s81.first.this.tox.of.pt[AEGRADE >= 3, .N, by = PREFTEXT]
write.table(s81.g3p.tox[order(-N)], file = 'pred_g3p_tox.txt', row.names = FALSE)

# subsets of toxicites that we decided aren't meaningful
# s81.g3.tox <- s81.psm.tox[AEGRADE == 3, .N, by = PREFTEXT]
# write.table(s81.g3.tox[order(-N)], file = 'pred_g3_tox.txt')
# 
# s81.g4.tox <- s81.psm.tox[AEGRADE == 4, .N, by = PREFTEXT]
# write.table(s81.g4.tox[order(-N)], file = 'pred_g4_tox.txt')
# 
# s81.g5.tox <- s81.psm.tox[AEGRADE == 5, .N, by = PREFTEXT]
# write.table(s81.g5.tox[order(-N)], file = 'pred_g5_tox.txt')
