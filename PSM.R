library(CortyKit)
library(MatchIt)

#### INPUT AND PREP DATA ####
study.79 <- readRDS('study79.dt')
study.81 <- readRDS('study81.dt')

# rename pt identifier to be consistent w study 79
setnames(study.81, names(study.81)[1], 'USUBJID')

mito.data <- rbind(study.81, study.79, use.names = TRUE)
setkey(mito.data, USUBJID)
mito.data$race <- factor(mito.data$race)

saveRDS(object = mito.data, file = 'mito_data_before_PSM')

# #### GET BEFORE PSM P VALUES ####
# predictors <- c('age', 'recent.doce', 'ecog', 'hgb', 'psa', 'meas')
# before.p <- vector(length = 6)
# before.control.mean <- vector(length = 6)
# before.treatment.mean <- vector(length = 6)
# before.control.median <- vector(length = 6)
# before.treatment.median <- vector(length = 6)
# for (i in 1:6) {
#    
#    t.test <- mito.data[, t.test(get(predictors[i]) ~ treatment)]
#    before.control.mean[i] <- mean(mito.data[[predictors[i]]][mito.data$treatment == 0])
#    before.treatment.mean[i] <- mean(mito.data[[predictors[i]]][mito.data$treatment == 1])
#    before.control.median[i] <- median(mito.data[[predictors[i]]][mito.data$treatment == 0])
#    before.treatment.median[i] <- median(mito.data[[predictors[i]]][mito.data$treatment == 1])
#    before.p[i] <- t.test$p.value
# }
# 
# 
# test.fit <- glm(treatment ~ age +
#                    recent.doce + 
#                    ecog + 
#                    hgb + 
#                    psa + 
#                    meas,
#                 data = mito.data,
#                 family = 'binomial')
# summary(test.fit)

#### DETERMINE DISTANCE ####
match.out <- matchit(!treatment ~  age +
                        recent.doce + 
                        ecog + 
                        hgb + 
                        psa + 
                        meas,
                     data = mito.data,
                     method = 'nearest',
                     distance = 'logit',
                     discard = 'none',
                     ratio = 1)

print(match.out)
saveRDS(object = match.out, file = 'distance_match')

matched.sd <- sd(match.data(match.out)$distance)
print(matched.sd)

#### DO MATCH ####
caliper.out.25 <- matchit(!treatment ~ age +
                             recent.doce + 
                             ecog + 
                             hgb + 
                             psa + 
                             meas,
                     data = mito.data,
                     method = 'nearest',
                     distance = 'logit',
                     discard = 'none',
                     ratio = 1,
                     caliper = 0.25 * matched.sd)

print(caliper.out.25)
saveRDS(object = caliper.out.25, file = 'caliper_match')

plot(caliper.out.25, type='jitter', interactive = FALSE)
plot(caliper.out.25, type='hist')
matched.mito.data.25 <- data.table(match.data(caliper.out.25),
                                   key = 'USUBJID')

saveRDS(object = matched.mito.data.25, file = 'caliper_matched_data')

# #### GET AFTER P VALUES ####
# after.p <- vector(length=6)
# after.control.mean <- vector(length=6)
# after.treatment.mean <- vector(length=6)
# after.control.median <- vector(length=6)
# after.treatment.median <- vector(length=6)
# after.control.lower <- vector(length = 6)
# after.treatment.lower <- vector(length = 6)
# after.control.upper <- vector(length = 6)
# 
# for (i in 1:6) {
# 
#    t.test <- matched.mito.data.25[, t.test(get(predictors[i]) ~ treatment)]
#    after.control.mean[i] <- mean(matched.mito.data.25[[predictors[i]]][mito.data$treatment == 0])
#    after.treatment.mean[i] <- mean(matched.mito.data.25[[predictors[i]]][mito.data$treatment == 1])
#    after.control.median[i] <- median(matched.mito.data.25[[predictors[i]]][mito.data$treatment == 0])
#    after.treatment.median[i] <- median(matched.mito.data.25[[predictors[i]]][mito.data$treatment == 1])
#    after.p[i] <- t.test$p.value
# }
# 
# 
# post.fit <- glm(treatment ~ age +
#                    recent.doce + 
#                    ecog + 
#                    hgb + 
#                    psa + 
#                    meas,
#                 data = matched.mito.data.25,
#                 family = 'binomial')
# summary(post.fit)

# #### VIEW BEFORE AND AFTER P VALUES ####
# before <- data.frame(control.mean = before.control.mean, 
#                      trt.mean = before.treatment.mean,
#                      control.med = before.control.median,
#                      trt.med = before.control.median,
#                      before.p)
# row.names(before) <- predictors
# after <- data.frame(control.mean = after.control.mean,
#                     trt.mean = after.treatment.mean,
#                     control.med = after.control.median,
#                     trt.med = after.control.median,
#                     after.p)
# row.names(after) <- row.names(before)
# 
# #xtable(before, caption='covariates before propensity score matching')
# #xtable(after, caption='covariates after propensity score matching')
# 
# #xtable(cbind(before, after))
# 
# par(mfrow = c(1,1))
# plot(before.p,
#      after.p,
#      col='red',
#      xlim=c(0,1),
#      ylim=c(0,1))
# abline(0, 1, lty=2, col='red')
