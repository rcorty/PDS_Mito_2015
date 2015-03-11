library(survival)
options(digits = 2)
matched.mito.data <- readRDS(file = 'caliper_matched_data')


# test proportional hazards assumption
cox.fit <- coxph(Surv(days.in.trial, !surv.censor) ~ age +
                    recent.doce + 
                    ecog + 
                    hgb + 
                    psa + 
                    meas,
                 data = matched.mito.data)
summary(cox.fit)
print(cox.zph(cox.fit))
plot(cox.zph(cox.fit))


############## make Surv objects ##################
mito.surv <- Surv(time = matched.mito.data$days.in.trial,
                  event = !matched.mito.data$surv.censor)


############# KM plots ####################
mito.pred.survfit <- survfit(mito.surv ~ treatment,
                             data = matched.mito.data)
plot(mito.pred.survfit,
     col = c('black', 'red'),
     ylab = 'Percent Survival',
     xlab = 'days')
legend(legend = c('pred', 'mito-pred'),
       fill = c('black', 'red'),
       x = 20, y = 0.3)
title('Kaplan-Meier Plot of Survival')



################# accelerated failure time models #####################
surv.fit.simple <- survreg(mito.surv ~ as.factor(treatment),
                           dist = 'weibull',
                           data = matched.mito.data)
summary(surv.fit.simple)
trt <- surv.fit.simple$coef[2]
sd <- sqrt(surv.fit.simple$var[2,2])
print(paste('mle:', round(exp(trt), 2)))
print(paste('ci:', round(exp(trt + 2*c(-sd, sd)), 2)))

# tox.g2p.fit.simple <- survreg(mito.toxg2p ~ treatment,
#                               data = matched.mito.data)
# summary(tox.g2p.fit.simple)
# 
# tox.g3p.fit.simple <- survreg(mito.toxg3p ~ treatment,
#                               data = matched.mito.data)
# summary(tox.g3p.fit.simple)

surv.fit.multiple <- survreg(mito.surv ~ treatment + age + recent.doce  + ecog + hgb + psa + meas,
                             dist = 'weibull',
                             data = matched.mito.data)
summary(surv.fit.multiple)
trt <- surv.fit.multiple$coef[2]
print('expected val')
print(exp(trt))
sd <- sqrt(surv.fit.multiple$var[2,2])
print('ci')
exp(trt + 2*c(-sd, sd))

tox.g2p.fit.multiple <- survreg(mito.toxg2p ~ treatment + recent.doce  + ecog + hgb + psa + meas,
                             data=matched.mito.data)
summary(tox.g2p.fit.multiple)

tox.g3p.fit.multiple <- survreg(mito.toxg3p ~ treatment + recent.doce  + ecog + hgb + psa + meas,
                             data=matched.mito.data)
summary(tox.g3p.fit.multiple)

##################### cox models #########################
cox.fit <- coxph(Surv(days.in.trial, !surv.censor) ~ treatment,
                 data = matched.mito.data)

cox.fit <- coxph(Surv(days.in.trial, !surv.censor) ~ treatment +
                    age +
                    recent.doce + 
                    ecog + 
                    hgb + 
                    psa + 
                    meas,
                 data = matched.mito.data)
exp(coef(cox.fit))
exp(confint(cox.fit))
