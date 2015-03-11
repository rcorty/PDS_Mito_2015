library(survival)

load('study79.df')
load('study81.df')

surv.79 <- Surv(time=study.79$time.in.trial,
                event=!study.79$censor,
                type='right')

surv.fit.79 <- coxph(surv.79 ~ days_horm_use + doce + ecog +
                        hgb + psa + alkp + meas,
                     data=study.79)
summary(surv.fit.79)

# doce, ecog, hgb are significant, meas is marginal


surv.81 <- Surv(time=study.81$time.in.trial,
                event=!study.81$censor,
                type='right')

surv.fit.81 <- coxph(surv.81 ~ days_horm_use + doce + ecog +
                        hgb + psa + alkp + meas,
                     data=study.81)
summary(surv.fit.81)
# doce, ecog, and hgb are significant, alkp and meas are marginal
