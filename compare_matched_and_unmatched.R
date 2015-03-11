library(data.table)

the.match <- readRDS('caliper_match')

#### Parse the match object ####
length(the.match$weights)
length(the.match$treat)

ctrl <- the.match$model$data[the.match$treat == 1,]
ctrl$matched <- the.match$weights[the.match$treat == 1]

trt <- the.match$model$data[the.match$treat == 0,]
trt$matched <- the.match$weights[the.match$treat == 0]

#### Descriptive Stats ####

datasets <- list(ctrl, trt)

print(num.pts <- sapply(X = datasets, FUN = nrow))

print(med.dit <- sapply(X = datasets, FUN = function(x) { median(x$days.in.trial) } ))

print(mean.dit <- sapply(X = datasets, FUN = function(x) { mean(x$days.in.trial) } ))

print(sum.cens <- sapply(X = datasets, FUN = function(x) { sum(x$surv.censor) } ))

print(pct.cens <- sum.cens/num.pts)


#### Modeling ####
library(survival)

ctrl.cox.fit <- coxph(Surv(days.in.trial, !surv.censor) ~ matched,
                 data = ctrl)
summary(ctrl.cox.fit)


trt.cox.fit <- coxph(Surv(days.in.trial, !surv.censor) ~ matched,
                 data = trt)
summary(trt.cox.fit)


ctrl.cox.fit <- coxph(Surv(days.in.trial, !surv.censor) ~ matched +
                         age +
                         recent.doce + 
                         ecog + 
                         hgb + 
                         psa + 
                         meas,
                      data = ctrl)
summary(ctrl.cox.fit)


trt.cox.fit <- coxph(Surv(days.in.trial, !surv.censor) ~ matched +
                        age +
                        recent.doce + 
                        ecog + 
                        hgb + 
                        psa + 
                        meas,
                     data = trt)
summary(trt.cox.fit)
