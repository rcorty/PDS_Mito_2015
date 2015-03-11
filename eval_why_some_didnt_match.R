#evaluate why the non-matched patients didn't match

the.match <- readRDS('caliper_match')

match.rgsn.ctrl <- glm(the.match$weights == 0 ~ age +
                          recent.doce + 
                          ecog + 
                          hgb + 
                          psa + 
                          meas,
                       data = the.match$model$data,
                       subset = treatment == 0,
                       family='binomial')
summary(match.rgsn.ctrl)


match.rgsn.trt <- glm(the.match$weights == 0 ~ age +
                         recent.doce + 
                         ecog + 
                         hgb + 
                         psa + 
                         meas,
                       data = the.match$model$data,
                       subset = treatment==1,
                       family = 'binomial')
summary(match.rgsn.trt)
