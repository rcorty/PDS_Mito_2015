N <- 200

shape1 = 479
shape2 = 623
scale = 1.5

ctrl.med <- shape1 * log(2)^(1/scale)
trt.med <- shape2 * log(2)^(1/scale)

ps <- rep(NA, 1000)

for (sim.num in 1:1000) {
   sim.data <- data.frame(time = c(shape1 * ( -log( 1-runif(N) ) ) ^(1/scale),
                                   shape2 * ( -log( 1-runif(N) ) ) ^(1/scale)),
                          event = runif(n = 2*N) < 0.7,
                          treat = rep(c(FALSE, TRUE), each = N))
   
   sim.surv <- Surv(time = sim.data$time,
                    event = sim.data$event)
   
   surv.fit.simple <- survreg(sim.surv ~ as.factor(treat),
                              dist = 'weibull',
                              data = sim.data)
   
   ps[sim.num] <- summary(surv.fit.simple)$table[2,4]
}

sum(ps < 0.05) / 1000
