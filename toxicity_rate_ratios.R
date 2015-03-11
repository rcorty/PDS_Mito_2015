library(CortyKit)

matched.mito.data <- readRDS(file = 'caliper_matched_data')


matched.mito.data[treatment == 0, mean(days.in.trial)]
matched.mito.data[treatment == 1, mean(days.in.trial)]

tox.g3p.fit <- glm(formula = num.g3p ~ treatment, 
                   offset = log(days.in.trial),
                   family = 'poisson', 
                   data = matched.mito.data)
summary(tox.g3p.fit)
round(exp(confint(tox.g3p.fit)[2,]), 2)

tox.g4p.fit <- glm(formula = num.g4p ~ treatment, 
                   offset = log(days.in.trial),
                   family = 'poisson', 
                   data = matched.mito.data)
summary(tox.g4p.fit)
round(exp(confint(tox.g4p.fit)[2,]), 2)

tox.g5p.fit <- glm(formula = num.g5p ~ treatment, 
                   offset = log(days.in.trial),
                   family = 'poisson', 
                   data = matched.mito.data)
summary(tox.g5p.fit)
round(exp(confint(tox.g5p.fit)[2,]), 2)
