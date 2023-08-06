# Fit a Markov-switching GAM to a training set and 
# evaluate it on a training set. 
# The datasets should have one row per watershed and week,
# sorted first by watersheds and then by week.
#
# The datasets are assumed to have 
# a column ``B'' for the binary outcome,
# a column ``ID'' with the names of the watersheds, and
# columns ``Rain'' and ``NDVI'' with the time-dependent covariates.
# Additional, static covariates can be added to the formula ``ft''

library(hmmTMB)

### train the model on a dataset ``train''
fe <- list(B = list(prob = ~ s(Rain, bs = "cs", k = 3) +
                              s(NDVI, bs = "cs", k = 3)))
ft <- ~ s(ID, bs="mrf", k=5, xt=list(nb=nb))
hid1 <- MarkovChain$new(data = train, n_states = 2, formula = ft) 
dists <- list(B = "binom")
pars <- list(B = list(size = c(1,1), prob = c(0.003, 0.1)))
obs1 <- Observation$new(data = train, n_states = 2,
                        dists = dists, par = pars,
                        formulas = fe)
fixpar <- list(obs = c("B.size.state1.(Intercept)" = NA,
                       "B.size.state2.(Intercept)" = NA)) 
hmm1 <- HMM$new(obs = obs1, hid = hid1, fixpar = fixpar)
hmm1$setup()
hmm1$fit()

### test the model on a dataset ``test''
hid2 <- MarkovChain$new(data = test, n_states = 2,
                        formula = ft)
obs2 <- Observation$new(data = test, n_states = 2,
                        dists = dists, par = pars, formulas = fe)
hmm2 <- HMM$new(obs = obs3, hid = hid3, init = hmm1)

### get the predictions on on the test dataset
emi <- hmm2$predict("obspar")
emi[2,, ]
sta <- hmm2$state_probs()
pre <- rep(0, nrow(sta))
for(i in 1:length(pre))
  pre[i] <- sum(emi[2,,] * sta[i,])