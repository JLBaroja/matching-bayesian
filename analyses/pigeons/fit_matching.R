library('R2jags')
birds <- read.csv('data/raw/pigeons/pigeons_by_session.csv')
birds <- subset(birds,dynamic_session==F)
Bl <- birds$resp_left
Br <- birds$resp_right
Wl <- birds$reinf_left
Wr <- birds$reinf_right
Nresp <- Br+Bl
Nreinf <- Wr+Wl
bird = birds$bird_num
#unit = birds$unit
unit = NA
for(b in 1:length(unique(bird))){
	unit[bird==b] <- 1:sum(bird==b)
}
n_birds = length(unique(birds$bird))
n_obs = nrow(birds)
observed <- list(
	Br=Br,Bl=Bl,Wr=Wr,Wl=Wl,
	Nresp=Nresp,Nreinf=Nreinf,
	bird=bird,
	unit=unit,
	n_birds=n_birds,n_obs=n_obs)
unobserved <- c(
	'alpha','beta','lambda','kappa',
	'theta','phi',
	'mu_phi','nu_phi',
	'theta_pred','phi_pred',
	'Bl_pred','Br_pred',
	'Wl_pred','Wr_pred',
	'Nresp_pred','Nreinf_pred'
)	

set.seed(123)
inits <- lapply(1:3, function(chain) {
  list(
    .RNG.name = "base::Mersenne-Twister",
    .RNG.seed = 123 + chain
  )
})
bayes_bird <- jags(
	data=observed,
	parameters.to.save=unobserved,
	inits=inits,
	n.chains=3,n.iter=25000,n.burnin=10000,n.thin=15,
	model.file='models/pigeons/matching.bug')
saveRDS(bayes_bird,file='results/models/pigeons_matching.rds')
print(n_obs)
