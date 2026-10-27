library('R2jags')
chess <- read.csv('data/raw/chess/queens_gambit_black.csv')
Br <- chess$n_QGA
Bl <- chess$n_QGD
Wr <- chess$n_wins_QGA
Wl <- chess$n_wins_QGD
Nresp <- chess$total_QG
Nreinf <- Wr+Wl
elo <- chess$avg_elo
elo <- as.numeric(scale(elo,center=T,scale=T))
n_obs <- nrow(chess)
observed <- list(
	Br=Br,Bl=Bl,Wr=Wr,Wl=Wl,
	Nresp=Nresp,Nreinf=Nreinf,
	#elo=chess$avg_elo,
	elo=elo,
	n_obs=n_obs)
unobserved <- c(
	'alpha','beta','lambda','kappa',
	'phi','theta',
	'mu_phi','nu_phi',
	'Bl_pred','Br_pred',
	'Wl_pred','Wr_pred',
	'b0_alpha_post','b1_alpha_post',
	'b0_beta_post','b1_beta_post',
	'b0_alpha_prior','b1_alpha_prior',
	'b0_beta_prior','b1_beta_prior',
	'mu_alpha','sd_alpha',
	'mu_beta','sd_beta'
)

set.seed(123)

bayes_chess <- jags(
    data = observed,
    parameters.to.save = unobserved,
    n.iter = 10000,
    n.burnin = 4000,
    n.thin = 6,
    model.file = 'models/chess/matching.bug'
)

saveRDS(bayes_chess, file = 'results/models/chess_matching.rds')

print(n_obs)
