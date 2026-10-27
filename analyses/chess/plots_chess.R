source('R/plotting_functions.R')
chess <- read.csv('data/raw/chess/queens_gambit_black.csv')
Br <- chess$n_QGA
Bl <- chess$n_QGD
Wr <- chess$n_wins_QGA
Wl <- chess$n_wins_QGD
bayes_chess <- readRDS('results/models/chess_matching.rds')
print(summary(bayes_chess$BUGSoutput$summary[,c('Rhat','n.eff')]))
print(head(sort(bayes_chess$BUGSoutput$summary[,'Rhat'],decreasing=T)))
print(head(sort(bayes_chess$BUGSoutput$summary[,'n.eff'],decreasing=F)))
nds <- bayes_chess$BUGSoutput$sims.list

sd_plot <- function(post,prior,at=0,res=80,lims=c(-1.5,1.5),lab=NULL){
	lims <- c(at-.75,at+.75)
	if(is.null(lims)){
		lims <- c(min(c(post,prior)),max(c(post,prior)))
	}
	brks <- seq(lims[1],lims[2],length.out=res)
	ht_post <- density(post,n=res)
	ht_prior <- density(prior,n=res)
	at_prior <- approx(ht_prior$x,ht_prior$y,xout=at,rule=2)$y
	at_post <- approx(ht_post$x,ht_post$y,xout=at,rule=2)$y
	BF <- at_post/at_prior
	bf_lab <- MASS::as.fractions(round(BF,1))
	plot(NULL,xlim=c(lims),ylim=c(0,1.1*max(ht_post$y,ht_prior$y)),
		axes=F,ann=F)
	axis(1,padj=-0.5);box()
	lines(ht_prior,lwd=2,col='#aaaaaa')
	lines(ht_post,lwd=2,col='#000000')
	lines(c(at,at),c(at_prior,at_post),
				cex=1.25,lwd=1.5,pch=21,bg=c('#aaaaaa','#ffffff'),type='o')
	if(!is.null(lab)){
		mtext(lab,1,line=1.75)
	}
	text(at-0.5,max(ht_post$y,ht_prior$y)*.7,bquote(BF['01']%~~%.(bf_lab)))
}
#sd_plot(nds$b0_alpha_post,nds$b0_alpha_prior)

pdf(file='results/figures/chess_1.pdf',width=4,height=4)
	par(mar=c(2.5,2.5,1,1),oma=c(0,0,0,0),
		tck=-0.02,mgp=c(3,0.5,0),cex.axis=.9)
	W_ratio_pred <- log(nds$Wl_pred/nds$Wr_pred)
	B_ratio_pred <- log(nds$Bl_pred/nds$Br_pred)
	alpha_post <- nds$b0_alpha_post
	beta_post <- nds$b0_beta_post
	plot(NULL,xlim=c(-4,4),ylim=c(-4,4))
	points(W_ratio_pred,B_ratio_pred,col='#99999935',pch=16,cex=.7)
	x <- W_ratio_pred
	y <- B_ratio_pred
	x_brks <- seq(-4,4,length.out=32)
	y_low <- y_high <- x_mids <- NA
	for(xb in 1:(length(x_brks)-1)){
		qntl <- quantile(y[x>=x_brks[xb]&x<x_brks[xb+1]&is.finite(y)],probs=c(0.025,0.975),
			na.rm=T)
		y_low[xb] <- qntl[1]; y_high[xb] <- qntl[2]
		x_mids[xb] <-x_brks[xb]+(x_brks[xb+1]-x_brks[xb])/2
	}
	lines(x_mids,y_high,lwd=2,col='#888888')
	lines(x_mids,y_low,lwd=2,col='#888888')
	#single_contour(log(Wl/Wr), log(Bl/Br))
	# Fitted matching
	smpl <- sample(dim(alpha_post)[1],size=100)
	#abline(mean(alpha_post),mean(beta_post),lty='solid',col='#0000dd',lwd=1.25)
	for(i in 1:length(smpl)){
		abline(alpha_post[i],beta_post[i],lty='solid',col='#88888830',lwd=0.5)
	}
	# Data
	points(log(Wl/Wr), log(Bl/Br),pch=16,cex=0.5)
	# Strict Matching
	abline(0,1,lty='42',col='#dd0000',lwd=1.25)
	#text(-3,3,paste('Bird',tm),font=2,adj=c(0,1))
		legend('bottomright',legend=c('Data','Posterior Prediction'),pch=c(16,22),
			pt.cex=.8,pt.bg=c(NA,'#cccccc'),col=c('black','#888888'),
			x.intersp=.7,cex=.7,box.lty='blank')
	mtext(expression(log(W[declined]/W[accepted])),1,line=1.5,outer=F,cex=0.8)
	mtext(expression(log(B[declined]/B[accepted])),2,line=1.25,outer=F,cex=0.8)
dev.off()

pdf(file='results/figures/chess_2.pdf',width=5.5,height=4)
layout(matrix(1:4,ncol=2,byrow=T))
par(mar=c(3,0.5,1,1),oma=c(0,0,0,0),
		tck=-0.03,mgp=c(3,0.5,0),cex.axis=.9)
sd_plot(nds$b0_alpha_post,nds$b0_alpha_prior,lab=expression(b[0][alpha]))
sd_plot(nds$b0_beta_post,nds$b0_beta_prior,lab=expression(b[0][beta]),at=1)
sd_plot(nds$b1_alpha_post,nds$b1_alpha_prior,lab=expression(b[1][alpha]))
sd_plot(nds$b1_beta_post,nds$b1_beta_prior,lab=expression(b[1][beta]))
dev.off()

#pdf(file='pigeons_2.pdf',width=4,height=4)
#layout(matrix(c(2,0,1,3),byrow=T,ncol=2),widths=c(3,1),heights=c(1,3))
#par(mar=rep(0.25,4),oma=c(4,4,1,1),mgp=c(3,.5,0),tck=-.02)
#lims_bias <- c(-.1,.3)
#lims_sensi <- c(0.45,1.55)
## Joint
#plot(NULL,xlim=lims_bias,ylim=lims_sensi)
#dev.off()
#pdf(file='chess_2.pdf',width=5.5,height=4)
#layout(matrix(1:4,ncol=2,byrow=T))
#par(mar=c(3,0.5,1,1),oma=c(0,0,0,0),
#		tck=-0.03,mgp=c(3,0.5,0),cex.axis=.9)
#sd_plot(nds$b0_alpha_post,nds$b0_alpha_prior,lab=expression(b[0]^alpha))
#sd_plot(nds$b0_beta_post,nds$b0_beta_prior,lab=expression(b[0]^beta))
#sd_plot(nds$b1_alpha_post,nds$b1_alpha_prior,lab=expression(b[1]^alpha))
#sd_plot(nds$b1_beta_post,nds$b1_beta_prior,lab=expression(b[1]^beta))
#dev.off()
