to_hex <- function(int,digits=2){
    str <- as.character(as.hexmode(int))
    if(nchar(str)<digits){
        str <- paste('0',str,sep='')
    }
    return(str)
}

color_ch <- function(hex,pctg=0,alph=1){
	r <- strtoi(paste('0x',substr(hex,2,3),sep=''))
	g <- strtoi(paste('0x',substr(hex,4,5),sep=''))
	b <- strtoi(paste('0x',substr(hex,6,7),sep=''))
	r <- r*(1+pctg)*(pctg<0)+(r+(255-r)*pctg)*(pctg>0)+r*(pctg==0)
	g <- g*(1+pctg)*(pctg<0)+(g+(255-g)*pctg)*(pctg>0)+g*(pctg==0)
	b <- b*(1+pctg)*(pctg<0)+(b+(255-b)*pctg)*(pctg>0)+b*(pctg==0)
	new_color <- rgb(r,g,b,maxColorValue=255,alpha=alph*255)
	return(new_color)
}

nice_scatter <- function(x,y,
                        color='#0044ee',
                        xlimz=NULL,
                        ylimz=NULL,
                        n_mids=40,
                        new_plot=T,
                        style='polygon',
                        point_scale=4,
                        raw_data=F,
												...){
		color <- substr(color,1,7)
    if(is.null(xlimz)){
        xlimz <- c(min(x),max(x))}
    if(is.null(ylimz)){
        ylimz <- c(min(y),max(y))}
  #  n_mids <- 30
    
    if(new_plot){
        plot(NULL,xlim=xlimz,ylim=ylimz,ann=F,...)
        if(raw_data){
            points(x,y,col='#cccccc')}
        }
    
    count_grid <- array(dim=c(n_mids,n_mids))
    x_brks <- seq(xlimz[1],xlimz[2],length.out = n_mids+1)
    y_brks <- seq(ylimz[1],ylimz[2],length.out = n_mids+1)
    x_width <- x_brks[2]-x_brks[1]
    y_width <- y_brks[2]-y_brks[1]
    x_mids <- x_brks[1:(length(x_brks)-1)]+x_width/2
    y_mids <- y_brks[1:(length(y_brks)-1)]+y_width/2
    for(i in 1:n_mids){
        for(j in 1:n_mids){
            # Counting
            count_grid[i,j] <- sum(x>=x_brks[i]
                                   &x<x_brks[(i+1)]
                                   &y>=y_brks[j]
                                   &y<y_brks[(j+1)])
        }
    }
    for(i in 1:n_mids){
        for(j in 1:n_mids){
            # Plotting
            alpha <- round(count_grid[i,j]*(255/max(count_grid)))
            # Squares of different sizes
            if(style=='points'){
                points(x_mids[i],y_mids[j],cex=point_scale*count_grid[i,j]/max(count_grid),
                       col=color,
                       pch=22,bg=paste(color,to_hex(alpha),sep=''))}
            # Polygons of different shades
            if(style=='polygon'){
                polygon(x=c(x_mids[i]+rep(c(-1,1),each=2)*x_width/2),
                        y=c(y_mids[j]+c(-1,1,1,-1)*y_width/2),
                        border=paste(color,to_hex(alpha),sep=''),
                        #border=NA,
                        col=paste(color,to_hex(alpha),sep=''))}
        }
    }
}

bird_postpred <- function(brd,dset,nds){
	b_dta <- subset(dset,bird_num==brd)
	idx <- sample(dim(nds$log_W_pred)[1],size=100)
	log_W <- nds$log_W_pred[idx,]
	log_B <- nds$log_B_pred[idx,brd,]
	Bl <- b_dta$resp_left
	Br <- b_dta$resp_right
	Wl <- b_dta$reinf_left
	Wr <- b_dta$reinf_right
	plot(log_W,log_B,pch=16,col='#00000013')
	points(log(Wl/Wr),log(Bl/Br),pch=21,bg='#ffffff',cex=1.5)
}

new_traceplot <- function(bys,node_name){
  # Grafica cada cadena del nodo 'node_name' dentro del
  # objeto 'bys' que contiene el resultado de JAGS.
  # El nombre del nodo 'node_name' permite indexación, 
  # es decir que permite graficar una sola posición de nodos 
  # multidimensionales.
  
  # Las cadenas (y sus dimensiones) están en la partición
  # 'bys$BUGSoutput$sims.array',
  # en donde 'bys' es la lista en R que guarda el resultado
  # que genera la función 'jags()'.
  n_iter <- dim(bys$BUGSoutput$sims.array)[1] # Primera dimensión *siempre* tiene iteraciones
  n_chains <- dim(bys$BUGSoutput$sims.array)[2] # Segunda *siempre* tiene número de cadenas
  iterations <- bys$BUGSoutput$sims.array[,,node_name] # 'node_name' es una cadena que incluye índice
  # e.g., 'node_name' puede ser 'theta_post[2]', 'a_post[9]', o 'k_prior[45,12,1]' si dicho nodo tuviera
  # tres dimensiones...
  
  # Extrayendo información adicional (p.s., ¡todo está en algún lado de 'bys'!):  
  r_hat <- round(bys$BUGSoutput$summary[node_name,'Rhat'],3) # Rhat
  n_eff <- bys$BUGSoutput$summary[node_name,'n.eff'] # n.eff
  model_file <- bys$model.file # Nombre del archivo de texto que contiene el modelo
  n_burnin <- bys$BUGSoutput$n.burnin
  n_thin <- bys$BUGSoutput$n.thin
  n_iter_tot <- bys$BUGSoutput$n.iter  
    
  paleta <- c('#fd2c3b','#6314af','#fe8f06','#88dc40','#21f0b6','#2379cc','#e1d936')
  cols <- sample(x = paleta,size = n_chains) # Colores aleatorios cada que corre la función
  ylims <- c(min(iterations),max(iterations))
  par(bg='#000000',fg='#aaaaaa',col.axis='#aaaaaa',oma=c(1,1,1,1))    
  plot(NULL,xlim=c(1,n_iter),ylim=ylims,axes=F,ann=F)
  for(cc in 1:n_chains){
    lines(1:n_iter,iterations[,cc],lwd=2,
          col=cols[cc])
  }
  axis(1)
  mtext('iterations',1,line=2.5,cex=1.5)
  axis(2,cex.axis=1.5)
  mtext('node',2,line=2.5,cex=1.5)    
  # 'paste()' permite crear una sola cadena de caracteres pegando varias cadenas
  # definidas en objetos diferentes:
  mtext(paste(node_name,
              ', R.hat=',r_hat,
              ', n.eff=',n_eff,
              '\nn_chains=',n_chains,
              ', n_iter=',n_iter_tot,
              ', burnin=',n_burnin,
              ', n_thin=',n_thin,
              sep=''),cex=1)
}

single_contour <- function(xplt,yplt,...){
	ellipse <- MASS::kde2d(xplt,yplt,n=50)
	contour(ellipse,levels=max(ellipse$z)*.3,drawlabels=F,add=T,...)
}

plot_margin <- function(node,colors=colors_elo,xlimz=NULL,...){
	if(is.null(xlimz)){
		xlimz <- c(min(node),max(node))
	}
	brks <- seq(min(node),max(node),length.out=150)
	maxim <- 0
	for(b in 1:dim(node)[2]){
		ht <- hist(node[,b],breaks=brks,plot=F)
		if(max(ht$density)>maxim) maxim <- max(ht$density)
	}
	plot(NULL,xlim=xlimz,ylim=c(0,maxim*1.1))
	for(b in 1:dim(node)[2]){
		ht <- hist(node[,b],breaks=brks,plot=F)
		polygon(
			x=c(head(ht$mids,1),ht$mids,tail(ht$mids,1)),
			y=c(0,ht$density,0),col=color_ch(colors[b],alph=0.2),
			border=F)
		lines(ht$mids,ht$density,col=colors[b],lwd=2)
	}
}

plot_data <- function(data=T,lines=F,pred=F,colors=colors_elo,nds=NULL){
Wl <- bayes_nfl$data$Wl
Wr <- bayes_nfl$data$Wr
Bl <- bayes_nfl$data$Bl
Br <- bayes_nfl$data$Br
group <- bayes_nfl$data$team
layout(matrix(1:length(unique(group)),ncol=4,byrow=T))
par(mar=rep(2,4))
	for(b in 1:length(unique(group))){
		plot(NULL,
			xlim=c(-1,3),ylim=c(-1,3))
		if(lines){
			indx <- sample(dim(nds$beta)[1],size=round(dim(nds$beta)[1])/2)
			for(i in indx){
				abline(nds$alpha[i,b],nds$beta[i,b],
					col=color_ch(colors[b],pctg=.15))
			}
		}
		if(pred){
			indx <- sample(dim(nds$beta)[1],size=round(dim(nds$beta)[1])/2)
			Wr_pred <- nds$Wr_pred[indx,b]
			Wl_pred <- nds$Wl_pred[indx,b]
			Br_pred <- nds$Br_pred[indx,b]
			Bl_pred <- nds$Bl_pred[indx,b]
			log_W <- log(Wl_pred/Wr_pred)
			log_B <- log(Bl_pred/Br_pred)
			valid_positions <- which(abs(log_W)!=Inf&abs(log_B)!=Inf)
			log_W <- log_W[valid_positions]
			log_B <- log_B[valid_positions]
				points(log_W,log_B,col=color_ch(colors[b],pctg=0.5),pch=16,cex=0.75)
		 # 	single_contour(log_W,log_B,lwd=2,col=colors[b])
			}
		if(data){
			points(
				x=log(Wl[group==b]/Wr[group==b]),y=log(Bl[group==b]/Br[group==b]),
				#bg=color_ch(colors[b],pctg=0.5),
				bg=color_ch('#ffffff',alph=0.8),
				col=color_ch(colors[b],pctg=-.25),
				pch=21,cex=1.3,lwd=1.5
			)
		}
		abline(0,1,lty='33')
		abline(h=0,lty='66')
	}
}
scatter_quantile <- function(x,y,
														 x_start=min(x[is.finite(x)]),
														 x_end=max(x[is.finite(x)]),n_bins=41){
	x_brks <- seq(x_start,x_end,length.out=n_bins)
	y_low <- y_high <- x_mids <- NA
	for(xb in 1:(length(x_brks)-1)){
		qntl <- quantile(x[x>=x_brks[xb]&x<x_brks[xb+1]],probs=c(0.025,0.975))
		y_low[xb] <- qntl[1]; y_high[xb] <- qntl[2]
		x_mids[xb] <-x_brks[xb]+(x_brks[xb+1]-x_brks[xb]/2)
	}
	lines(x_mids,y_high,lwd=2)
	lines(x_mids,y_low,lwd=2)

	#x_plot <- c(x_brks,x_brks[(length(x_brks)-1):1])
	#x_plot <- c(x_mids,x_mids[(length(x_mids)):1])
	#y_plot <- c(y_low,y_high[(length(x_mids)):1])
	#return(list(x_plot=x_plot,y_plot=y_plot,y_low=y_low,y_high=y_high))
}
