#!/usr/bin/env/Rscript
dataset <- function(n,m,c,gamma){
	graph <- matrix(,nrow=3,ncol=n);
	num_left <- 0; 
	num_right <- 0; 
	i <- 1; 

	while((num_left < (n/2)) || (num_right < (n/2))){
		x <- runif(1,0,1); 
		y <- runif(1,0,1); 
		if ((y > m*x+(c+gamma)) && (num_left < (n/2))){
			graph[1,i] <- x; 
			graph[2,i] <- y; 
			graph[3,i] <- 1; 
			num_left = num_left + 1; 
			i = i+1; 
		}
		else if((y < m*x+(c-gamma)) && (num_right < (n/2))){
			graph[1,i] <- x; 
			graph[2,i] <- y; 
			graph[3,i] <- -1; 
			num_right = num_right + 1; 
			i = i+1; 
		}
	}
	
	graph <- graph[,order(graph[1,])];
	line <- m*graph[1,] + c; 

	i <- 1:length(graph[1,]);
	plus <- i[graph[3,i] == 1];
	minus <- i[graph[3,i] == -1];
	colour <- seq(1,length(graph[1,]));
	colour[plus] <- 'blue';
	colour[minus] <- 'red';
	
	return (list(feat1=graph[1,],feat2=graph[2,],class=graph[3,],orig=line,upper=m*graph[1,]+c+gamma,lower=m*graph[1,]+c-gamma,col=colour));
}

initialise <- function(dset){
	datapoints = length(dset$feat1);
	x0 = matrix(rep(1,datapoints),nrow=1,ncol=datapoints);
	x1 = matrix(dset$feat1,nrow=1,ncol=datapoints);
	x2 = matrix(dset$feat2,nrow=1,ncol=datapoints);
	y = matrix(dset$class,nrow=1,ncol=datapoints);
	x = rbind(x0,x1,x2);
	w = matrix(rep(1,3),nrow=1,ncol=3); 
	return (list(feat=x,class=y,weights=w));
}

learn <- function(ip,limit){
	x = ip$feat;
	w = ip$weights;
	y = ip$class; 

	num_errors <- 0;
	iter <- 0;
	total_points <- length(y);
	repeat{
		iter = iter + 1;
		h = sign(w %*% x); 
		points = (1:length(y));
		points_in_error <- points[h[points] != y[points]];
		num_errors <- length(points_in_error);
		
		if (num_errors == 0 || iter == limit){
			break;
		}
		else{
			point = points_in_error[1];
			w <- w + (y[point] * t(x[,point]));
		}
	}
	return (list(w=w,iter=iter,err=num_errors/total_points));
}

perceptron <- function(datapoints,m,c,gamma){
	dset <- dataset(datapoints,m,c,gamma);
	p <- initialise(dset);
	learnt <- learn(p,100000);
	
	x1 <- dset$feat1; 
	a <- -(learnt$w[2]/learnt$w[3]);
	b <- -(learnt$w[1]/learnt$w[3]);
	x2 <- a*x1 + b; 
	
	rho <- min(p$class * (learnt$w %*% p$feat));
	R_sq <- max(diag(t(p$feat) %*% p$feat));
	w_star <- sum(learnt$w^2); 
	t <- (R_sq*w_star^2)/(rho^2);
	
	#plot(x1,dset$feat2,col=dset$col);
	#lines(x1,x2);
	#lines(x1,dset$orig,col='blue');
	#lines(x1,dset$upper,col='blue');
	#lines(x1,dset$lower,col='red');

	return (list(coeff1=a,coeff2=b,iter=learnt$iter,t=t,rho=rho,w=learnt$w)); 
}
