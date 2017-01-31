#!/usr/bin/env/Rscript
dataset <- function(n){
	x1 <- seq(1,n+1); 
	line <- 3*x1+5; 
	rand <- rnorm(n+1,mean=0,sd=100);
	x2 <- line + rand;
	i <- (1:length(x2));
	plus <- i[x2[i] >= line[i]]; 
	minus <- i[x2[i] < line[i]];
	y = seq(1,n+1);
	colour = seq(1,n+1);
	y[plus] = 1;
	y[minus] = -1; 
	colour[plus] = 'blue';
	colour[minus] = 'red';
	return (list(feat1=x1,feat2=x2,class=y,orig=line,col=colour));
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
	return (w);
}

dset <- dataset(100);
p <- initialise(dset);
w <- learn(p,10000);

x1 <- dset$feat1; 
x2 <- -(w[2]/w[3])*x1 - (w[1]/w[2]); 
plot(x1,dset$feat2,col=dset$col);
lines(x1,x2,col='red');
lines(x1,dset$orig,col='blue');

