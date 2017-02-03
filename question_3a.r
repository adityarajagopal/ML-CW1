#!/usr/bin/env/Rscript
dataset <- function(n,m,c,gamma){
	if (gamma == 0){
		x1 <- seq(from=0,to=1,by=1/(n-1)); 
		#x1 <- c(1,x1);
		line <- m*x1+c; 
		rand <- runif(n,-1,1);

		x2 <- line + rand;
		x2 <- abs(x2);
		x2 <- x2 / max(x2);
		i <- (1:length(x2));
		
		plus <- i[x2[i] >= line[i]]; 
		minus <- i[x2[i] < line[i]];
		y = seq(1,n);
		colour = seq(1,n);
		y[plus] = 1;
		y[minus] = -1; 
		colour[plus] = 'blue';
		colour[minus] = 'red';
		return (list(feat1=x1,feat2=x2,class=y,orig=line,col=colour));
	}
	else{
		l1 <- (-c-gamma)/m; 
		l2 <- (1-c-gamma)/m; 
		r1 <- (-c+gamma)/m; 
		r2 <- (1-c+gamma)/m; 

		if (l1 < 0){l1 <- 0;}
		if (l2 > 1){l2 <- 1;}
		if (r1 < 0){r1 <- 0;}
		if (r2 > 0){r2 <- 1;}
		n1 <- (n/2)-1; 
		
		x1_l <- seq(l1,l2,(l2-l1)/n1);
		x1_r <- seq(r1,r2,(r2-r1)/n1); 
		l <- m*x1_l + c + gamma; 
		r <- m*x1_r + c - gamma; 

		x2_l <- runif(length(x1_l),l,1);
		x2_r <- runif(length(x1_r),0,r); 
	
		left <- matrix(rbind(x1_l,x2_l,rep(1,length(x1_l))),nrow=3);
		right <- matrix(rbind(x1_r,x2_r,rep(-1,length(x1_r))),nrow=3);
		graph <- matrix(cbind(left,right),nrow=3);
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

perceptron <- function(datapoints,m,c,gamma){
	dset <- dataset(datapoints,m,c,gamma);
	p <- initialise(dset);
	w <- learn(p,10000);
	
	x1 <- dset$feat1; 
	a <- -(w[2]/w[3]);
	b <- -(w[1]/w[3]);
	x2 <- a*x1 + b; 
	plot(x1,dset$feat2,col=dset$col);
	lines(x1,x2);
	lines(x1,dset$orig,col='green');
	lines(x1,dset$upper,col='blue');
	lines(x1,dset$lower,col='red');

	return (list(coeff1=a,coeff2=b)); 
}

perceptron(100,1,0.1,0.1)
