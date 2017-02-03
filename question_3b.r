#!/usr/env/bin/Rscript
source("question_3a.r")

train <- function(num_iter,inc){
	c <- 1; 
	d <- 0.1;
	sets <- seq(1,500/inc);
	error <- matrix(,nrow=length(sets),ncol=num_iter);
	epsilon <- c();

	start <- proc.time();
	for (i in sets){
		for (j in 1:num_iter){
			line <- perceptron(i*inc,c,d); 
			error[i,j] <- get_error(line$coeff1,line$coeff2,c,d);
			epsilon[i] <- sqrt(-(log(0.05)/(2*(i*inc)))); 
		}
	}
	#results <- mapply(calculations,sets,1:num_iter,list(inc=inc,c=c,d=d,error=error,epsilon=epsilon));	
	elapsed <- proc.time() - start;

	error <- t(apply(error,1,sort));
	five_percent <- error[,5]; 
	nfive_percent <- error[,95]; 
	avg <- t(rowMeans(error)); 
	sample_size <- sets*inc; 
	hoeff_5 <- avg - epsilon; 
	hoeff_95 <- avg + epsilon; 
	
	max_value <- max(hoeff_95)+0.01;
	min_value <- min(hoeff_5)-0.01;
	diff <- max_value - min_value; 
	plot(sample_size,seq(min_value,max_value-0.00000002,diff/length(sample_size)),type='n',xlab='Sample Size',ylab='Error Probability');
	lines(sample_size,avg,type='p'); 
	lines(lowess(sample_size,avg));
	lines(sample_size,five_percent,col='green',type='p');
	lines(lowess(sample_size,five_percent),col='green');
	lines(sample_size,nfive_percent,col='red',type='p');
	lines(lowess(sample_size,nfive_percent),col='red');
	lines(lowess(sample_size,hoeff_5),col='blue');
	lines(lowess(sample_size,hoeff_95),col='purple');
	
	return (list(e=error,t=elapsed)); 
}

calculations <- function(i,j,params){
	inc <- 100; 
	c <- 1;
	d <- 0.1;
	error <- params['error'];
	epsilon <- params['epsilon']; 
	line <- perceptron(i*inc,c,d); 
	error[i,j] <- get_error(line$coeff1,line$coeff2,c,d);
	epsilon[i] <- sqrt(-(log(0.05)/(2*(i*inc)))); 
	return (list(error=error,eps=epsilon));
}

get_error <- function(a,b,c,d){
	#for l1 = cx + d = x + 0.1
	l1 = list(x1=(1-d)/c,x0=-d/c); 
	#for l2 = ax + b
	l2 = list(x1=(1-b)/a,x0=-b/a);
	#point of intersection 
	x0 = (d-b)/(a-c); 
	y0 = x0+0.1; 

	#case 1 (left half plane)
	if (x0 <= 0) {case <- 1;} 
	#case3 (1st quad and outside the box)
	else if (x0>0 && y0>=0 && (x0>=1 || y0>=1)) {case <- 3;} 
	#case2 (within the box)
	else if (x0>0 && x0<1 && y0>0 && y0<1) 
	{
		if ((sign(a) == 1)){
			case <- 21;
		}
		else {case <- 22;}
	}

	if (case == 1 || case == 3 || case == 21){
		if (l1$x1 < l2$x1){
			y1 = list(q=l1$x0,r=l1$x1,m=c,c=d);
			y2 = list(t=l2$x0,s=l2$x1,m=a,c=b); 
		}
		else if (l1$x1 > l2$x1){
			y1 = list(q=l2$x0,r=l2$x1,m=a,c=b); 
			y2 = list(t=l1$x0,s=l1$x1,m=c,c=d);
		}
	}
	else if(case == 22){
		y1 = list(q=l1$x0,r=l1$x1,m=c,c=d); 
		y2 = list(t=l2$x0,s=l2$x1,m=a,c=b); 
	}

	return (error_prob(y1,y2,x0,case)); 
}

error_prob <- function(y1,y2,x0,case){
	if (y1$q <= 0) {q <- 0;}
	else if (y1$q>0 && y1$q<1) {q <- y1$q;}
	else {return (0);}

	if (y2$t<=0) {t <- 0;}
	else if (y2$t>0 && y2$t<1) {t <- y2$t;}
	else {t <- 1;}

	if (y1$r<=0) {r <- 0;}
	else if (y1$r>0 && y1$r<1) {r <- y1$r;}
	else {r <- 1;}

	if (y2$s<=0){
		if(case == 22){s <- 0;}
		else {return (0);}
	}
	else if (y2$s>0 && y2$s<1) {s <- y2$s;}
	else {s <- 1;}
	
	if (case == 1 || case == 3){
		return (integral_1(q,r,1,y1) - integral_1(t,s,1,y2));
	}
	else if(case == 21){
		A1 <- -integral_1(q,x0,x0,y1) + integral_1(t,x0,x0,y2);
		A2 <- integral_1(x0,r,1,y1) - integral_1(x0,s,1,y2);
		return (A1+A2); 
	}
	else{
		A1 <- integral_1(q,x0,x0,y1) + integral_1(x0,t,t,y2); 
		y1_1 <- y1; 
		y1_1$m = -y1$m; 
		y1_1$c = y1$m + y1$c; 
		y2_1 <- y2; 
		y2_1$m = -y2$m; 
		y2_1$c = y2$m + y2$c; 
		A2 <- -(integral_1(1-r,1-x0,1-x0,y2_1) + integral_1(1-x0,1-s,1-s,y1_1)); 
		return (1-A1-A2); 
	}
}

integral_1 <- function(lim1,lim2,lim3,fx){
	return (0.5*fx$m*(lim2^2-lim1^2) + (fx$c-1)*lim2 - fx$c*lim1 + lim3);
}

train(100,100)
