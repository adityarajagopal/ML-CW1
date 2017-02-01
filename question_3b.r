#!/usr/env/bin/Rscript
source("question_3a.r")

train <- function(n){
	line <- perceptron(n,1,0.1); 
	a <- line$coeff1; 
	b <- line$coeff2;
	isect <- get_intersections(2,0.5);
	return (c(isect,error_prob_13(isect$y1,isect$y2,isect$x0,isect$case)));
}

get_intersections <- function(a,b){
	#for l1 = x + 0.1
	l1 = list(x1=0.9,x0=-0.1); 
	#for l2 = ax + b
	l2 = list(x1=(1-b)/a,x0=-b/a);
	#point of intersection 
	x0 = (0.1-b)/(a-1); 
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
			y1 = list(q=l1$x0,r=l1$x1,m=1,c=0.1);
			y2 = list(t=l2$x0,s=l2$x1,m=a,c=b); 
		}
		else if (l1$x1 > l2$x1){
			y1 = list(q=l2$x0,r=l2$x1,m=a,c=b); 
			y2 = list(t=l1$x0,s=l1$x1,m=1,c=0.1);
		}
	}
	else if(case == 22){
		y1 = list(q=l1$x0,r=l1$x1,m=1,c=0.1); 
		y2 = list(t=l2$x0,s=l2$x1,m=a,c=b); 
	}

	return (list(y1=y1,y2=y2,case=case,x0=x0)); 
}

error_prob_1321 <- function(y1,y2,x0,case){
	if (y1$q <= 0) {q <- 0;}
	else if (y1$q>0 && y1$q<1) {q <- y1$q;}
	else {return (0);}

	if (y2$t<=0) {t <- 0;}
	else if (y2$t>0 && y2$t<1) {t <- y2$t;}
	else {t <- 1;}

	if (y1$r<=0) {r <- 0;}
	else if (y1$r>0 && y1$r<1) {r <- y1$r;}
	else {r <- 1;}

	if (y2$s<=0) {return (0);}
	else if (y2$s>0 && y2$s<1) {s <- y2$s;}
	else {s <- 1;}

	return (integral_1(q,r,1,y1) - integral_1(t,s,1,y2));
}

error_prob_21 <- function(y1,y2,x0){
		
}

integral_1 <- function(lim1,lim2,lim3,fx){
	return (0.5*fx$m*(lim2^2-lim1^2) + (fx$c-1)*lim2 - fx$c*lim1 + lim3);
}

train(100)
