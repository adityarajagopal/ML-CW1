#!/usr/bin/env/Rscript

initialise1 <- function(file){
	raw_train <- read.table(file);
	i <- 1:nrow(raw_train); 
	index_2 <- i[raw_train[i,1] == 2]; 
	index_8 <- i[raw_train[i,1] == 8]; 
	train_2 <- t(raw_train[index_2,2:ncol(raw_train)]);
	train_8 <- t(raw_train[index_8,2:ncol(raw_train)]); 
	
	col <- ncol(train_2) + ncol(train_8);
	row <- nrow(train_2)+1;
	x0 <- matrix(rep(1,col),ncol=col,nrow=1);
	y1 <- matrix(rep(1,ncol(train_2)),nrow=1);
	y2 <- matrix(rep(-1,ncol(train_8)),nrow=1);
	
	y <- cbind(y1,y2);
	x <- rbind(x0,cbind(train_2,train_8));
	w <- matrix(rep(1,row),nrow=1);

	return(list(class=y,feat=x,weights=w));
}

learn1 <- function(ip,limit){
	x = ip$feat;
	w = ip$weights;
	y = ip$class; 

	num_errors <- 0;
	iter <- 0;
	W <- matrix(,ncol=ncol(w)); 
	error <- c(); 
	total_points <- length(y);
	repeat{
		iter = iter + 1;
		h = sign(w %*% x); 
		points = (1:length(y));
		points_in_error <- points[h[points] != y[points]];
		num_errors <- length(points_in_error);
		
		W <- rbind(W,w);
		error <- c(error,num_errors/total_points);

		if (num_errors == 0 || iter == limit){
			break;
		}
		else{
			point = points_in_error[1];
			w <- w + (y[point] * t(x[,point]));
		}
	}
	W <- W[-1,];
	return (list(w=w,iter=iter,w_log=W,error_log=error,err=num_errors/total_points));
}

learn2 <- function(ip,limit){
	x <- ip$feat;
	w <- ip$weights;
	y <- ip$class; 
	W <- matrix(,ncol=ncol(w));
	error <- c();
	
	num_errors <- 0;
	iter <- 0;
	total_points <- length(y);
	min_error <- total_points + 1; 
	w_min <- w; 
	repeat{
		iter = iter + 1;
		h = sign(w %*% x); 
		points = (1:length(y));
		points_in_error <- points[h[points] != y[points]];
		num_errors <- length(points_in_error);
		
		#modified perceptron updates W and error selectively
		if (num_errors <= min_error){
			min_error <- num_errors;
			w_min <- w; 
		}
		W <- rbind(W,w_min);
		error <- c(error,min_error/total_points);
		
		#check for break otherwise update w
		if (num_errors == 0 || iter == limit){
			break;
		}
		else{
			i <- runif(1,1,length(points_in_error));
			point = points_in_error[i];
			w <- w + (y[point] * t(x[,point]));
		}
	}
	#return the required values
	W <- W[-1,];
	index <- which.min(error); 
	w <- W[index,];
	err <- error[index];
	
	return (list(w=w,iter=iter,w_log=W,error_log=error,error=err));
}

test_hypothesis <- function(w,x,y,w_log){
	points = (1:length(y));
	h_test = sign(w %*% x); 
	points_in_error <- points[h_test[points] != y[points]];

	H <- sign(w_log %*% x);
	P <- c();
	min_error <- 1; 
	for (i in 1:nrow(w_log)){
		error <- length(points[H[i,points] != y[points]])/length(y);
		if (error <= min_error){
			min_error <- error; 
		}
		P[i] <- min_error;
	}

	return(list(e_percent=length(points_in_error)/length(y),e_log=P));
}
test_hypothesis_old <- function(w,x,y,w_log){
	points = (1:length(y));
	h_test = sign(w %*% x); 
	points_in_error <- points[h_test[points] != y[points]];

	H <- sign(w_log %*% x);
	P <- c();
	for (i in 1:nrow(w_log)){
		P[i] <- length(points[H[i,points] != y[points]])/length(y);
	}

	return(list(e_percent=length(points_in_error)/length(y),e_log=P));
}

linreg <- function(X,y){
	X <- t(X); 
	y <- t(y); 
	w <- solve(t(X) %*% X) %*% t(X) %*% y;

	return(t(w));
}

q3c_main <- function(){
	train_raw <- initialise1('data/zip.train');
	test_raw <- initialise1('data/zip.test');
	learn_raw <- learn2(train_raw,1000);
	learn_raw_old <- learn1(train_raw,1000);
	test_error_raw <- test_hypothesis_old(learn_raw$w,test_raw$feat,test_raw$class,learn_raw$w_log);
	test_error_raw_old <- test_hypothesis_old(learn_raw_old$w,test_raw$feat,test_raw$class,learn_raw_old$w_log);

	train_feat <- initialise1('data/features.train');
	test_feat <- initialise1('data/features.test');
	learn_feat <- learn2(train_feat,1000);
	learn_feat_old <- learn1(train_feat,1000);
	test_error_feat <- test_hypothesis_old(learn_feat$w,test_feat$feat,test_feat$class,learn_feat$w_log);
	test_error_feat_old <- test_hypothesis_old(learn_feat_old$w,test_feat$feat,test_feat$class,learn_feat_old$w_log);
	
	#initialise weights for features using linear regression
	train_feat$weights <- linreg(train_feat$feat,train_feat$class); 
	learn_feat_w <- learn2(train_feat,1000);
	learn_feat_w_old <- learn1(train_feat,1000);
	test_error_feat_w <- test_hypothesis_old(learn_feat_w$w,test_feat$feat,test_feat$class,learn_feat_w$w_log);
	test_error_feat_w_old <- test_hypothesis_old(learn_feat_w_old$w,test_feat$feat,test_feat$class,learn_feat_w_old$w_log);

	#compare test and training error for both cases
	sink('q3c_graphs/q3c.txt');
	print ('Raw training error :'); print(learn_raw$error);
	print ('Raw test error :'); print(test_error_raw$e_percent);
	print ('Feat training error :'); print(learn_feat$error); 
	print ('Feat test error :'); print(test_error_feat$e_percent);
	print ('Feat training error (w) :'); print(learn_feat_w$error);
	print ('Feat test error (w) :'); print(test_error_feat_w$e_percent);
	print ('Raw data w :'); print(learn_raw$w);
	print ('Feature data w :'); print(learn_feat$w);
	
	
	pdf('q3c_graphs/q3c_21.pdf');
	#plot raw training and test error vs iterations for modified perceptron
	plot(1:length(learn_raw$error_log),learn_raw$error_log,type='n',main='Raw Data Training and Test Error (Modified Perceptron)',xlab='Number of Updates',ylab='Error Probability');
	lines(1:length(learn_raw$error_log),learn_raw$error_log,col='red');
	lines(1:length(learn_raw$error_log),test_error_raw$e_log,col='blue');
	
	pdf('q3c_graphs/q3c_22.pdf');
	#plot raw training and test error vs iterations for original perceptron
	plot(1:length(learn_raw_old$error_log),learn_raw_old$error_log,type='n',main='Raw Data Training and Test Error (Original Perceptron)',xlab='Number of Updates',ylab='Error Probability');
	lines(1:length(learn_raw_old$error_log),learn_raw_old$error_log,col='red');
	lines(1:length(learn_raw_old$error_log),test_error_raw_old$e_log,col='blue');
	
	pdf('q3c_graphs/q3c_23.pdf');
	#plot feat training and test error vs iterations for modified perceptron
	plot(1:length(learn_feat$error_log),learn_feat$error_log,type='n',main='Feature Data Training and Test Error (Modified Perceptron)',xlab='Number of Updates',ylab='Error Probability');
	lines(1:length(learn_feat$error_log),learn_feat$error_log,col='red');
	lines(1:length(learn_feat$error_log),test_error_feat$e_log,col='blue');
	
	pdf('q3c_graphs/q3c_24.pdf');
	#plot feat training and test error vs iterations for original perceptron
	plot(1:length(learn_feat_old$error_log),learn_feat_old$error_log,type='n',main='Feature Data Training and Test Error (Original Perceptron)',xlab='Number of Updates',ylab='Error Probability');
	lines(1:length(learn_feat_old$error_log),learn_feat_old$error_log,col='red');
	lines(1:length(learn_feat_old$error_log),test_error_feat_old$e_log,col='blue');
	
	pdf('q3c_graphs/q3c_31.pdf');
	#plot feat training and test error vs iterations for linreg initialised modified perceptron	
	#plot(1:length(learn_feat_w$error_log),learn_feat_w$error_log,type='n');
	plot(1:length(learn_feat_w$error_log),test_error_feat_w$e_log,col='blue',type='l',main='Feature Data Training and Test Error \n (Modified Perceptron initialised with linear regression)',xlab='Number of Updates',ylab='Error Probability');
	lines(1:length(learn_feat_w$error_log),learn_feat_w$error_log,col='red',type='l');
	pdf('q3c_graphs/q3c_32.pdf');
	#plot feat training and test error vs iterations for linreg initialised original perceptron	
	plot(1:length(learn_feat_w_old$error_log),learn_feat_w_old$error_log,type='n',main='Feature Data Training and Test Error \n (Original Perceptron initialised with linear regression)',xlab='Number of Updates',ylab='Error Probability');
	lines(1:length(learn_feat_w_old$error_log),learn_feat_w_old$error_log,col='red');
	lines(1:length(learn_feat_w_old$error_log),test_error_feat_w_old$e_log,col='blue');
}
