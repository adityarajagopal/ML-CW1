#!/usr/bin/env/Rscript
source('question_3a.r')

initialise <- function(file){
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

learn_1 <- function(ip,limit){
	x <- ip$feat;
	w <- ip$weights;
	y <- ip$class; 
	W <- matrix(,ncol=ncol(w));
	error <- c();
	
	num_errors <- 0;
	iter <- 0;
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
	w_old <- w; 
	W <- W[-1,];
	index <- which.min(error); 
	w <- W[index,];
	return (list(w=w,w_old=w_old,iter=iter,w_log=W,error_log=error));
}

test_hypothesis <- function(w,x,y,w_log){
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

main <- function(){
	train_raw <- initialise('data/zip.train');
	test_raw <- initialise('data/zip.test');
	learn_raw <- learn_1(train_raw,1000);
	test_error_raw <- test_hypothesis(learn_raw$w,test_raw$feat,test_raw$class,learn_raw$w_log);

	train_feat <- initialise('data/features.train');
	test_feat <- initialise('data/features.test');
	learn_feat <- learn_1(train_feat,1000);
	test_error_feat <- test_hypothesis(learn_feat$w,test_feat$feat,test_raw$class,learn_feat$w_log);

	train_feat$weights <- linreg(train_feat$feat,train_feat$class); 
	learn_feat_w <- learn_1(train_feat,1000);
	test_error_feat_w <- test_hypothesis(learn_feat_w$w,test_feat$feat,test_raw$class,learn_feat_w$w_log);
	
	plot(1:length(learn_raw$error_log),learn_raw$error_log,type='n');
	lines(1:length(learn_raw$error_log),learn_raw$error_log,col='red');
	lines(1:length(learn_raw$error_log),test_error_raw$e_log,col='blue');

	plot(1:length(learn_feat$error_log),learn_feat$error_log,type='n');
	lines(1:length(learn_feat$error_log),learn_feat$error_log,col='red');
	lines(1:length(learn_feat$error_log),test_error_feat$e_log,col='blue');
	
	plot(1:length(learn_feat_w$error_log),learn_feat_w$error_log,type='n');
	lines(1:length(learn_feat_w$error_log),learn_feat_w$error_log,col='red');
	lines(1:length(learn_feat_w$error_log),test_error_feat_w$e_log,col='blue');
}

main()
