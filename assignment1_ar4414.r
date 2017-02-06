#!/usr/bin/env/Rscript
source('question_3a.r')
source('question_3b.r')
source('question_3c.r')

main <- function(){
	#q3a (y = 3x + 0.2)
	#N <- c(2,4,10,100);
	#for (n in N){
	#	pdf(paste0('q3a_graphs/q3a_',n,'.pdf'));
	#	q3a <- perceptron(n,3,0.2,0);
	#	dset <- q3a$dset; 
	#	plot(dset$feat1,dset$feat2,col=dset$col,main=paste0('Samples: ',n),xlab='Feature 1',ylab='Feature 2');
	#	lines(dset$feat1,q3a$line,col='red');
	#	lines(dset$feat1,dset$orig,col='blue');
	#	#lines(x1,dset$upper,col='blue');
	#	#lines(x1,dset$lower,col='red');
	#}
	
	#q3b(2) run 1 iter for 100,200,300,400,500
	#pdf(paste0('q3b_graphs/q3b(2).pdf'));
	#q3b2 <- train(1,100,0);
	#plot(q3b2$sample_size,q3b2$e,main='Test Error',xlab='Sample Size',ylab='Error Probability',type='o');
	
	##q3b(3) 100 iterations of each above
	#pdf(paste0('q3b_graphs/q3b(3).pdf'));
	#q3b3 <- train(100,100,0);
	#error <- q3b3$e_s; 
	#epsilon <- q3b3$epsilon;
	#sample_size <- q3b3$sample_size;
	#generate_plot(error,epsilon,sample_size,0);

	#q3b(42) run for different gammas 1 iteration
	#gamma <- c(0.3,0.1,0.01,0.001);
	#for (g in gamma){
	#	pdf(paste0('q3b_graphs/q3b(42)_',g,'.pdf'));
	#	q3b2 <- train(1,100,g);
	#	plot(q3b2$sample_size,q3b2$e,main=paste0('Test Error (gamma=',g,')'),xlab='Sample Size',ylab='Error Probability',type='o');
	#}
	
	#q3b(43) run for different gammas 1 iteration
	gamma <- c(0.3,0.1,0.01,0.001);
	for (g in gamma){
		pdf(paste0('q3b_graphs/q3b(43)_',g,'.pdf'));
		q3b43 <- train(100,100,g);
		error <- q3b43$e_s; 
		epsilon <- q3b43$epsilon;
		sample_size <- q3b43$sample_size;
		generate_plot(error,epsilon,sample_size,g);
		sink(paste0('q3b_graphs/q3b_43.txt'),append=TRUE);
		print(paste0('gamma :',g));
		print(q3b43$i);
		print(q3b43$T);
		print(q3b43$w_star);
		cat('\n');
	}

	#q3c all graphs and data
	#q3c_main();
}

generate_plot <- function(error,epsilon,sample_size,gamma){
	five_percent <- error[,5]; 
	nfive_percent <- error[,95]; 
	avg <- t(rowMeans(error)); 
	hoeff_5 <- avg - epsilon; 
	hoeff_95 <- avg + epsilon; 
	if(gamma == 0){
		max_value <- max(hoeff_95)+0.1;
		min_value <- min(hoeff_5)-0.01;	
	}else{
	   max_value <- max(nfive_percent)+0.01;
	   min_value <- min(five_percent)-0.00001;
	}
	
	diff <- max_value - min_value; 
	plot(sample_size,seq(min_value,max_value-0.00000002,diff/length(sample_size)),type='n',xlab='Sample Size',ylab='Error Probability',main=paste0('Test Error :(gamma=',gamma,')'));
	lines(sample_size,avg,type='p'); 
	lines(lowess(sample_size,avg));
	lines(sample_size,five_percent,col='green',type='p');
	lines(lowess(sample_size,five_percent),col='green');
	lines(sample_size,nfive_percent,col='red',type='p');
	lines(lowess(sample_size,nfive_percent),col='red');
	if (gamma == 0){
		lines(lowess(sample_size,hoeff_5),col='blue');
		lines(lowess(sample_size,hoeff_95),col='purple');	
	}
}

main()

