Q1<-function(N,T,a,b){
		nu<-matrix(,10000,T)
		for (i in 1:10000){
				nu[i,]<-runif(T,1,10)
		}
		last<-nu[,-c(1:(T-N))]
		L<-apply(last,1,prod)
		s<-apply(nu,1,sort)
		max<-s[-c(1:(T-N)),]
		M<-apply(max,2,prod)
 	
 		dif<-M-L
 		q1<-signif(mean(dif),10)
 		q2<-signif(sd(dif),10)
 		pr<-length(subset(dif,dif>=a & dif<=b))/length(subset(dif,dif<=b))
 		p<-signif(pr,10)
	return(c(q1,q2,p))
}
