#1
#creating the unit square and our function 
xmin.encl=0
xmax.encl=1
ymin.encl=0
ymax.encl=1
plot(x=c(0,1,1,0,0), y=c(0,0,1,1,0), type='l', frame.plot=F, xlab='x', ylab='y', asp=1)
curve(exp(x-1), from=0, to=1, add=T, col='blue') 	#function

#creating the phat for the monte carlo
n=100
y=runif(n,min=ymin.encl,max=ymax.encl)
x=runif(n,min=xmin.encl,max=xmax.encl)
z=cbind(x,y)
phat=sum(y<=exp(x-1))/n; phat
#n=10  		0.6
#n=100  	0.67
#n=100		0.641
#n=1000		0.6306

#iii-iv-v
u=y<exp(x-1) + 0	#this gives the same as the prev phat calculation
phat=mean(u)	
phat.var=sd(u)^2		#phat variance and sd
phat.sd=sd(u)
phat.se = phat.sd/sqrt(n)
phat + c(-1,1)*qnorm(0.975)*phat.se 	#CI [0.6211399, 0.6400601]

#vi
#calculate p by integration
exp(1-1)-exp(0-1)
#=0.6321206

#vii
#calculate a CI for analytical p and use phat.se in it as p is only one value
p= exp(0)-exp(-1)
p + c(-1,1)*qnorm(0.975)*phat.se		#CI [0.6226605, 0.6415806]
 
#B-C
#do phat and phat CI for different n's
 

 

