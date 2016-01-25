## Program that provides a visual display of selection
## starting points are random
## the target shifts out from under the population instantaneously
## lines are printed only when there is an increase in population fitness
## Written by Shanan E. Peters

#######  VARIABLES THAT AFFECT HOW THE SIMULATION BEHAVES ##################################################
gen.size=15 #<<<------- FECUNDITY: generation size scalar
speed_break= 2000 #<<--- loop size to slow down execution for real-time graphics (decrease to speed up)
text.size=1.4 #<<--- cex for text
y.step=2.8	#<<--- position change for print out
############################################################################################################
target=c("T","H","E","R","E","","I","S","","G","R","A","N","D","E","U","R")
target2=c("I","N","","T","H","I","S","","V","I","E","W","","O","F","","L","I","F","E")
len=length(target)
fit.vector=numeric(0)
all.fits=numeric(0)

abc=c(LETTERS,"")
index=c(1:len)

par(mar=c(.1,.1,.1,.1))
plot(1,1,xlim=c(0,len+4),ylim=c(0,100),type="n",axes=FALSE,ann=FALSE)

fit=0
start=sample(abc,size=len,replace=TRUE)
new.start=start

for (i in 1:len) text(i,102,start[i],cex=text.size)

fit=sum(ifelse(start==target,1,0))
text(0,102,0)

y=102 #incrementer for plot y-axis position
counter=fit #keeps track of increase in fit
dum=0 #counts iterations

while(fit<len){
	dum=dum+1
	point.mutation=sample(index,size=gen.size,replace=TRUE)
	mutation.value=sample(abc,size=gen.size,replace=TRUE)
	
	for (i in 1:gen.size){
		new.start=start
		temp=point.mutation[i]
		new.start[temp]=mutation.value[i]
		fit.vector[i]=sum(ifelse(new.start==target,1,0))
	}
	
	max.fit=match(max(fit.vector),fit.vector)
	start[point.mutation[max.fit]]=mutation.value[max.fit]
	
	fit=fit.vector[max.fit]
	all.fits[dum]=fit
	
	if (fit>counter) { #ONLY print if the new generation is better than last
		y=y-y.step
		for (k in 1:len) {
			if (fit==len) text(k,y,start[k],cex=text.size,col="red",font=2)
			else text(k,y,start[k],cex=text.size)
		}
		text(0,y,dum)
		counter=fit;
	}
		for (k in 1:speed_break){  #speed break to allow for nice real-time graphical display
		a=exp(runif(1)) }

}

##NEW TARGET##

target=target2
start=c(start,rep("",times=length(target)-len))
new.start=start
len=length(target)
index=c(1:len)

fit=sum(ifelse(start==target,1,0))

counter=fit #keeps track of increase in fit
old.dum=dum
dum=0 #counts iterations

while(fit<len){
	dum=dum+1
	point.mutation=sample(index,size=gen.size,replace=TRUE)
	mutation.value=sample(abc,size=gen.size,replace=TRUE)
	
	for (i in 1:gen.size){
		new.start=start
		temp=point.mutation[i]
		new.start[temp]=mutation.value[i]
		fit.vector[i]=sum(ifelse(new.start==target,1,0))
	}
	
	max.fit=match(max(fit.vector),fit.vector)
	start[point.mutation[max.fit]]=mutation.value[max.fit]
	
	fit=fit.vector[max.fit]
	all.fits[old.dum+dum]=fit
	
	if (fit>counter) { #ONLY print if the new generation is better than last
		y=y-y.step
		for (k in 1:len) {
			if (fit==len) text(k,y,start[k],cex=text.size,col="red",font=2)
			else text(k,y,start[k],cex=text.size)
		}
		text(0,y,dum)
		counter=fit;
	}
		for (k in 1:speed_break){  #speed break to allow for nice real-time graphical display
		a=exp(runif(1))
	}

}
#plot(1:length(all.fits),all.fits,type="l",xlab="Generations",ylab="Fitness")
