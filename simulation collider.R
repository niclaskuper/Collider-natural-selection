
############################################
### Simple simulation on collider  #########
### bias &  natural selection ##############
### Niclas Kuper 16.03.19 ##################
############################################

# number of parents 
n<-100000

# beta for relationship between IQ & bodily strength
beta<-c(0,0.5,1)

for ( i in 1:3){

# generate parent IQ
parent1_IQ<-rnorm(n)
parent2_IQ<-rnorm(n)

# generate parent bodily strength 
parent1_bodily_strength<-rnorm(n)+beta[i]*parent1_IQ
parent2_bodily_strength<-rnorm(n)+beta[i]*parent2_IQ

# reproductive fit determined in part by IQ & bodily strength 
parent1_reproductive_fit<- parent1_IQ+parent1_bodily_strength+rnorm(n)
parent2_reproductive_fit<- parent2_IQ+parent2_bodily_strength+rnorm(n)

# children generation 
children_IQ<-(parent1_IQ+parent2_IQ)/2
children_bodily_strength<-(parent1_bodily_strength+parent2_bodily_strength)/2

# every parent has children
dat<-data.frame(children_IQ,children_bodily_strength)

# only parents who both have reproductive fit > 0 have children (heavily simplified)
dat_select<-dat[parent1_reproductive_fit>0 & parent2_reproductive_fit>0,]

print (paste("beta =",beta[i]))

# correlation IQ & bodily strength without natural selection
print("no natural selection")
print(round(cor(dat),2))

# correlation IQ & bodily strength with natural selection
print("natural selection = collider bias ")
print(round(cor(dat_select),2))
print("------------------")
}
