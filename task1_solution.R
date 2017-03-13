#page 17-Assembly of the mass matrix

MassAssembler1D<-function(x){
n<-length(x)-1
M<-matrix(0,n+1,n+1)
x.num<-as.numeric(x)
for(i in 1:n) {
  h<-x.num[i+1]-x.num[i]
  M[i,i]=M[i,i]+h/3
  M[i,i+1]=M[i,i+1]+h/6
  M[i+1,i]= M[i+1,i]+h/6
  M[i+1,i+1]=M[i+1,i+1]+h/3
}
Mass<-print(as.matrix(M))
}

#page 19-Assembly of the load vector

LoadAssembler1D<-function(x,f){
n<-length(x)-1
b<-matrix(0,n+1,1)
x.num<-as.numeric(x)
for(i in 1:n) {
  h<-x.num[i+1]-x.num[i]
  b[i]=b[i] + f(x.num[i])*h/2
  b[i+1]= b[i+1]+ f(x.num[i+1])*h/2
}
  Vector<-print(as.vector(b))
}

#Fuction creation
f<-function(x){
x.num<-as.numeric(x)
y=x.num*sin(x.num)
}

#page 20- L2Projection

L2Projector1D<-function(){
n=5
h<-1/n
x=list(0,h,1)
M<-MassAssembler1D(x)
b<-LoadAssembler1D(x,f)
print(is.numeric(M))
print(is.numeric(b))
Pf=solve(M,b)
print(Pf)
plot(x,Pf) 
lines(x,Pf)
}

u<-L2Projector1D()



