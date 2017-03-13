StiffnessAssembler1D<-function(x,a,kappa){
n<-length(x)-1;
A<-matrix(0,n+1,n+1)
x.num<-as.numeric(x)
for(i in 1:n) {
  h<-x.num[i+1]-x.num[i]
  xmid<-(x.num[i+1] + x.num[i])/2
  a=Conductivity(xmid)
  amid=a
  A[i,i] = A[i,i] + amid/h
  A[i,i+1] = A[i,i+1] - amid/h
  A[i+1,i] = A[i+1,i] - amid/h
  A[i+1,i+1] = A[i+1,i+1] + amid/h
}
kappa.num<-as.numeric(kappa)
A[1,1] = A[1,1] + kappa.num[1]
A[n+1,n+1] = A[n+1,n+1] + kappa.num[2]
A[is.na(A)] <- 0
Mass<-print(as.matrix(A))
}


#page 19-Assembly of the load vector
LoadAssembler1D<-function(x,f){
n<-length(x)-1
b<-matrix(0,n+1,1)
x.num<-as.numeric(x)
print(x)
for(i in 1:n) {
  h<-x.num[i+1]-x.num[i]
  h<-as.numeric(h)
  b[i]=b[i] + f[i]*h/2
  b[i+1]= b[i+1]+f[i+1]*h/2
}
  Vector<-print(as.vector(b))
}


SourceAssembler1D<-function(x,f,kappa,g){
b <-LoadAssembler1D(x,f);
kappa.num<-as.numeric(kappa)
g.num<-as.numeric(g)
b[1] = b[1] + kappa.num[1]*g.num[1];
b[length(b)] = b[length(b)] + kappa.num[2]*g.num[2];
 matt<-print(as.vector(b))
}

#####Conductivity function###
Conductivity<-function(x){
x.num<-as.numeric(x)
y<-0.1*(5 - 0.6*x.num)

}
#####Source function###
Source<-function(x){
x.num<-as.numeric(x)
y=0.03*(x.num-6)^4
}

PoissonSolver1D<-function(xx){
#h = 0.1
x =as.numeric(xx)
kappa = list(1.e+6,0)
g =list(-1,0)
A = StiffnessAssembler1D(x, Conductivity(x), kappa)
b = SourceAssembler1D(x, Source(x), kappa, g)
u = solve(A,b);
plot(x,u,col=ifelse(x, "red", "black"))
lines(x,u)
}

###Passing values to the PoissonSolver function here####
u<-PoissonSolver1D(list(2,3,4,5,6,7,8))
