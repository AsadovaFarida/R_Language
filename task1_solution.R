#function M = MassAssembler1D(x)
#n = length(x)-1; % number of subintervals
#M = zeros(n+1,n+1); % allocate mass matrix
#for i = 1:n % loop over subintervals
#h = x(i+1) - x(i); % interval length
#M(i,i) = M(i,i) + h/3; % add h/3 to M(i,i)
#M(i,i+1) = M(i,i+1) + h/6;
#M(i+1,i) = M(i+1,i) + h/6;
#M(i+1,i+1) = M(i+1,i+1) + h/3;
#end

MassAssembler1D<-function(x){
n<-length(x)-1
M<-matrix(0,n+1,n+1)
x.num<-as.numeric(x)
for(i in 1:n) {
  h<-x.num[i+1]-x.num[i]
  M[i,i]=M[i,i]+h/3
  M[i,i+1]=M[i,i+1]+h/6
  M[i+1,i]= M[i+1,i]+h/6;
  M[i+1,i+1]=M[i+1,i+1]+h/3
}
  Mylist<-print(list(M))
}

u<-MassAssembler1D(list(1,2,3,4,5))