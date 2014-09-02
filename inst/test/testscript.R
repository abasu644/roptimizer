#source(paste(system.file(package="roptimizer"),"R/vectors.R",sep="/"))
vct=new("rvector",data=1:10)
#norm(vct,-1)

mat=new("rmatrix",data=matrix(c(1,2,2,1),2))
diag(mat)
t(mat)

mat2=new("rmatrix",data=matrix(c(2,2,2,2),2))
mat%*%mat2

norm(mat,"F") * norm(mat2,"F")
norm(mat2,"F")
norm(mat%*%mat2,type="F")

mati=new("rmatrix",data=matrix(c(1,0,0,1),2,2))
norm(mati,"F")

real=new("Real",data=c(1,0,0,1),n=4)
norm(real,type=1)
proofFlag=TRUE
norm(real,type=1)


cauchysequence(asin,list(x=0.3,y=5,e=0.001))
