vct=new("rvector",data=1:10)
norm(vct,-1)

mat=new("rmatrix",data=matrix(c(1,2,2,1),2))
diag(mat)
t(mat)

mat2=new("rmatrix",data=matrix(c(2,2,2,2),2))
mat%*%mat2

norm(mat,"F")
