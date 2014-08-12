norm(Real)
norm(mat)

setMethod(f = ">=", signature(e1="norm",e2="rvector"),definition = function(e1,e2) {
  d1=e1@data
  d2=e2@data
  d=d1 * d2
  rv=new("rvector",data=d)
  return(rv)
})

eval(parse("",text= "norm(vct,type=1)"))
