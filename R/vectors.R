setClass("rvector", representation(data = "numeric"))
setClass("rmatrix",representation(data="matrix"))
setMethod(f = "norm", signature = "rvector",definition = function (x,type=c(1,2,Inf) ) {
            d=x@data
            if(type==1) return(sum(abs(d)))
            else if(type==2) return(t(d) %*% d)
            else if(is.infinite(type)) return(max(abs(d))) 
            else stop("select type=c(1,2,Inf) ")
            
          })

setMethod(f = "sum", signature = "rvector",definition = function(x) {
  d=x@data
  rv=new("rvector",data=sum(d))
  return(rv)
  
})
setMethod(f = "diag", signature (x="rmatrix"),definition = function(x ) {
  d=x@data
  rv=new("rvector",data=diag(d))
  return(rv)
})
setGeneric("tr", function(x, step, ...)  standardGeneric("tr"))
setMethod(f = "tr", signature = "matrix",definition = function(x) {
  return(sum(diag(x)))
})
setMethod(f = "tr", signature = "rmatrix",definition = function(x) {
  return(sum(diag(x)))
})

setMethod(f = "t", signature = "rmatrix",definition = function(x) {
  d=x@data
  rv=new("rvector",data=t(d))
  return(rv)
})

setMethod(f = "t", signature = "rmatrix",definition = function(x) {
  d=x@data
  rm=new("rmatrix",data=t(d))
  return(rm)
})

setMethod(f = "%*%", signature (x="rmatrix",y="rmatrix"),definition = function(x,y) {
  d1=x@data
  d2=y@data
  d=d1 %*% d2
  rm=new("rmatrix",data=d)
  return(rm)
})

setMethod(f = "norm", signature = "rmatrix",definition = function (x,type=c("F") ) {
  
   if(type=="F"){
    xt=t(x)
    d=x %*% xt
    trc=tr(d)
    trc.d=trc@data
    rv=new("rvector",data=sqrt(trc.d))
    return(rv)
  } 
  else stop("type is constrained ")
  
})




