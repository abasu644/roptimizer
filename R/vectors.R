proofFlag=FALSE
setClass("rvector", representation(data = "numeric"))
setClass("Real", representation(n = "numeric"),contains="rvector")

setClass("rmatrix",representation(data="matrix"))
setMethod(f = "norm", signature = "Real",definition = function (x,type=c(1,2,Inf) ) {
  if(proofFlag)
  {
    st=sprintf("norm(%s) >= 0",as.character(substitute(x)))
    return(st) 
  }else
  {
    
    d=x@data
    if(type==1) return(sum(abs(d)))
    else if(type==2) return(t(d) %*% d)
    else if(is.infinite(type)) return(max(abs(d))) 
    else stop("select type=c(1,2,Inf) ")
  }
})
setMethod(f = "norm", signature = "rvector",definition = function (x,type=c(1,2,Inf) ) {
            d=x@data
            if(type==1) return(sum(abs(d)))
            else if(type==2) return(t(d) %*% d)
            else if(is.infinite(type)) return(max(abs(d))) 
            else stop("select type=c(1,2,Inf) ")
            
          })

setMethod(f = "*", signature(e1="rvector",e2="rvector"),definition = function(e1,e2) {
  d1=e1@data
  d2=e2@data
  d=d1 * d2
  rv=new("rvector",data=d)
  return(rv)
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

setMethod(f = "norm", signature = "rmatrix",definition = function (x,type=c("F","E") ) {
  
   if(type=="F"){
     #Frobenius norm
    xt=t(x)
    d=x %*% xt
    trc=tr(d)
    trc.d=trc@data
    rv=new("rvector",data=sqrt(trc.d))
    return(rv)
  } 
  else if(type=="E"){
    #
  }
  else stop("type is constrained ")
  
})

setGeneric("cauchysequence", function(f, args, ...)  standardGeneric("cauchysequence"))
setMethod(f = "cauchysequence", signature = "function",definition = function (f,args ) {
  a=args[["x"]]
  b=args[["y"]]
  e=args[["e"]]
  l=abs(a-b)
  while(l>e)
  {
    #sq=seq(a,b,intrsz)
    if(!is.nan(f(a)))
    {
      if(is.nan(f(b)))
      {
        k=(a+b)/2
        if(is.nan(f(k)))
        {
          b=k
        }else
        {
          a=k
        }
      }
    }
    l=abs(a-b)
    print(sprintf("a=%s,b=%s",a,b))
  } 
   
})

