##these functions allow you to reference / store the value of a matrix's inverse

##returns a list of the 4 functions that will be used in cacheSolve. 
##also stores in its environment the value of the matrix and its inverse, if any. 
##these values are referenced / edited by the 4 functions that it returns.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setinv <- function(n){inv <<- n}
  getinv <- function(){inv}
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##the argument x is the list of 4 functions returned by makeCacheMatrix.
##if an inverse exists, return the inverse. if not, compute inverse and return it

cacheSolve <- function(x, ...){
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}