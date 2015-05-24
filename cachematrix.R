## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL 
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
  } 
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() 
  if(!is.null(inv)) { 
    message("getting cached data.") 
    return(inv) 
  } 
  data <- x$get() 
  inv <- solve(data) 
  x$setinverse(inv) 
  inv 
}


##### Sample run of the functions 
##> x = rbind(c(2, -7), c(-7, 2)) 
##> m = makeCacheMatrix(x) 
## -- First run of the function 
##> m$get()
##[,1] [,2]
##[1,]    2   -7
##[2,]   -7    2
####--- First run of the cache function .. Result is cached
##> cacheSolve(m) 
##[,1]        [,2]
##[1,] -0.04444444 -0.15555556
##[2,] -0.15555556 -0.04444444
### Second run of the cache function retrieved data from Cache
##> cacheSolve(m) 
##getting cached data.
##[,1]        [,2]
##[1,] -0.04444444 -0.15555556
##[2,] -0.15555556 -0.04444444
