## The firts function create a square matrix and the second function, 
## verifies if the inverse matrix was calculated, otherwise it is calculated and saved in cahe  
 

## This function create a square matrix 
## and initialize a function to calculate the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  im <- NULL
  
  set <- function(y){
    x <<- y
    im <<- NULL 
  }
  
  get <- function() x
  
  setsolve <- function(inmtx) im <<- inmtx
  
  getsolve <- function() im
  
  list(set = set, get=get,
       setsolve=setsolve, getsolve= getsolve)
}


## verifies if the inverse matrix was calculated, otherwise it is calculated and saved in cahe 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getsolve()
  
  if(!is.null(im)){
     
    message("getting cached data")
    
    return(im)
  }
  
  data <- x$get()
  
  im <- solve(data, ...)
  
  x$setsolve(im)
  
  im
  
}




