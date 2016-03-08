##makeCacheMatrix creates a matrix, by:
##setting the value of the matrix
##getting the value of the matrix
##setting the inverse of the matrix
##getting the inverse of the matrix

makeCacheMatrix <- function (x = matrix()){
  m <- NULL 
  
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function () x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##cacheSolve is a function where if inverse of matrix has been 
##previously created, cached data will be retrived.
##If not, the inverse of matrix will be calculated 

cacheSolve <- function(x, ...){
    
    m <- x$getinverse()
    
    if (!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    
    x$setinverse(m)
    m
}