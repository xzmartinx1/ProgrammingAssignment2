# The makeCasheMatrix function creates a special matrix that caches a matrix inverse

makeCasheMatrix <- function(x = matrix()) { 
  inv <- NULL  
  set <- function(y) { 
    x <<- y  # the <<- operator is used to assign a value to an object in an environment that is different from the current environment
    m <<- NULL
  }
  get <- function() x #get the value of the Matrix
  
  
  setinverse <- function(inverse) inv <<- inverse #set the value of the invertible matrix
  getinverse <- function() inv  #get the value of the invertible matrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an input 
## and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not

cacheSolve <- function(x, ...) {  
  m <- x$getinverse()
  if(!is.null(inv)) { ## if inverse matrix is not NULL
    message("getting cached data") #Type message: Getting Cached Invertible Matrix 
    return(inv) #return the invertible matrix
  }
  
  data <- x$get() #get the original Matrix Data 
  inv <- solve(data, ...) #use solve function to compute the matrix inverse
  x$setinverse(inv) #set the invertible matrix 
  inv  
}

