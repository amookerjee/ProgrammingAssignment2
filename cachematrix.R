## The functions below caches the data of a matrix and then inverses the matrix by checking for the cached data.
## 

## The makeCacheMatrix function stores a matrix in a cache

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL         #initialies m to NULL as default
  
  #The set function assigns the value of the matrix m and caches it using <<-       
  set <- function(y) {
    
    x <<- y
    
    m <<- NULL
    
  }
  
  get <- function() x  #get return the function x
  
  setsolve <- function(solve) m <<- solve   #sets the matrix to m
  
  getsolve <- function() m                 #returns the matrix
  
  list(set = set, get = get,
       
       setsolve = setsolve,
       
       getsolve = getsolve)       #returns a vector of the functions
  
}


## This function inverses a matrix but first checks if there is existing cached data.
## It also caches the data of the inversed matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()        ##checks for cached data
  
  if(!is.null(m)) {          ##if cached data exists returns cached matrix
    
    message("getting cached data")
    
    return(m)
  }
  
  data <- x$get()           ##gets value of the input matrix
  
  m <- solve(data, ...)     #inverses the matrix
  
  x$setsolve(m)           #caches the inverse matrix  
  
  m
  
}
