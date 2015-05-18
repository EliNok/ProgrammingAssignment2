## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#  makeCacheMatrix function creates a special inverse matrix which contains following functions
#  set - sets the values in matrix
#  get - gets the matrix values
#  setinverse - set values of the inverse matrix 
#  getinverse - get values of inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        
        s <- NULL
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(solve) s <<- solve
        
        getinverse <- function() s
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        s <- x$getinverse()
        
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        data <- x$get()
        
        s <- solve(data, ...)
        
        x$setinverse(s)
        
        s
}
