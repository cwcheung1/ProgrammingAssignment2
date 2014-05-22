## FUNCTIONS WHICH SOLVE FOR MATRIX INVERSE, RETRIEVING ANSWER FROM CACHE IF 
## AVAILABLE

## 1. macheCacheMatrix: creates a object to hold input matrix, with a list of 
##                      methods to handle caching operations

makeCacheMatrix <- function(x = matrix()) {
        
        ## Clear cache variable 'm'
        m <- NULL
        
                ## SET matrix values to this object and clear cache 'm'
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                
                ## GET the matrix values
                get <- function() x
                
                ## CACHE variable 'inverse' to cache variable 'm'
                setinverse <- function(inverse) m <<- inverse
                
                ## GET matrix inverse from cache variable 'm'
                getinverse <- function() m
        
        # LIST constructor - put above four methods into a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## 2. cacheSolve:       interacts with object 'x' created with makeCacheMatrix, 
##                      to get matrix inverse from cache, or solve using solve()                    
                        
cacheSolve <- function(x, ...) {
        
        ## GET inverse from object x, store into m
        m <- x$getinverse()   
        
        ## If m contains something, output msg, return and end
        if(!is.null(m)) {
                message("getting cached data")
                return(m) 
        }
        
        ## Otherwise, solve for inverse. 
        else {
                
                ## GET matrix data
                data <- x$get()
                
                ## SOLVE for the inverse and assign to m
                m <- solve(data, ...)
                
                ## Cache this to object x's cache variable
                x$setinverse(m)
                
                ## Return inverse to finish
                m
        }
        
}
