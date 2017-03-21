# Matt Livingston Programming Assignment 2

# This function caches a matrix that is fed to it as the variable x. 
# The first function receives the input of a matrix and outputs a list of functions.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # Define set.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Define get, which just returns the matrix (x).
        get <- function() x
        
        # Defines setinv and getinv 
        setinv <- function(inver) inv <<- inver
        getinv <- function() inv
        
        # Output list where
        # 1. Set the matrix
        # 2. Get the matrix
        # 3. Set the inverse matrix.
        # 4. Get the inverse matrix.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

# This function decides, upon receiving the input of the above function,
# whether to solve for the inverse of the matrix or simply pull the inverse
# from the cache to save on computing time. 

cacheSolve <- function(x, ...) {
        
        # Pull out the inver function from the list to see if a cache exists.
        inv <- x$getinv()
        
        # Only triggers if their is a previously cached matrix.
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        
        # If the matrix isn't cached, calculate the inverse.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv      
        
}
