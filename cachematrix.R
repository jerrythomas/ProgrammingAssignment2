# Create a composite object for an input matrix which consists of
#  a) the input matrix
#  b) the cached inverse matrix
#  c) getters and setters for the original matrix
#  d) getters and setters for the inverse

makeCacheMatrix <- function(x = matrix()) {
    # Basic validation ensures that only square matrices are accepted
    if (nrow(x) != ncol(x)){
        stop("Input is not a square matrix")
    }
    # The cache object for inverse
    m <- NULL
        
    # Setter to store the input matrix within the CacheMatrix
    set <- function(y) {
        # whenever the input matrix changes clear the cache
        x <<- y
        m <<- NULL
    }
        
    # Getter to retrieve the input matrix
    get <- function() x
        
    # Setter to store the inverse matrix in cache 
    setInverse <- function(inverse) m <<- inverse
       
    # Getter to retrieve the cached inverse matrix
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# This function retrieves a cached inverse of input CacheMatrix object
# If the inverse is not available it is calculated and stored
cacheSolve <- function(x, ...) {
    # Fetch the inverse of the matrix
    m <- x$getInverse()
    
    # Check if inverse is available in cache
    if(!is.null(m)) {
        # Retrieve the inverse from cache 
            
        message("Getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    # Calculate the inverse matrix
    m <- solve(data, ...)
    # Store in the cache
    x$setInverse(m)
    m
}
