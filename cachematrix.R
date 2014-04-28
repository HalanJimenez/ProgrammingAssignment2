##
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean
##
makeCacheMatrix <- function(x = matrix()) {
        # Initializing the stored inverse matrix
        inv <- NULL
        
        # Setter of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Getter of the matrix
        get <- function() x
        
        # Setter of the inverse matrix
        setinv <- function(inverse) inv <<- inverse
        
        # Getter of the inverse matrix
        getinv <- function() inv
        
        # Return a list with the getters and setters above
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
##

cacheSolve <- function(x, ...) {
        # Is the inverse already cached?
        # Then return the cached inverse matrix
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Else: calculate the inverse matrix
        data <- x$get()
        inv <- solve(data, ...)
        
        # Cache the inverse matrix and return it
        x$setinv(inv)
        inv
}