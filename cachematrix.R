## makeCacheMatrix: Creates a special "matrix" object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse
        )
}


## cacheSolve: Computes inverse of special "matrix" returned by makeCacheMatrix 
## If inverse is already calculated (and matrix has not changed), cachesolve will retrieve inverse from cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting Cached Matrix Data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        return(inv)
}
