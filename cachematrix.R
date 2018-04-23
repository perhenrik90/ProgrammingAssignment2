

# Create a special matrix
makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL;
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    # get data from the special matrix
    get <- function() x

    # set the cached value when first computed
    setsolve <- function(solve) inverse <<- solve

    # get the cache if it exists
    getsolve <- function() inverse
    
    list(set = set, get = get, setsolve=setsolve, getsolve=getsolve)
}



# Execute the solve function one the special cache matrix. 
cacheSolve <- function(x, ...) {
    s <- x$getsolve()

    # get cached data if it exists
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    
    # else compute the matrix
    data = x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

