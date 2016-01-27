makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y  ## different environment form the current  
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv))## if the inverse has already calculated {
        message("getting cached data.") ## do this
        return(inv)
    }
    ## if it is not continue
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
