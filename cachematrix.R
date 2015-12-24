
makeCacheMatrix <- function(x = matrix()) {
        
        xinv <- NULL
        
        set <- function(y) {
                x <<- y
                xinv <<- NULL }
        
        get <- function() x
        
        setxinv <- function(mv) xinv <<- mv
        
        getxinv <- function() xinv
        
        list(set=set,get=get,setxinv=setxinv,getxinv=getxinv)
}

## ---------------------------------------------

cacheSolve <- function(x = matrix(), ...) {
        
        xinv <- x$getxinv()
        
        if (!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        
        m <- x$get()
        
        m <- solve(m, ...)
        
        x$setxinv(m)
        
        m
}
