## The function creates a special "matrix" object - a list containing functions to: 1) set and 2) get the value of the matrix, and the functions to 3) set and 4) get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve =  setsolve,
             getsolve = getsolve)
}



##The function retrieves the cached inverse of the matrix returned by makeCacheMatrix if available; otherwise it computes the inverse of the matrix

cacheSolve <- function(x, ...) {
             s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s

}
