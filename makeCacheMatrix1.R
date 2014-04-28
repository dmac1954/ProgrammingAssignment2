## This function (from the Coursera R course) is an illustration of how R
## can be used to nestle functions.  The book that seems most apropos is
## 'The Art of Programming in R' in which he writes (on Recursion) that the
## key  to understanding recursion is understanding the Towers of Hanoi
## problem, and you use R's scoping and enviroments to go back and forth
## to solve a function, and either keep it local or make it global as needed
## a list can be a list of functions


## hard to improve on the title of the function; this makes a matrix, and stores
## it in a list('m')  which is a global_env variable 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve

        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## this does the same as previous but it checks 'm' first to see if the
## solved matrix has been previously accomplished
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } 
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}