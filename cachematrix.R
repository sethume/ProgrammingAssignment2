## Put comments here that give an overall description of what your
## functions do
## This is for Programming Assignment 2 of the R Programming Course
## from Coursera.org
## This exercise has two functions. The objective is to demonstrate
## lexical scoping and functions within functions
## Inversing a matrix takes lot of computation power especially
## if it is a large matrix. If the inverted matrix is needed more than
## one time, it is better to store the result from the first time
## and reuse it instead of inverting again and again
## This improves the performance


## Write a short comment describing this function
## This function takes x as an argument that has a default value of matrix()
## It provides a list of four functions
## set a matrix and get that matrix
## set a value for inversed matrix and get that inversed matrix
## set function clears the cached value in the global environment
## setinversedmatrix sets the value of the inversed matrix in the 
## global environment so there is no need to inverse again


makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinversedmatrix <- function(invmat) m <<- invmat
    getinversedmatrix <- function() m
    
    list (set = set, get = get, setinversedmatrix = setinversedmatrix, getinversedmatrix = getinversedmatrix)
    
    
}



## Write a short comment describing this function
## This first checks if the inversed matrix for the given matrix exists 
## and if yes, it simply returns that. 
## If not, it not only computes
## but also caches it so that it can be reused
## The argument ... is strictly not required for this exercise
## but here for future use (also following the sample for grading)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinversedmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinversedmatrix(m)
    m
}
