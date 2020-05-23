## Put comments here that give an overall description of what your
## functions do

##Week 3 Assignment by Sami Muhtasim, github username: muhtasimology



makeCacheMatrix <- function(x = matrix()) {
  ## Write a short comment describing this function
  ## This function creates a special "matrix" object that can cache its inverse.
  
  makeCacheMatrix <- function(x = matrix()){       ##define the argument with default mode of matrix
    inv <- NULL                                    ##inv is initialized as NULL to hold the value of the inverse matrix
    set <- function (y){                           ##define set to assign new
      x <<- y                                      ##value in parent environment
      inv <<- NULL                                 ##if there is a new matrix inv is reset
    }
    get <- function() x                            ##define get to return the value of matrix
    
    setinverse <- function(inverse) inv<<- inverse ##assign inv in parent environment
    getinverse <- function() inv                   ##get inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)}
    
  }




## Write a short comment describing this function
  
## This function calculates inverse of matrix returned by the special above.
## If the inverse has already been calculated and matrix is same,then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
