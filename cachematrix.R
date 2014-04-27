## Cachematrix.R
## Author: Nikhil Mascarenhas


##----------------- OVERALL DESCRIPTION -------------------------------------
##
## The following code contains a pair of functions 
## that cache the inverse of a matrix to save 
## time by avoiding repeated computation
##
## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix". 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
##
##-------------------------------------------------------------------------




## ------------------  FUNCTION: makeCacheMatrix --------------------------
##
## The first function, makeVector creates a special "matrix", 
## which is really a list containing a function to
##  - set the values of the matrix (set)
##  - get the value of the matrix (get)
##  - set the value of the inverse of the matrix (setinverse)
##  - get the inverse of the matrix (getmatrix)
##
##-------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) 
{
  
  inverse <- NULL

  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function()
  {
    x
  }
  
  setinverse <- function(new_inverse)
  {
    inverse <- new_inverse
  }
  
  getinverse <- function()
  {
    inverse
  }
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##-------------------------------------------------------------------------





## ------------------  FUNCTION: cacheSolve -------------------------------
##
## This function calculates the inverse of the special "matrix". 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setinverse function.
##
##-------------------------------------------------------------------------

cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()
  
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  
  inv
}

##-------------------------------------------------------------------------
