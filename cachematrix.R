## Put comments here that give an overall description of what your
## functions do
## 


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL

  set <- function(y){
     x <<- y
     inverse <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setinverse <- function(new_inverse){
    inverse <- new_inverse
  }
  
  getinverse <- function(){
    inverse
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

m
