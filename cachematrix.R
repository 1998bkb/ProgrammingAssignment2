## These functions will allow us to cache the inverse of a matrix, to prevent having to repeatedly calculate potentially time-consuming calculations. That way if the inverse is repeatedly needed for a large matrix, we can get it from the cache instead of repeatedly recalcuating it. 

## makeCacheMatrix makes a special matrix through 4 steps:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<- function(y){
    x<<-y
    m<<- NULL
  }
  get <- function()x
  setinverse<-function(solve) m <<-solve
  getinverse<-function()m
  list(set=set, get=get, setinverse=setinverse, getinverse= getinverse)
 }


## cacheSolve finds the inverse of the special matrix created with the previous function. If it has already been calculated it `get`s the inverse from the cache and skips the computation. Otherwise, it calculates the inverse and sets it in the cache with the `setinverse` function. 

cacheSolve <- function(x, ...) {
  m<- x$getinverse()
  if(!is.null(m)){message("getting cached data")
    return(m)}
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
        }
