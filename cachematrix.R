
## A function to create  matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
     m  <- NULL

#Function to set the value of matrix
     set <- function(y){
              x <<- y
              m <<- NULL
     }
     get <- function() x

#Function to set the value of the inverse matrix
     setmatrix<-function(solve) m <<- solve
     getmatrix<-function() m
     list(set=set, get=get, setmatrix=setmatrix,getmatrix=getmatrix)

}



#This function attempts to inverse the matrix created by makeCacheMatrix 
#only if the inversion has not been done earlier.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


              m <- x$getmatrix()
              if(!is.null(m)) {
                 message("If m is not null then return the value of m from cache")
                 return(m)
              }

#if not found in cache then inverse the matrix. 
              matrix<-x$get()
              m <- solve(matrix, ...)
              x$setmatrix(m)
              m
}
