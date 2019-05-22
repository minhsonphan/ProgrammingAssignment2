## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function() {
        #m est une variable locale à makeVector()
        initial_matrix <- NULL
        inversed_matrix <- NULL
        
        setinitialmatrix <- function(matrix) {
                if(!is.null(matrix)){
                        initial_matrix <<- matrix        
                }else{
                        initial_matrix <<- NULL
                        inversed_matrix <<- NULL
                }
                
        }
        setinversedmatrix <- function(){
                if(!is.null(initial_matrix)){
                        inversed_matrix<<-solve(initial_matrix)
                }else{
                        print("cannot calculate inverse matrix because initial matrix is NULL")
                }
        } 
        getinitialmatrix <- function() initial_matrix
        getinversedmatrix <- function() inversed_matrix
        
        list(setinitialmatrix = setinitialmatrix, setinversedmatrix = setinversedmatrix,
             getinitialmatrix = getinitialmatrix, getinversedmatrix = getinversedmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        #m est une variable locale de cachemean()
        inversed_matrix <- x$getinversedmatrix()
        if(!is.null(inversed_matrix)) {
                message("getting cached data")
                return(inversed_matrix)
        }
        initial_matrix <- x$getinitialmatrix()
        inversed_matrix <- x$setinversedmatrix()
        
}

mymatrix<-matrix(1:4,nrow=2,ncol=2)
mycacheMatrix<-makeCacheMatrix()
mycacheMatrix$setinitialmatrix(mymatrix)
mycacheMatrix$getinitialmatrix()
mycacheMatrix$setinversedmatrix()
mycacheMatrix$getinversedmatrix()
cacheSolve(mycacheMatrix)

mymatrix<-NULL
mycacheMatrix<-makeCacheMatrix()
mycacheMatrix$setinitialmatrix(mymatrix)
mycacheMatrix$getinitialmatrix()
mycacheMatrix$getinversedmatrix()
cacheSolve(mycacheMatrix)
