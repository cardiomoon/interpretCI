#'Decide whether a vector can be treated as a numeric variable
#'
#'Decide whether a vector can be treated as a numeric variable
#'@param x A vector
#'@param maxy.lev An integer indicating the maximum number of unique values of
#'a numeric variable be treated as a categorical variable
#'@export
#'@examples
#'x=1:5
#'is.mynumeric(x)
#'x=1:13
#'is.mynumeric(x)
is.mynumeric=function(x,maxy.lev=5){
     ifelse((is.numeric(x) & (length(unique(x)) > maxy.lev)), TRUE,
            FALSE)
}

