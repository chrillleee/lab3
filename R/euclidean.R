#' Euclidean algorithm
#'
#' @description This function implements  is an efficient method for computing the greatest common divisor (GCD) of two integers (numbers).
#' @param num1 An integer representing the first number.
#' @param num2 An integer representing the second number.
#' Both numbers are used to calculate their greatest common divisor (GCD).
#' @return The greatest common divisor.
#' @examples
#' euclidean(num1 = 100, num2 = 1000)
#' 
#' @references
#' Wikipedia: \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{Euclidean Algorithm}
#' @export



euclidean <- function(num1,num2){
    stopifnot(is.numeric(num1)&length(num1)==1,is.numeric(num2)&length(num2)==1)
    num1 <- abs(num1)
    num2 <- abs(num2)
    if(num1>=num2){
        a <- num1
        b <- num2
    } else{
        b <- num1
        a <- num2
    }

    while(b != 0){
        temp <- b
        b <- a %% b
        a <- temp
    }
    return(a)
}
