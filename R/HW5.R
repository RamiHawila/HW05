#'  Calculates the third side of a right triangle based on the Pythagorean Theorem.
#' @description  Calculates the length of the third side of a right triangle when two sides are inputted. c is the hypotenuse, a is the height, and b is the base.
#' @export
#' @param a,b,c numeric
#' @examples
#' pythagorean(a = 3 , b=4)
#' pythagorean(a=3 , c=5)
#' @author Rami Hawila



# Building a function for the Pythagorean Theorem.

pythagorean = function(a = NULL,b = NULL ,c  = NULL){
  sides = c(a,b,c)



  # Produce an error for any negative inputs.

  if (any(sides<0))
  {
    message("Error: Sides of a triangle should have positive lengths.")
  }





  # Produce an error when there are at least two inputs missing.

  else if((is.null(a)&is.null(b)) ||  (is.null(a) & is.null(c)) || (is.null(b) & is.null(c)) || (is.null(a) & is.null(b) & is.null(c)))
  {
    message("Error:Please include two sides to the function.")
  }





  #Produce an error when there are sides that aren't numeric.


  else if(!is.numeric(sides))
  {
    message("Error: Sides of a triangle must have numeric lengths.")
  }





  #Calculate the length of the hypotenuse when the height and base are inputted.


  else if(is.null(c))
  {
    message("The length of the hypotenuse is:")
    c = sqrt(a^2 + b^2)
    return(c)
  }





  #Calculate the length of the base when the hypotenuse and height are inputted.

  else if(is.null(b))
  {
    message("The length of the base is:")
    b = sqrt(c^2 - a^2)
    return(b)
  }






  #Calculate the length of the height when the hypotenuse and base are inputted.

  else if(is.null(a))
  {
    message("The length of the height is:")
    a = sqrt(c^2 - b^2)
    return(a)
  }
}











#' Calculates the mean of the vector when s smallest elements and l largest elements are removed from the vector.
#' @param x numeric vector
#' @param s number of smallest elements removed.
#' @param l number of largest elements removed.
#' @examples
#' trimmed_mean(c(1,2,3,4,5,6),1,2)
#' trimmed_mean(c(1,7,3,2,5,0.5,9,10),1,2)
#' @author Rami Hawila
#' @export
# Building a function that calculates special trimmed mean of a numeric vector.


trimmed_mean = function(x,s,l)
{


  # Produce an error if at least one of the elements are not numeric.

  if (!is.numeric(x))
  {
    message("Error: Elements of the vector x must all be numeric.")
  }




  #Produce the trimmed mean depending on the arguments s and l.

  else if (length(x)>=s+l+1)
  {
    x = sort(x)
    return(mean(x[-(c(1:s,(length(x)-l+1):length(x)))]))

  }
  else
  {
    message("Error: The length of the vector must be greater than s+l+1")
  }

}

