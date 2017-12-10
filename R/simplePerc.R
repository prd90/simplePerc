#' @title Simple Perceptron
#' @description  A single layer perceptron
#' @param X Data frame with numeric columns
#' @param yt Training labels (numeric)
#' @param act.fn Activation function. Accepts "step","relu", "sigmoid", "tanh" and "gauss". Step is the default value.
#' @param niter Number of iterations. Default is 20
#' @param lrate Learning rate. Default is 0.6
#' @return A list of errors at each iteration in classification
#' @examples
#' err = simplePerc(X,yt,"step",30,0.4)
#' @seealso
#' \link[mlbench]{Sonar}
#' Sonar data set
#' @export
simplePerc <- function(X, yt, act.fn="step", niter=20, lrate=0.6) {

   weight <- matrix(rep(0,ncol(X)), ncol = ncol(X))
   delwt <- matrix(rep(0,ncol(X)), ncol = ncol(X))


  bias <-  1
  errors <- rep(0, niter)

  if(!is.numeric(yt))
     as.numeric(as.character(yt))

  if(act.fn=="step")
  {
   for(outer in 1:niter)
    {
      for(inner in 1:length(yt))
      {

         z <- (sum(weight[1, ]* X[inner, ]) + bias)

         #ACTIVATION STEP
         if(z <= 0)
         {
           yc <- 0
         }
         else
           yc <- 1

         #delta calculation
         delta <- yt[inner] - yc

         #weight difference
           temp <- lrate * delta * X[inner, ]
           #str(temp)
           delwt <- rbind(temp,delwt)
          # str(delwt)
           delwt <- delwt[-2,]

           #update the weights
          weight <- weight + delwt


           if ((yt[inner] - yc) != 0.0)
             {
                     errors[outer] <- errors[outer] + 1
             }
      }
    }
  }

  else if(act.fn=="sigmoid")
  {
    for(outer in 1:niter)
    {
      for(inner in 1:length(yt))
      {

        z <- (sum(weight[1, ]* X[inner, ]) + bias)

        #Sigmoid Activation Fn calculation
        yc <- 1 / (1+exp(-z))

        #delta calculation
        delta <- yt[inner] - yc

        #weight difference
        temp <- lrate * delta * X[inner, ]
        #str(temp)
        delwt <- rbind(temp,delwt)
        # str(delwt)
        delwt <- delwt[-2,]

        #update the weights
        weight <- weight + delwt


        if ((yt[inner] - yc) != 0.0)
        {
          errors[outer] <- errors[outer] + 1
        }
      }
    }
  }

  else if(act.fn=="tanh")
  {
    for(outer in 1:niter)
    {
      for(inner in 1:length(yt))
      {

        z <- (sum(weight[1, ]* X[inner, ]) + bias)

        #Sigmoid Activation Fn calculation
        yc <- (exp(-z) - exp(z)) / (exp(-z) + exp(z))

        #delta calculation
        delta <- yt[inner] - yc

        #weight difference
        temp <- lrate * delta * X[inner, ]
        #str(temp)
        delwt <- rbind(temp,delwt)
        # str(delwt)
        delwt <- delwt[-2,]

        #update the weights
        weight <- weight + delwt


        if ((yt[inner] - yc) != 0.0)
        {
          errors[outer] <- errors[outer] + 1
        }
      }
    }
  }

  else if(act.fn=="gauss")
  {
    for(outer in 1:niter)
    {
      for(inner in 1:length(yt))
      {

        z <- (sum(weight[1, ]* X[inner, ]) + bias)

        #Sigmoid Activation Fn calculation
        yc <- exp(-((z^2)/2))

        #delta calculation
        delta <- yt[inner] - yc

        #weight difference
        temp <- lrate * delta * X[inner, ]
        #str(temp)
        delwt <- rbind(temp,delwt)
        # str(delwt)
        delwt <- delwt[-2,]

        #update the weights
        weight <- weight + delwt


        if ((yt[inner] - yc) != 0.0)
        {
          errors[outer] <- errors[outer] + 1
        }
      }
    }
  }

  else if(act.fn=="relu")
  {
    for(outer in 1:niter)
    {
      for(inner in 1:length(yt))
      {

        z <- (sum(weight[1, ]* X[inner, ]) + bias)

        #Sigmoid Activation Fn calculation
        yc <- max(0,z)

        #delta calculation
        delta <- yt[inner] - yc

        #weight difference
        temp <- lrate * delta * X[inner, ]
        #str(temp)
        delwt <- rbind(temp,delwt)
        # str(delwt)
        delwt <- delwt[-2,]

        #update the weights
        weight <- weight + delwt


        if ((yt[inner] - yc) != 0.0)
        {
          errors[outer] <- errors[outer] + 1
        }
      }
    }
  }
  else
    stop("Please enter a valid activation function value.")

  print("Final weight values.....")
   print(weight)
   return(errors)
}



