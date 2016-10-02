errorMatrix <- function(actual.class, predicted.class)
{
  x   <- table(actual.class, predicted.class)

   # Number of classes.
  
  nc  <- nrow(x)

  # Number of values.
  
  nv  <- length(actual.class) - sum(is.na(actual.class) | is.na(predicted.class))

  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))

  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")

  return(tbl)
}
