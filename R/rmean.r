#' Fill with NA missing data
#'
#' \code{fillna} Missing data fill by NA
#'
#' @param x A xts data
#' @return Data fill NA
#'
#' @author Mahmut MUSLUM, \email{mmuslumm@@gmail.com}
#'
#' @export
fillna <- function(x, ...) UseMethod("fillna")

#'
#' @export
fillna.xts <- function(x) {
  ix <- zoo::index(x)
  dix <- diff(ix)
  unit <- attr(dix, "units")
  mix <- Mode(dix)
  i <- range(ix) #baslangıc ve bitiş
  a <- seq(i[1], i[2], paste(mix, unit))
  y <- merge(x, xts::xts(, a))
  return(y)
}

#' Finds the mode of xts object.
#'
#' \code{Mode} Mode of xts object
#'
#' @param x A xts data
#' @return
#'
#' @author Mahmut MUSLUM, \email{mmuslumm@@gmail.com}
#'
#' @export
Mode <- function(x) {
  # Finds the mode of the vector.
  # See: http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#' Calculate Mean by  time units
#'
#' \code{MeanBy} Calculate Mean by  time units
#'
#' @param x A xts data
#' @return Mean data
#'
#' @author Mahmut MUSLUM, \email{mmuslumm@@gmail.com}
#'
#' @export
MeanBy <- function(xdata, on, percNA=NULL) {

  cls <- class(xdata)

  mymean <- function(x, period, ...) {
    if(length(x)<(period*(100-percNA)/100)) return(NA)
    #   cat(length(x), " = ", period, " \ ")
    #   cat(x)
    #   stop()
    # Calculate NA counts and their rates to length of columns
    c1<-sapply(X=x, FUN=function(x) sum(is.na(x)))
    # Calculate length of each column.
    c2<-sapply(X=x, FUN=function(x) length(x))
    p <- (c1/c2)*100 # NA rate
    r<- (p > percNA) # if Na rate is less than 25%
    # Get the mean with removing NA
    res<-sapply(X=x, FUN=mean, na.rm=T)
    # If NA is TRUE, then replace the value
    # with NA.
    res[which(r == TRUE, arr.ind=TRUE)] <- NA
    #   print(res)
    return(res)
  }

  ep       <- xts::endpoints(xdata, on=on)
  # use data of previous minutes for hourly mean
  if(on=="hours") {
    ep     <- c(ep[1], (ep[c(-1,-length(ep))]+1), ep[length(ep)])
  }
  per      <- Mode(ep[2:length(ep)] - ep[1:(length(ep)-1)])
  x        <- if(!is.null(percNA))
    period.apply(xdata, INDEX=ep, FUN=mymean, per) else # with 25% criteria
      period.apply(xdata, INDEX=ep, FUN=mean, na.rm=T)    # with na.rm=T criteria

  index(x) <- if(on=="months")
    as.yearmon(index(x), format="%Y %m") else
      as.POSIXct(trunc(index(x), on))
  x[is.nan(x)] <- NA
  x <- na.trim(x,is.na="all")
  class(x) <- cls
  return(x)
}

