#' Generate Samples from a given Sample, "bootstrap-wise," and create an accompanying detailed histograph
#'
#' You have at least 5 inputs.
#' 'iter' is the number of samples you want generated
#' 'x' is the input sample. Should be a vector of numbers.
#' 'fun' is the function that corresponds with the statistic you want each 'iter' sample to calculate. Some common inputs would be "mean" "sd" "median".)
#' 'alpha' is 1 minus the confidence interval you want calculated. So if you want a 95% Confidence Interval, 'alpha = 0.05'.
#' 'cx' is merely a variable that should be a numerical input that is used with the 'cex' parameter in making the histograph. It determines how large the symbols and letter in the graph are.
#' The output is a histograph of the statistic of each of the 'iter' samples.
#' Additionally, a vertical line showing the value of that statistic for the input sample, and labels displaying the confidence interval.
#' Additionally, 'myboot2()' will return a list containing the calculated confidence interval, the function corresponding to the calculated statistic, and the input sample. Note, this list is 'invisible' in that it won't display by default but only if explicitly invoked (to cut back on potentially displaying large screeds of data).
#'
#' @param iter Numeric, x Numeric, fun String, alpha Numeric, cx Numeric.
#'
#' @return List of the calculated confidence interval, the function corresponding to the calculated statistic, and the input sample.
#'
#' @examples
#' myboot2(10000, rnorm(20, mean = 10, sd = 4), fun = "mean", alpha = 0.05, cx = 1.5)
#'
#' @export
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))# Some output to use if necessary
}
