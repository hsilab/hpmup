% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/u_05_Satisfaction.R
\name{GetDesire}
\alias{GetDesire}
\title{GetDesire()}
\usage{
GetDesire(dim, wgt, wear, safe, dura, control, comfort)
}
\arguments{
\item{dim}{Dimension: size, height, length, width}

\item{wgt}{Weight}

\item{wear}{Ease in adjusting (fixing or fastening) the parts of the assistive device}

\item{safe}{Safety or security}

\item{dura}{Durability (endurance or resistance to wear)}

\item{control}{Ease of use}

\item{comfort}{Comfortability}
}
\value{
The average of desire
}
\description{
Return the average of desire
}
\examples{
GetDesire(dim, wgt, wear, safe, dura, control, comfort)
}
