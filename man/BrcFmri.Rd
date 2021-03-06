% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BrcFmri.R
\name{BrcFmri}
\alias{BrcFmri}
\title{BrcFmri constructor.}
\usage{
BrcFmri(data2d, id, parcellation)
}
\arguments{
\item{data2d}{A matrix representing an fMRI}

\item{id}{An identification string}

\item{parcellation}{A \code{BrcParcellation} object}
}
\value{
A new \code{BrcFmri} instance.
}
\description{
\code{BrcFmri} makes a new \code{BrcFmri} instance.
}
\details{
A BrcFmri dataype represents a functional magnetic resonance imaging (fMRI)
scan. It is a list with three named components:
\enumerate{
   \item \code{data2d}, the fMRI data matrix. Each column represents a voxel,
     or 3-dimensional pixel, in the fMRI. Each row represents a time step.
     Thus, one column of \code{data2d} is the time series for a single fMRI
     voxel, and one row of \code{data2d} is an fMRI image at one point in
     time.
   \item \code{id}, an identification string. It is not guaranteed to be
     unique or nonempty---those depend on how the \code{BrcFmri} object
     is constructed. Typically this field would be used to cross-reference
     the MRI with a dataframe containing phenotype information.
   \item \code{parcellation}, a \code{BrcParcellation} datatype. This
     contains the information necessary to transform the two-dimensional
     \code{data2d} matrix into the four-dimensional series of volumes useful
     for visualization.
}
Because \code{BrcFmri} is a dataype, not an object there are no accessor
functions. You can get its components directly with the \code{$} operator.'

If you are trying to make a new \code{BrcFmri} instance, the 
\code{buildBrcFmri} function will build the required 
\code{parcellation} argument for you and wire everything together. If you 
only have the four-dimensional fMRI data, then the \code{data4dTo2d}
function will transform it for you.
}
\seealso{
\code{\link{BrcParcellation}}, \code{\link{buildBrcFmri}},
 \code{\link{data4dTo2d}}
}

