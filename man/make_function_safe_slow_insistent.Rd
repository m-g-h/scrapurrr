% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrapurrr.R
\name{make_function_safe_slow_insistent}
\alias{make_function_safe_slow_insistent}
\title{Makes a function slow (delay before it's called) insistent (retries
execution on fail) and safe (return error message instead of stopping)}
\usage{
make_function_safe_slow_insistent(.function, delay = 0, attempts = 3)
}
\arguments{
\item{.function}{A \code{function} to be made slow, insistent and safe}

\item{delay}{\code{numeric scalar} giving the number of seconds to wait
before the function is called}

\item{attempts}{\code{numeric scalar} giving the number of retry attempts
if the function fails}
}
\value{
Returns a \code{function}
}
\description{
Makes a function slow (delay before it's called) insistent (retries
execution on fail) and safe (return error message instead of stopping)
}
