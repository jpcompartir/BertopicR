% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embed.R
\name{bt_make_embedder}
\alias{bt_make_embedder}
\title{Create an embedding model}
\usage{
bt_make_embedder(model_name)
}
\arguments{
\item{model_name}{Name of embedding model as a string (not case sensitive)}
}
\value{
a Python object
}
\description{
Initially this function is built upon the \code{sentence_transformers} Python library, but it may be expanded to accept other frameworks. You should feed in your documents as a list. You can use hardware accelerators e.g. GPUs, to speed up computation.
}
\examples{
embedder <- bt_make_embedder("all-mpnet-base-v2")

embedder <- bt_make_embedder("aLL-minilm-l6-v2")
}
