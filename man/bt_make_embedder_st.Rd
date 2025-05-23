% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embed.R
\name{bt_make_embedder_st}
\alias{bt_make_embedder_st}
\title{Create an embedding model using \href{https://www.sbert.net/docs/pretrained_models.html}{sentence-transformers}}
\usage{
bt_make_embedder_st(model)
}
\arguments{
\item{model}{Name of embedding model as a string (not case sensitive)}
}
\value{
an embedding model, formed according to the model defined, that can be input to be_do_embedding to create document embeddings
}
\description{
Initially this function is built upon the \code{sentence_transformers} Python library, but it may be expanded to accept other frameworks. You should feed in your documents as a list. You can use hardware accelerators e.g. GPUs, to speed up computation.
}
\examples{
embedder <- bt_make_embedder_st("all-miniLM-L6-v2")

embedder <- bt_make_embedder_st("aLL-minilm-l6-v2")
}
