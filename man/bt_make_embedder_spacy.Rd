% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embed.R
\name{bt_make_embedder_spacy}
\alias{bt_make_embedder_spacy}
\title{Create an embedding model using a model available from the \href{https://spacy.io/models}{Spacy Library}}
\usage{
bt_make_embedder_spacy(model, ..., prefer_gpu = TRUE, exclude = NULL)
}
\arguments{
\item{model}{The pipeline used to make predictions}

\item{...}{additional arguments to be sent to the spacy.load function}

\item{prefer_gpu}{if TRUE use gpu if available}

\item{exclude}{name of pipeline components to exclude}
}
\value{
an embedding model, formed according to the model defined, that can be input to be_do_embedding to create document embeddings
}
\description{
Create an embedding model using a model available from the \href{https://spacy.io/models}{Spacy Library}
}
\examples{
\donttest{
# specify a non-transformer model, excluding features not required
embedder <- bt_make_embedder_spacy(model = "en_core_web_md", exclude = c("tagger", "parser", "ner", "attribute_ruler", "lemmatizer"))

# specify a transformer model and exclude features not required
embedder <- bt_make_embedder_spacy(model = "en_core_web_trf", exclude = c("tagger", "parser", "ner", "attribute_ruler", "lemmatizer"))
}
}
