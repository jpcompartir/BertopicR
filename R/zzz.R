test_is_python_object <- function(object){
  return(any(grepl("python\\.builtin\\.object", methods::S3Class(object))))
}
