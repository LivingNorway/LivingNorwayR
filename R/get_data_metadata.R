#' get_data_metadata
#' read the metadata from a rmarkdown file
#' @param X an RMarkdown file
#' @return Output: text string of yaml information
#' @export

get_data_metadata = function(file) {
  x = readr::read_lines(file) # read markdown using readlines
  rng = grep("^---$", x)
  rng = rng + c(1, -1)
  x = x[rng[1]:rng[2]]
  names(x) = gsub("(.*):.*", "\\1", x)
  x = gsub(".*: (.*)", "\\1", x)
  as.list(x)
}


