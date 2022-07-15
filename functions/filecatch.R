findfiles <- function(outpath, dest) {
  all <- list.files(outpath, full.names=TRUE, recursive=TRUE)
  lapply(all, function(e) {
    contents <- tryCatch({
    if (file.size(e) > 0) {
      read_csv(e, col_names = TRUE)
    } else {file.copy(e, dest)}
    }, error = function(err) {
    return(NULL)
    })

    if(!is.null(contents)) {
    delete.columns <- grep("(^X)", colnames(contents), perl=T)
    if (length(delete.columns) > 0) {
      dir.create(file.path(dest, "missing_header"))
      file.copy(e, file.path(dest, "missing_header"))
    }
    if(filetype == "raw") {
    if (length(delete.columns) > 0) {
      if(ncol(contents) > 5) {
        names(contents) <- c("Time","RadioId","TagId","TagRSSI","NodeId","Validated")
      } else {names(contents) <- c("Time","RadioId","TagId","TagRSSI","NodeId")}
      }
      v <- ifelse(any(colnames(contents)=="Validated"), 2, 1)
    correct <- ifelse(v < 2, 5, 6)
    indx <- count.fields(e, sep=",")
    if(any(indx != correct)) {
      file.copy(e, dest)
    }
    }
  } else {file.copy(e, dest)}
  })
}