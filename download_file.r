download_file <- function(link, file, retries = 5, sleep = 5) {
  name <- basename(file)
  attempt <- 1
  while (attempt <= retries) {
    tryCatch({
      download.file(link, file, method = 'curl', cacheOK = FALSE, quiet = TRUE)
      return(name)  # Возвращаем название файла
    }, error = function(e) {
      print(paste0('Ошибка при загрузке ', name, ', попытка № ', attempt))
      attempt <<- attempt + 1
      Sys.sleep(sleep)
    })
  }
  return(FALSE)
}
