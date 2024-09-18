repair_fastq <- function(input, output) {
  # Открываем файл для чтения и записи
  rcon <- file(input, open = 'r')
  wcon <- file(output, open = 'w')
  
  read_count <- 0
  # Читаем строки по 6 (для одного рида в FASTQ формате)
  while (length(read_data <- readLines(rcon, n = 6, warn = FALSE)) > 0) {
    read_count <- read_count + 1
    
    repaired_read <- c()
    repaired_read[1] <- read_data[1]                    # Заголовок рида
    repaired_read[2] <- paste0(read_data[2], read_data[3])  # Объединение последовательностей
    repaired_read[3] <- read_data[4]                    # Плюс строка
    repaired_read[4] <- paste0(read_data[5], read_data[6])  # Объединение качественных оценок

    # Удаляем символы возврата каретки \r
    repaired_read <- gsub("\r", "", repaired_read)
    
    writeLines(repaired_read, wcon)                     # Запись результата в новый файл
    
    if (read_count == 1) {
      print(read_data)
      print(repaired_read)
    }
  }
  
  # Закрываем файлы после обработки
  close(rcon)
  close(wcon)
  
  return(read_count)
}
