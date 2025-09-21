fastq_gcN_filter <- function(read1, read2 = NULL, pfile = 'out', pN = 0, GC = c(20, 80)) {
  library(stringi)
  
  # Чтение ридов
  con_r1 <- gzfile(read1, "rt")
  con_l1 <- readLines(con_r1)
  close(con_r1)
  
  if (!is.null(read2)) {
    con_r2 <- gzfile(read2, "rt")
    con_l2 <- readLines(con_r2)
    close(con_r2)
  }
  
  # Файлы для записи
  if (is.null(read2)) {
    out_con_r1 <- file(paste0(pfile, '.fq'), "wt")
  } else {
    out_con_r1 <- file(paste0(pfile, '_R1.fq'), "wt")
    out_con_r2 <- file(paste0(pfile, '_R2.fq'), "wt")
    out_con_un <- file(paste0(pfile, '_UN.fq'), "wt")
  }
  
  # Проверка рида
  check_seq_gcN <- function(seqs, pN = 0, GC = c(20, 80)) {
    cN  <- stri_count_regex(seqs, '[Nn]') / nchar(seqs) * 100
    cGC <- stri_count_regex(seqs, '[GC]') / nchar(seqs) * 100
    bad <- (cN > pN) || (cGC <= GC[1]) || (cGC >= GC[2])
    return(bad)
  }
  
  len_read <- length(con_L1) / 4
  for (i in seq_len(len_read)) {
    p1 <- i * 4 - 3
    p4 <- i * 4
    
    st1 <- con_L1[p1:p4]
    bad1 <- check_seq_gcN(st1[2], pN, GC)
    
    if (!is.null(read2)) {
      st2 <- con_L2[p1:p4]
      bad2 <- check_seq_gcN(st2[2], pN, GC)
      
      if (!bad1 && !bad2) {
        writeLines(st1, out_con_r1)
        writeLines(st2, out_con_r2)
      } else if (!bad1 && bad2) {
        writeLines(st1, out_con_un)
      } else if (bad1 && !bad2) {
        writeLines(st2, out_con_un)
      }
    } else {
      if (!bad1) {
        writeLines(st1, out_con_r1)
      }
    }
  }
  
  close(out_con_r1)
  if (!is.null(read2)) {
    close(out_con_r2)
    close(out_con_un)
  }
}
