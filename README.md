# Функция для записи последовательности в одну строку:
```R
write_fasta <- function(x, file) {
  name <- trimws(paste0('>', names(x)))
  seqs <- as.character(x)
  nase <- paste(name, seqs, sep = '\n')
  write(nase, file = file)
}
```

# SemicircularDiagram
Улучшенная версия полукруглой диаграммы https://stackoverflow.com/a/47738295/22134124

## Пример использования
```R
library(ggforce)
library(tidyverse)
source('semicircle_diag.r', encoding = "utf-8")

df <- data.frame(
  group = c('A', 'V', 'C'),
  value = c(50, 30, 10)
)

myPalet <- RColorBrewer::brewer.pal(length(df$group), name = 'Set2')

parliament_plot <- semicircle_diag(df$group, df$value, myPalet, sort_order = 'descending')
print(parliament_plot)
```
Результат:
![alt text](https://github.com/SerhEL/SemicircularDiagram/blob/main/result.jpg?raw=true)


# RepairFastqc
Востанавливает структуру FASTQC файлов

## Пример
```R
source('repair_fastq.r', encoding = "utf-8")

f <- list.files('.', pattern = '.fastq')
f <- f[-grep('gz', f)]
n <- gsub('.fastq', '_repaired.fq', f)

for(i in seq_along(f)) {
  repair_fastq(f[i], n[i])  
}
```
# DownloadFile
Повторно загружает файл если было разорвано соединение

## Пример
```R
source('download_file.r', encoding = "utf-8")

meta <- read.xlsx('Salmonella_Genomes.xlsx')
link <- paste0(
  'https://api.ncbi.nlm.nih.gov/datasets/v2alpha/genome/accession/',
  meta$Assembly.Accession, '/download?include_annotation_type=GENOME_FASTA'
  )
file <- paste0('./download/', meta$Assembly.Accession, '.zip')

for (i in 1:nrow(meta)) {
  if (!file.exists(file[i])) {
    res <- download_file(link[i], file[i])
    print(paste0('Загружен: ', res, ' - ', i))
  }
}
```
# Запросы в NCBI
```R
# Разбиваем данные на части по 100 элементов
split_accessions <- split(meta_grp$assembly_accession, ceiling(seq_along(meta_grp$assembly_accession) / 100))

# Функция для отправки запросов в NCBI, возвращает максимум 100 json объектов
search_accessions <- function(accessions, len_object = 100) {
  url <- "https://www.ncbi.nlm.nih.gov/datasets/api/datasets/v2/genome/dataset_report"
  
  headers <- c(
    "Accept" = "application/json, text/plain, */*",
    "Content-type" = "application/json",
    "Cookie" = "ncbi_sid=7C70BB0E7A1F9433_17565SID; gdh-data-hub-csrftoken=JzbX24aCIIHNKFoPc9k1CD6Z3a8uciZ7; QSI_SI_9p2ci2cOSG1dkA5_intercept=true; cebs=1; _ce.clock_data=481%2C5.167.22.115%2C1%2C78b89f3f9f8543608d90756d5737e138%2CYandex%2CRU; cebsp_=1; WebEnv=12WYpj%407C70BB0E7A1F9433_17565SID; _ce.s=v~a7454762c6c663dfbae083a0e3fc8295fc1d5b34~lcw~1740212236749~vir~returning~lva~1740212234098~vpv~9~v11.cs~156325~v11.s~6cab8ad0-f0f5-11ef-bbc2-49c26d84e3fd~v11.sla~1740212237628~v11.send~1740212236749~lcw~1740212237628; ncbi_pinger=N4IgDgTgpgbg+mAFgSwCYgFwgCIAYBCeAnAGJH4Ac2AjBQOwDCArLq7kwKIBsAzHdtiJ0uXAIIA6auIC2caiAC+QA===; ncbi_sid=7C70BB0E7A1F9433_17565SID",
    "ncbi-phid" = "D0BD09F9B8D187C500005E637DD9766A.1.m_1.03",
    "origin" = "https://www.ncbi.nlm.nih.gov",
    "referer" = paste0('https://www.ncbi.nlm.nih.gov/datasets/genome/?accession=', paste0(accessions, collapse = ',')),
    "x-csrftoken" = "JzbX24aCIIHNKFoPc9k1CD6Z3a8uciZ7"
  )
  
  # Формируем тело запроса
  body <- list(
    page_size = len_object,
    page_token = "",
    returned_content = "COMPLETE",
    sort = list(),
    accessions = as.list(accessions),
    filters = list(
      assembly_version = "all_assemblies",
      exclude_paired_reports = FALSE
    )
  )
  
  # Выполняем запрос
  response <- POST(url, add_headers(.headers = headers), body = body, encode = 'json')
  
  # Проверяем статус ответа
  if (status_code(response) == 200) {
    result <- content(response, as = "parsed", type = "application/json")[["reports"]]
    return(result)
  } else {
    stop("Ошибка запроса: ", status_code(response))
  }
}
search_accessions_safe <- function(accessions, len_object = 100, retries = 10, sleep = 10) {
  attempt <- 1
  repeat {
    res <- tryCatch({
      search_accessions(accessions, len_object)  # ваш основной запрос
    }, error = function(e) {
      message("Ошибка при запросе (попытка №", attempt, "): ", conditionMessage(e))
      return(NULL)
    })
    
    if (!is.null(res)) {
      return(res)  # успешный результат
    }
    
    if (attempt >= retries) {
      warning("Достигнут лимит попыток для: ", paste(accessions, collapse = ", "))
      return(NULL)
    }
    
    attempt <- attempt + 1
    Sys.sleep(sleep)  # пауза между попытками
  }
}
```
# Функция для простой чистки ридов по количеству N и GC составу
```R
fastq_gcN_filter('S250093277_L01_96_1.fq.gz', 'S250093277_L01_96_2.fq.gz', 'S250093277_L01_96')
```
