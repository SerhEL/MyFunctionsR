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
