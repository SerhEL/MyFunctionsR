library(openxlsx)
library(MALDIquant)
library(MALDIquantForeign)

inp_dir <- '2025-08-08_III_gproup/'
out_dir <- '2025-08-08_III_gproup_Масс-листы/'

datapath <- list.dirs(inp_dir, recursive = F, full.names = TRUE)
i <- 1

for (i in seq_along(datapath)) {
  # создаем новый Excel workbook
  wb <- createWorkbook()
  
  dir_name <- basename(datapath[i])
  
  cells <- importBrukerFlex(datapath[i], verbose=FALSE)
  
  j <- 1
  for (j in seq_along(cells)) {
    
    if (length(cells) > 3) {
      if (j == 4) break
    }
    
    cell <- cells[[j]]
    
    # Статистические данные
    sm <- data.frame(
      Statistic = names(summary(mass(cell))),
      Mass = as.numeric(summary(mass(cell))),
      Intensity = as.numeric(summary(intensity(cell)))
    )
    
    cell_name <- paste0('Statistic ', j)
    addWorksheet(wb, cell_name)
    writeData(wb, cell_name, sm)
    
    # raw данные
    raw.data <- data.frame(
      mass = mass(cell),
      intensity = intensity(cell)
    )
    cell_name <- paste0('Raw data ', j)
    addWorksheet(wb, cell_name)
    writeData(wb, cell_name, raw.data)
    
    # обработка
    s2 <- transformIntensity(cell, method="sqrt")
    s3 <- smoothIntensity(s2, method="MovingAverage", halfWindowSize=2)
    s4 <- removeBaseline(s3, method="SNIP")
    p <- detectPeaks(s4)
    peak.data <- data.frame(
      mass = mass(p),
      intensity = intensity(p)
    )
    cell_name <- paste0('Peak data all', j)
    addWorksheet(wb, cell_name)
    writeData(wb, cell_name, peak.data)
    
    # ----- сохраняем графики -----
    plotfile <- paste0(tempfile(), ".png")
    png(plotfile, width=1800, height=1100)
    par(
      mfrow=c(2,3),
      cex.main=2,   # увеличивает заголовки 
      cex.lab=2,    # увеличивает подписи осей
      cex.axis=2    # увеличивает деления осей
    )
    xl <- range(mass(cell))
    
    plot(cell, sub="", main="1: raw", xlim=xl)
    plot(s2, sub="", main="2: variance stabilisation", xlim=xl)
    plot(s3, sub="", main="3: smoothing", xlim=xl)
    plot(s4, sub="", main="4: baseline correction", xlim=xl)
    plot(s4, sub="", main="5: peak detection", xlim=xl)
    points(p)
    top20 <- intensity(p) %in% sort(intensity(p), decreasing=TRUE)[1:15]
    labelPeaks(p, index=top20, underline=TRUE, cex=1.6)
    plot(p, sub="", main="6: Top 15 peak", xlim=xl)
    labelPeaks(p, index=top20, underline=TRUE, cex=1.6)
    
    dev.off()
    
    # Сохраняем топ 20 пиков
    peak.data_top <- peak.data[top20,]
    cell_name <- paste0('Top 15 Peaks ', j)
    addWorksheet(wb, cell_name)
    writeData(wb, cell_name, peak.data_top)
    
    # вставляем картинку в Peak-лист
    insertImage(wb, sheet = cell_name,
                file = plotfile, width = 16, height = 8,
                startRow = 2, startCol = ncol(peak.data) + 2)
  }
  
  # сохраняем Excel
  saveWorkbook(wb, paste0(out_dir, dir_name, '.xlsx'), overwrite = TRUE)
}
