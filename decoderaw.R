# Decodificación de archivos RAW con R
# www.overfitting.net


# FUNCIONES RAW

LoadRAW = function(filename, verbose=T, scale=T, integer=F) {
  # Lee archivo RAW cargándolo en una matriz
  # scale=T: sustrae offset de negro y escala saturación al fondo de escala
  #          en otro caso no escala valores numéricos RAW
  # integer=F: datos RAW en rango coma flotante 0..1
  #            en otro caso rango entero 0..65535 (16 bits)
  library(tiff)  # Para leer el TIFF generado por DCRAW
  
  cmd=paste0("dcraw ",  # Construimos comando de línea DCRAW
      iif(verbose, "-v ", ""),
      iif(scale, "-d -r 1 1 1 1", "-D"),  # RAW escalado (-d) o puro (-D)
      " -t 0 -4 -T ", filename)  # Sin rotación (-t), lineal (-4), TIFF (-T)
  if (verbose) cat(paste0(cmd, "\n"))  # Mostrar comando en consola
  system(cmd)
  
  raw=readTIFF(paste0(substr(filename, 1, nchar(filename)-4), ".tiff"))
  if (integer) raw=round(raw*65535)
  if (verbose) cat(paste0("Options: scale=", scale,
      ", integer=", integer, "\n"))
  return (raw)
}

WhitebalanceRAW = function(raw, pattern="RG/GB",
    wb=c(2.13248, 1.000000, 1.000000, 1.480864)) {  # Luz de día Canon 350D
  # Aplica balance de blancos (requiere RAW)
  NROW=nrow(raw)
  if (NROW%%2 | ncol(raw)%%2 |
      !(pattern %in% c("RG/GB", "BG/GR", "GR/BG", "GB/RG"))) return (-1)
  
  if        (pattern=="RG/GB") { mul=c(1,2,3,4)
  } else if (pattern=="BG/GR") { mul=c(4,2,3,1)
  } else if (pattern=="GR/BG") { mul=c(2,1,4,3)
  } else {                       mul=c(2,4,1,3)
  }

  # Índices Bayer: se calculan y almacenan solo una vez
  i=which(row(raw)%%2 & col(raw)%%2)  # Fotosito R de un patrón RG/GB
  raw[i]       =raw[i]       *wb[mul[1]]
  raw[i+NROW]  =raw[i+NROW]  *wb[mul[2]]
  raw[i+1]     =raw[i+1]     *wb[mul[3]]
  raw[i+NROW+1]=raw[i+NROW+1]*wb[mul[4]]
  
  # if (max(raw))>1 raw=raw/max(raw)  # Normalizamos a 1
  
  return(raw)
}

DebayerRAW = function(raw, pattern="RG/GB", averageG=T) {
  # Deshace patrón de Bayer (requiere RAW)
  NROW=nrow(raw)
  if (NROW%%2 | ncol(raw)%%2 |
      !(pattern %in% c("RG/GB", "BG/GR", "GR/BG", "GB/RG"))) return (-1)
  
  # Índices Bayer: se calculan y almacenan solo una vez
  i=which(row(raw)%%2 & col(raw)%%2)  # Fotosito R de un patrón RG/GB
  if (averageG) {  # Devuelve {R, (G1+G2)/2, B}
    if        (pattern=="RG/GB") {
      img=c(raw[i], (raw[i+NROW]+raw[i+1])/2, raw[i+NROW+1])
    } else if (pattern=="BG/GR") {
      img=c(raw[i+NROW+1], (raw[i+NROW]+raw[i+1])/2, raw[i])
    } else if (pattern=="GR/BG") {
      img=c(raw[i+NROW], (raw[i]+raw[i+NROW+1])/2, raw[i+1])
    } else {
      img=c(raw[i+1], (raw[i]+raw[i+NROW+1])/2, raw[i+NROW])
    }
    dim(img)=c(dim(raw)/2, 3)
  } else {  # Devuelve {R, G1, G2, B}
    if        (pattern=="RG/GB") {
      img=c(raw[i], raw[i+NROW], raw[i+1], raw[i+NROW+1])
    } else if (pattern=="BG/GR") {
      img=c(raw[i+NROW+1], raw[i+NROW], raw[i+1], raw[i])
    } else if (pattern=="GR/BG") {
      img=c(raw[i+NROW], raw[i], raw[i+NROW+1], raw[i+1])
    } else {
      img=c(raw[i+1], raw[i], raw[i+NROW+1], raw[i+NROW])
    }
    dim(img)=c(dim(raw)/2, 4)    
  }
  
  return(img)
}

ShowRAW = function(img, trunc=T, gamma=2.2, interpolate=F) {
  # Muestra RAW en pantalla (admite RAW y de-bayer)
  # Solo si trunc=F y la imagen excede de 1 se reescala a 1
  if (length(dim(img))>2 & dim(img)[3]>3) {
    img=img[,,c(1:2,4)]
    warning("Using G1 sub-channel, G2 sub-channel ignored")
  }
  img[img<0]=0
  if (trunc) img[img>1]=1
  plot(as.raster((img / max(max(img),1))^(1/gamma), max=1),
       interpolate=interpolate)
}

HistRAW = function(img, gamma=1, breaks=512+1, filename="") {  # breaks=bins+1
  # Calcula histogramas RAW (requiere de-bayer)
  if (length(dim(img))>2 & dim(img)[3]>3) {
    img=img[,,c(1:2,4)]
    warning("Using G1 sub-channel, G2 sub-channel ignored")
  }
  xmax=max(1,max(img))
  xrange=seq(from=0, to=xmax, length.out=breaks)
  hr=hist(img[,,1]^(1/gamma), breaks=xrange, plot=F)
  hg=hist(img[,,2]^(1/gamma), breaks=xrange, plot=F)
  hb=hist(img[,,3]^(1/gamma), breaks=xrange, plot=F)

  ymax=max(c(hr$counts, hg$counts, hb$counts))
  hr$counts=hr$counts/ymax
  hg$counts=hg$counts/ymax
  hb$counts=hb$counts/ymax
  
  if (filename != "") {
      if (tolower(substr(filename, nchar(filename)-3,
          nchar(filename))) != ".png") filename=paste0(filename, ".png")
      png(filename, width=breaks+129, height=512, antialias='default')
      }
  plot(hg, xlim=c(0,xmax), ylim=c(0,1), col=rgb(0,1,0,1/1),
      border=rgb(0,1,0,0),
      main=paste0("RAW histogram ", "(gamma=", gamma, ")"),
      xlab="RGB range", ylab='')
  plot(hr, xlim=c(0,xmax), ylim=c(0,1), col=rgb(1,0,0,1/2),
      border=rgb(1,0,0,0), add=T)
  plot(hb, xlim=c(0,xmax), ylim=c(0,1), col=rgb(0,0,1,1/3),
      border=rgb(0,0,1,0), add=T)
  abline(h=c(0,1), v=c(0,xmax), col='gray')
  if (filename != "") dev.off()
}

SaveRAW = function(img, filename, trunc=T, gamma=2.2) {
  # Guarda RAW en formato PNG (admite RAW y de-bayer)
  # Solo si trunc=F y la imagen excede de 1 se reescala a 1
  library(tiff)
  img[img<0]=0
  if (trunc) img[img>1]=1
  if (tolower(substr(filename, nchar(filename)-3,
      nchar(filename))) != ".tif") filename=paste0(filename, ".tif")
  writeTIFF((img / max(max(img),1))^(1/gamma), filename,
      bits.per.sample=16, compression="LZW")
}

iif = function(condicion, val1, val2) {
  if (condicion) return(val1)
  return(val2)
}


# EJERCICIO

# Lee RAW
raw=LoadRAW("namibia.cr2")
SaveRAW(raw, filename="raw.tif", gamma=1)

# De-bayering
img=DebayerRAW(raw)
SaveRAW(img, filename="debayer.tif", gamma=1)

# Histograma RAW
HistRAW(img)
HistRAW(img, filename="histogramaraw.png")

# Balance de blancos
imgwb=DebayerRAW(WhitebalanceRAW(raw))
SaveRAW(imgwb, filename="debayerwb.tif", gamma=1)

# Correlaciones
imgRGGB=DebayerRAW(raw, averageG=F)

# Generamos dataframe de 4 columnas: R, G1, G2, B
dim(imgRGGB)=c(dim(imgRGGB)[1]*dim(imgRGGB)[2], dim(imgRGGB)[3])
df=as.data.frame(imgRGGB)
colnames(df)=c('R','G1','G2','B')

library(qgraph)
qgraph(cor(df), edge.labels=T,
    shape="square", posCol="darkgreen", negCol="darkred",
    layout=matrix(c(0,1,0,1,1,1,0,0), nrow=4, ncol=2), vsize=20)

smoothScatter(df$G1, df$G2, xlab='G1', ylab='G2',
    nrpoints=0, nbin=600, xlim=c(0,1), ylim=c(0,1),
    colramp=colorRampPalette(c("white", "lightgrey", "grey", "darkgrey",
        "black", "red", "yellow", "white")) )

smoothScatter(df$G1, df$B, xlab='G1', ylab='B',
    nrpoints=0, nbin=600, xlim=c(0,1), ylim=c(0,1),
    colramp=colorRampPalette(c("white", "lightgrey", "grey", "darkgrey",
        "black", "red", "yellow", "white")) )

