### MortalityRR-PM2.5
## Functions common for all the scrips in the project
## PBH Nov 2020

# https://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

f_remover_acentos <- function(x){
  x %>% 
    str_replace_all("á","a") %>% 
    str_replace_all("é","e") %>% 
    str_replace_all("í","i") %>% 
    str_replace_all("ó","o") %>% 
    str_replace_all("ú","u") %>% 
    str_replace_all("ñ","n") %>% 
    str_replace_all("Ñ","N") %>% 
    str_replace_all("Á","A") %>% 
    str_replace_all("É","E") %>% 
    str_replace_all("Í","I") %>% 
    str_replace_all("Ó","O") %>% 
    str_replace_all("Ú","U")
}

f_split_n <- function(X,Sep,N){
  X %>% str_split(Sep) %>% sapply(function(x) x[N])
}

quartileHour <- function(x){
  retVal = 'NA'
  if (x<7){
    retVal='Hour: 00-06'
  }
  else if (x<13){
    retVal='Hour: 06-12'
  }
  else if (x<19){
    retVal='Hour: 12-18'
  }
  else{retVal='Hour: 18-24'}
  return(retVal)
}

## Source: https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
# https://www.estacionesdelano.com/chile/
getSeason <- function(dat) {
  stopifnot(class(dat) == "Date")
  scalarCheck <- function(dat) {
    m <- month(dat)      
    d <- day(dat)        
    if ((m == 9 & d >= 23) | (m == 10) | (m == 11) | (m == 12 & d < 21)) {
      r <- 1
      } else if ((m == 12 & d >= 21) | (m == 1) | (m == 2) | (m == 3 & d < 21)) {
        r <- 2
        } else if ((m == 3 & d >= 21) | (m == 4) | (m == 5) | (m == 6 & d < 21))
          {
          r <- 3
          } else {
            r <- 4
            }
    r
    }
    res <- sapply(dat, scalarCheck)
    res <- ordered(res, labels=c("spring", "summer", "fall", "winter"))
    invisible(res)
}


## Save Plot ----------
f_savePlot <- function(p1, file_path, dpi=600){
  cat("Saving: ",file_path)
  ggsave(file_path, {{p1}},dpi=dpi,
         width = 14.87, height = 9.30, units = "in")
}

## Save CSV --------
f_saveCsv <- function(datos, file_path){
  cat("Saving: ",file_path)
  cat('sep=; \n',file = file_path)
  write.table(datos, file_path,
              sep=';',row.names = F, append = T)
}



## Rename Variables ----------
# Function to change name of variables for display
# Must have Dictionary_Variables.xlsx loaded, as dictionary
dictionary <- read_excel("Data/Dictionary_Variables.xlsx",
                         sheet = "Dictionary")
f_replaceVar <- function(variable_orig){
  plyr::mapvalues(variable_orig,dictionary$variable,dictionary$description,
                  warn_missing = F)
}

## Type of  Variables ----------
# Function to add "group" to the variable, to classify by type
# Useful for presenting data in a sorted way
f_addTypeVar <- function(var){
  plyr::mapvalues(var,dictionary$variable,dictionary$group,
                  warn_missing = F) %>% 
      factor(levels=c("Mortality", "Air pollution exposure","Wood consumption",
                      "Demography", "Socioeconomic", "Meteorology", "Geographic"))
}


## Fuente: https://stackoverflow.com/questions/7508229/how-to-create-a-column-with-a-quartile-rank
## Returns a vector divided into quartiles
qgroup = function(numvec, n = 5){
  qtile = quantile(numvec, probs = seq(0, 1, 1/n), na.rm=T)
  out = sapply(numvec, function(x) sum(x >= qtile[-(n+1)]))
  out=paste("Q",out,sep="") %>% factor(levels=paste("Q",1:n,sep=""))
  return(out)
}
