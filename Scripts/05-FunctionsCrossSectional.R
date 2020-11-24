### Analisis-COVID-MP2.5
## Funciones para generar Figuras/Tablas de un modelo Transversal ajustado
## PBH Julio 2020


foot_note <- "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"

f_tableCoef <- function(model, preview="none", highlight=F){
  # est <- cbind(est=coef(mod), confint(mod))
  est <- summary(model)$coefficients[,1:4] %>% as.data.frame() %>% 
    as_tibble(rownames = "parametro")
  names(est) <- c("parametro","coef","sd","z_value","p_value")
  
  ## Add codes
  est <- est %>% mutate(codes=case_when(
    p_value<0.001 ~ "***",
    p_value<0.01 ~ "**",
    p_value<0.05 ~ "*",
    p_value<0.1 ~ ".",
    T ~ ""))
  
  ## Tabla coeficientes
  table <- est %>% 
    mutate(parametro=parametro %>% 
             str_remove_all("scale|\\(|\\)|log") %>% 
             f_replaceVar() %>% 
             str_replace("rmRM","Comuna dentro RM")) %>% 
    rename(Parametro=parametro, `Coef.`=coef, `Desv.`=sd,
           `Valor-z`=z_value,`Valor-p`=p_value,`Sign.`=codes) %>% 
    flextable() %>% 
    colformat_num(big.mark=" ", digits=4, j=2:5,
                  na_str="s/i") %>% 
    bold(bold=T, part="header") %>% 
    autofit(add_w = 0.1, add_h = 0.3) %>%
    align(j=1, align = "left", part="all") %>% 
    footnote(j=6, value=as_paragraph(foot_note), part="header", inline=T)
  
  if (highlight){
    table <- table %>% bold(bold=T, i = ~`Valor-p`<=0.05)
  } else {
    table <- table %>% bold(j=1, bold=T)
  }
  
  
  # Retorno tabla
  if (preview=="docx"){
    return(table %>% print(preview="docx"))
  } else if(preview=="pptx"){
    return(table %>% print(preview="pptx"))
  } else {
    return(table)
  }
  
}

## Funcion para generar tabla con MRR estimados. 
# Opcion preview permite abrir la tabla en word (docx) o en ppt (pptx)
# Opcion highlight destaca variables con significancia al 5%
f_tableMRR <- function(model, preview="none", highlight=F){
  est <- summary(model)$coefficients[,1:4] %>% as.data.frame() %>% 
    as_tibble(rownames = "parametro")
  names(est) <- c("parametro","coef","sd","z_value","p_value")
  
  ## Add codes
  est <- est %>% mutate(codes=case_when(
    p_value<0.001 ~ "***",
    p_value<0.01 ~ "**",
    p_value<0.05 ~ "*",
    p_value<0.1 ~ ".",
    T ~ ""))
  
  # Calculate MRR
  est <- est %>% 
    mutate(ci=confint(model, method="Wald", level=0.95) %>% na.omit(),
           low=ci[,1], high=ci[,2], ci=NULL)
  est_mrr <- est[-1,] %>% mutate(coef=exp(coef) %>% round(2), 
                                 low=exp(low) %>% round(2), 
                                 high=exp(high) %>% round(2),
                                 ci=paste("(",format(low,digits=2),
                                          ", ",format(high,digits=2),")",sep = ""),
                                 p_value=round(p_value,4))
  
  # Tabla MRR
  table <- est_mrr %>% 
    dplyr::select(parametro, coef, ci, p_value, codes) %>% 
    mutate(parametro=parametro %>% 
             str_remove_all("scale|\\(|\\)|log") %>% 
             f_replaceVar() %>% 
             str_replace("rmRM","Comuna dentro RM")) %>% 
    rename(Variable=parametro, MRR=coef, `95% I.C.`=ci,
           `Valor-p`=p_value,`Sign.`=codes) %>% 
    flextable() %>% 
    bold(bold=T, part="header") %>% 
    autofit(add_w = 0.1, add_h = 0.3) %>%
    align(j=1, align = "left", part="all") %>% 
    footnote(j=5, value=as_paragraph(foot_note), part="header", inline=T)
  
  if (highlight){
    table <- table %>% bold(bold=T, i = ~`Valor-p`<=0.05)
  } else {
    table <- table %>% bold(j=1, bold=T)
  }
  
  # Retorno tabla
  if (preview=="docx"){
    return(table %>% print(preview="docx"))
  } else if(preview=="pptx"){
    return(table %>% print(preview="pptx"))
  } else {
    return(table)
  }
  
}

f_figMRR <- function(model){
  # est <- cbind(est=coef(mod), confint(mod))
  est <- summary(model)$coefficients[,1:4] %>% as.data.frame() %>% 
    as_tibble(rownames = "parametro")
  names(est) <- c("parametro","coef","sd","z_value","p_value")
  
  ## Add codes
  est <- est %>% mutate(codes=case_when(
    p_value<0.001 ~ "***",
    p_value<0.01 ~ "**",
    p_value<0.05 ~ "*",
    p_value<0.1 ~ ".",
    T ~ ""))
  
  # Calculate MRR
  est <- est %>% 
    mutate(ci=confint(model, method="Wald", level=0.95) %>% na.omit(),
           low=ci[,1], high=ci[,2], ci=NULL)
  est_mrr <- est[-1,] %>% mutate(coef=exp(coef) %>% round(2), 
                                 low=exp(low) %>% round(2), 
                                 high=exp(high) %>% round(2),
                                 ci=paste("(",format(low,digits=2),
                                          ", ",format(high,digits=2),")",sep = ""),
                                 p_value=round(p_value,4))
  
  ## Figure MRR
  p <- est_mrr %>% 
    rowid_to_column() %>% 
    mutate(parametro=parametro %>% 
             str_remove_all("scale|\\(|\\)|log") %>% 
             f_replaceVar() %>% 
             str_replace("rmRM","Comuna dentro RM")) %>% 
    ggplot(aes(x=reorder(parametro,desc(rowid)), y=coef))+
    geom_point()+
    geom_errorbar(aes(ymin=low, ymax=high))+
    geom_hline(yintercept = 1, linetype = "dashed")+
    labs(x="",y="MRR")+
    coord_flip()
  
  return(p)
}

## Para calcular MRR del MP2.5 con su intervalo de confianza
f_MRR_mp25 <- function(mod, param="mp25"){
  # Get coefficients
  est <- summary(mod)$coefficients[,1:4] %>% as.data.frame() %>% 
    as_tibble(rownames = "parametro") %>% 
    filter(parametro==param)
  names(est) <- c("parametro","coef","sd","z_value","p_value")
  
  # Calculate MRR
  est <- est %>% 
    mutate(ci=confint(mod, method="Wald", level=0.95) %>% na.omit() %>% 
             as.data.frame() %>% as_tibble(rownames = "parametro") %>% 
             filter(parametro==param),
           low=ci[,2] %>% unlist(), high=ci[,3] %>% unlist(), ci=NULL)
  
  est_mrr <- est %>% mutate(RR=exp(coef) %>% round(4), 
                            lower_CI=exp(low) %>% round(4), 
                            upper_CI=exp(high) %>% round(4)) %>% 
    dplyr::select(RR, lower_CI, upper_CI)
  return(est_mrr)
}



## EoF