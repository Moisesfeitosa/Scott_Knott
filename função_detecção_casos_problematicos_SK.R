detect_p2 <- function(table){
  #table$trt<- as.factor(table$trt)
  modelo1<- lm(media ~ trt, data= table)
  tukey_results<- emmeans(modelo1, specs= ~trt) %>% contrast(method= "tukey")
  
  final_results <- as.tibble(tukey_results)
  
  final_results <- final_results %>%
    rename(contraste = contrast, 
           p_valor = p.value,
           estimativa= estimate, 
           EP= SE, 
           GL= df, 
           razão.t= t.ratio)
  
  significant_data <- final_results %>%
    filter(p_valor <= 0.05) %>%
    filter(!str_detect(contraste, "Controle"))
  
  if(nrow(significant_data) == 0) {
    message("Não houve violações")
    return(NULL)
  } else {
    message("As seguintes violações foram encontradas (p ≤ 0.05):")
    return(significant_data)
  }

}

