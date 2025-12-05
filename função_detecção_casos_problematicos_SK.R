library(readxl)
library(tidyverse)
library(multcompView)
library(emmeans)
library(DHARMa)
library(multcomp)
library(gvlma)
library(DescTools)
library(ScottKnott)

# importando os dados
teste<- read_xlsx("C:/Users/moise/OneDrive/Documentos/teste.xlsx")

teste$media<- as.numeric(teste$media)
teste$trt<- as.factor(teste$trt)


modelo1<- lm(media ~ trt, data= teste)
summary(modelo1)
modelo2<-lm(media ~ trt, data= teste2)
anova(modelo1)
plot(modelo1)
ggqqplot(modelo1$residuals)
gvlma(modelo1)
simulateResiduals(modelo1, plot= TRUE)

# conduzindo o tukey
emmeans(modelo1, specs = ~trt) %>%
  cld(method= "tukey", Letters= letters, alpha= 0.05, decreasing= TRUE)


# contraste usando DescTools
todos<- DunnettTest(x= teste$media, g= teste$trt)


# agora excluindo o controle
excl<- teste %>%
  filter(trt != "Controle")


emmeans(modelo1, specs= ~trt) %>% contrast(method= "tukey")
?pairs
contraste_exc<- emmeans(modelo2, specs= ~trt) %>% pairs()
contraste_todos
contraste_exc



# conduzindo um Scott-Knott
summary(SK(x= modelo1, which= "trt"))
detect_p(tabela_contraste)



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
