library(tidyverse)

df_morador <- read_fwf(
  file = "CAMINHO A INSERIR",
  col_positions = fwf_positions(
    start = c(7, 8,  17, 19, 20, 22, 33, 36, 37, 76, 92),
    end   = c(7, 16, 18, 19, 21, 23, 35, 36, 37, 77, 105),
    col_names = c("TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "V0306", "V0403", "V0404", "V0405", "ANOS_ESTUDO", "PESO_FINAL")
  ),
  col_types = cols(.default = col_double()),
  progress = FALSE
) %>% filter(V0306 == 1) %>% select(-V0306)

df_qv <- read_fwf(
  file = "CAMINHO A INSERIR",
  col_positions = fwf_positions(
    start = c(8,  17, 19, 20, 42, 94, 122),
    end   = c(16, 18, 19, 21, 61, 94, 141),
    col_names = c("COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "FUNCAO_PERDA", "V601", "RENDA_DISP_PC")
  ),
  col_types = cols(.default = col_double()),
  progress = FALSE
)

##### FUNDIR, LIMPAR E CRIAR AS DUMMIES
df_final <- inner_join(df_morador, df_qv, by = c("COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE")) %>%
  mutate(
    PESO_FINAL    = PESO_FINAL / 100000000,
    FUNCAO_PERDA  = FUNCAO_PERDA,
    RENDA_DISP_PC = RENDA_DISP_PC / 1000,
    Idade         = V0403,
    Dummy_Acesso_Financeiro = ifelse(V601 == 0, 1, 0),
    Dummy_Urbana  = ifelse(TIPO_SITUACAO_REG == 1, 1, 0),
    Dummy_Mulher  = ifelse(V0404 == 2, 1, 0),
    Dummy_Branco  = ifelse(V0405 == 1, 1, 0)
  ) %>%
  select(-V601, -TIPO_SITUACAO_REG, -V0404, -V0403, -V0405) %>%
  drop_na() %>%
  filter(RENDA_DISP_PC >= 0)


write.csv2(df_final, "CAMINHO A INSERIR", row.names = FALSE)
cat("Tamanho final da amostra limpa:", nrow(df_final), "observações.\n")

##### ========================================================================

if(!require(stargazer)) install.packages("stargazer")
library(stargazer)

##### Seleciona as 8 variáveis do modelo
df_estatisticas <- df_final %>% select(
  IPQV = FUNCAO_PERDA,
  Acesso_Financeiro = Dummy_Acesso_Financeiro,
  Renda_Per_Capita = RENDA_DISP_PC,
  Escolaridade_Anos = ANOS_ESTUDO,
  Area_Urbana = Dummy_Urbana,
  Chefe_Mulher = Dummy_Mulher,
  Idade_Anos = Idade,
  Chefe_Branco = Dummy_Branco
)

stargazer(as.data.frame(df_estatisticas), type = "html", 
          out = "CAMINHO A INSERIR", 
          title = "Tabela 1: Estatísticas Descritivas das Variáveis", 
          digits = 2, 
          summary.stat = c("n", "mean", "sd", "min", "max"))

##### ========================================================================

if(!require(sandwich)) install.packages("sandwich")
if(!require(lmtest))   install.packages("lmtest")
library(sandwich)
library(lmtest)

##### 1. Rodando a Regressão Linear Múltipla (MQO padrão)
modelo_mqo <- lm(FUNCAO_PERDA ~ Dummy_Acesso_Financeiro + RENDA_DISP_PC + ANOS_ESTUDO + Dummy_Urbana + Dummy_Mulher + Idade + Dummy_Branco, data = df_final)

##### 2. Calculando os erros padrão robustos à heterocedasticidade (HC1)
erros_robustos <- vcovHC(modelo_mqo, type = "HC1")

##### 3. Extraindo os erros padrão e p-valores robustos para o stargazer
ep_robustos <- sqrt(diag(erros_robustos))
pval_robustos <- coeftest(modelo_mqo, vcov = erros_robustos)[, 4]

stargazer(modelo_mqo, type = "html", 
          out = "CAMINHO A INSERIR", 
          title = "Tabela 2: Resultados da Regressão Linear Múltipla (MQO) — Erros Padrão Robustos (HC1)", 
          covariate.labels = c("Acesso Financeiro (1=Sim)", "Renda Per Capita", "Escolaridade (Anos)", "Área Urbana (1=Sim)", "Chefe Mulher (1=Sim)", "Idade do Chefe (Anos)", "Chefe Branco (1=Sim)"), 
          dep.var.labels = "IPQV (Privações)", 
          se = list(ep_robustos), 
          p  = list(pval_robustos), 
          notes = "Erros padrão robustos à heterocedasticidade (HC1) entre parênteses.", 
          notes.append = FALSE)

summary(modelo_mqo)
coeftest(modelo_mqo, vcov = erros_robustos)