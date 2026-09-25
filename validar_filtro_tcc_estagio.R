## =============================================================================
## VALIDACAO DO FILTRO DE TCC/ESTAGIO
## Roda depois de tratamento_base.R (usa base_alunos = antes do filtro,
## e base_alunos_final = depois do filtro, que ja existem nesse ponto).
## =============================================================================

## INSTALACAO DE PACOTES
pacotes <- c("dplyr", "stringr", "readr")
faltando <- pacotes[!pacotes %in% installed.packages()[, "Package"]]
if (length(faltando) > 0) install.packages(faltando)

library(dplyr)
library(stringr)
library(readr)

normalizar_texto <- function(x) iconv(str_to_upper(x), 
                                      from = "UTF-8", 
                                      to = "ASCII//TRANSLIT")

TERMOS_TCC_ESTAGIO <- c("TCC", "TFC", "TFG", "TRABALHO DE CONCLUSAO",
                        "MONOGRAFIA", "ESTAGIO", "PRATICA PROFISSIONAL")

## 0. Quanto foi removido ======================================================
n_antes  <- nrow(base_alunos)
n_depois <- nrow(base_alunos_final)
cat("===== 0. VOLUME REMOVIDO =====\n")
cat("Linhas antes do filtro: ", n_antes, "\n")
cat("Linhas depois do filtro:", n_depois, "\n")
cat("Linhas removidas:       ", n_antes - n_depois, "\n\n")

disciplinas_antes  <- sort(unique(base_alunos$NOME_DISCIPLINA))
disciplinas_depois <- sort(unique(base_alunos_final$NOME_DISCIPLINA))
disciplinas_removidas <- setdiff(disciplinas_antes, disciplinas_depois)

cat("Disciplinas distintas antes: ", length(disciplinas_antes), "\n")
cat("Disciplinas distintas depois:", length(disciplinas_depois), "\n")
cat("Disciplinas removidas:       ", length(disciplinas_removidas), "\n\n")

## 1. Removeu de menos? (sobrou TCC/estagio em base_alunos_final) ==============
cat("===== 1. REMOVEU DE MENOS? (deveria dar 0 linhas) =====\n")
sobrou <- base_alunos_final %>%
  filter(str_detect(normalizar_texto(NOME_DISCIPLINA), 
                    paste(TERMOS_TCC_ESTAGIO, collapse = "|")))

if (nrow(sobrou) == 0) {
  cat("OK - nenhuma disciplina com esses termos restou na base final.\n\n")
} else {
  cat("ATENCAO -", nrow(sobrou), "linha(s) ainda batem com o padrao:\n")
  print(sort(unique(sobrou$NOME_DISCIPLINA)))
  cat("\n")
}

## 2. Removeu de mais? (disciplina removida que nao parece TCC/estagio) ========
cat("===== 2. REMOVEU DE MAIS? (revisar cada nome abaixo) =====\n")
if (length(disciplinas_removidas) == 0) {
  cat("Nenhuma disciplina foi removida, filtro nao encontrou nada para tirar.\n")
} else {
  cat("Disciplinas efetivamente removidas pelo filtro:\n")
  print(disciplinas_removidas)
  cat("\n-> Leia a lista acima: se algum nome nao for claramente um TCC ou\n")
  cat("   estagio, o filtro pegou algo por engano (falso positivo) e o\n")
  cat("   TERMOS_TCC_ESTAGIO em tratamento_base.R precisa ficar mais especifico\n")
  cat("   (ex.: trocar 'SUPERVISIONAD' por algo mais restrito, se ele estiver\n")
  cat("   pegando disciplinas de 'ESTAGIO SUPERVISIONADO EM ...' que na verdade\n")
  cat("   voce quer manter).\n")
}

## Criar lista de disciplinas excluidas ========================================

disciplinas_removidas <- base_alunos %>%
  filter(!NOME_DISCIPLINA %in% base_alunos_final$NOME_DISCIPLINA) %>%
  count(NOME_DISCIPLINA, name = "n_linhas_na_base") %>%
  arrange(desc(n_linhas_na_base))

# write_csv(disciplinas_removidas, "disciplinas_removidas_tcc_estagio.csv")


cat("\n===== RESUMO =====\n")
cat("Filtro validado se: secao 1 deu OK (nao sobrou nada) e todos os nomes\n")
cat("da secao 2 forem, de fato, disciplinas de TCC ou estagio.\n")
