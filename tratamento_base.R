## =============================================================================
## SCRIPT DE TRATAMENTO DA BASE DE DADOS
## =============================================================================

## INSTALACAO DE PACOTES
pacotes <- c("dplyr", "tidyr", "stringr")
faltando <- pacotes[!pacotes %in% installed.packages()[, "Package"]]
if (length(faltando) > 0) install.packages(faltando)

## IMPORT DE PACOTES
library(dplyr)
library(tidyr)
library(stringr)

## LER CSV =====================================================================

# ---- 1. Importar a base ----
# Testamos os separadores mais comuns e escolhemos o que resulta em mais
# de 1 coluna (evita importar tudo "grudado" numa unica coluna por engano).
caminho_arquivo <- "~/Documentos/Estatística/CE07 - PROJETOS/dados_alunos_status_disciplinas_.csv"

testar_separador <- function(sep) {
  tryCatch({
    linha <- read.table(caminho_arquivo, sep = sep, nrows = 1,
                        stringsAsFactors = FALSE, encoding = "UTF-8")
    ncol(linha)
  }, error = function(e) 0)
}

seps_candidatos <- c(",", ";", ".", "\t")
n_colunas_por_sep <- sapply(seps_candidatos, testar_separador)
cat("\n===== TESTE DE SEPARADOR =====\n")
print(setNames(n_colunas_por_sep, seps_candidatos))

sep_escolhido <- seps_candidatos[which.max(n_colunas_por_sep)]
cat("Separador escolhido automaticamente:", sep_escolhido, "\n")
cat("(Se estiver errado, defina 'sep_escolhido' manualmente)\n")

base_alunos <- read.csv(caminho_arquivo,
                        sep = sep_escolhido,
                        stringsAsFactors = FALSE,
                        encoding = "UTF-8",
                        check.names = FALSE)

## TRATAR/SEPARAR COLUNAS ======================================================

# NOME_PROGRAMA vem como "CURSO - MODALIDADE - CIDADE"; separamos em tres
# colunas quando o "-" existir. extra = "merge" evita perder pedaco caso
# o nome do curso tambem tenha um "-" dentro dele.
base_alunos <- base_alunos %>%
  separate(NOME_PROGRAMA, into = c("CURSO", "MODALIDADE", "CIDADE"),
           sep = " - ", extra = "merge", fill = "right") %>%
  rename(
    STATUS         = STATUS_DESCRICAO,
    DEPARTAMENTO   = NOME_DEPARTAMENTO,
    SETOR          = SETOR_PROGRAMA,
    CAMPUS         = CAMPUS_PROGRAMA,
    PERIODO_LETIVO = PERIODOLETIVO
  )


## FILTRAR/ REMOVER TCCs & ESTAGIOS ============================================

# Normaliza maiuscula/minuscula e remove acentos antes de comparar, para
# que "Estágio", "ESTAGIO" e "estagio" caiam todos no mesmo padrao.

normalizar_texto <- function(x) {
  x <- str_to_upper(x)
  iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
}

TERMOS_TCC_ESTAGIO <- c(
  "TCC", "TFC", "TFG",
  "TRABALHO DE CONCLUSAO",
  "MONOGRAFIA",
  "ESTAGIO",
  "PRATICA PROFISSIONAL"
)

base_alunos_final <- base_alunos %>%
  filter(!str_detect(normalizar_texto(NOME_DISCIPLINA),
                      paste(TERMOS_TCC_ESTAGIO, collapse = "|")))

## DIMENSOES FINAIS ============================================================

cat("\nDimensoes finais:", nrow(base_alunos_final), 
    "linhas x", ncol(base_alunos_final), "colunas\n")

## =============================================================================
## FIM
## =============================================================================
