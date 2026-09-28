# script roteiro do BDEM - no repositório Projeto_BDEM_2016
# Antes de começar a fazer qualquer coisa:
# a) Coloque todos os arquivos postados no Classroom (já descompactados) dentro do repositório local Projeto_BDEM_2016
# b) commit este roteiro com a mensagem "dados, arquivos de texto e script roteiro BDEM" e envie para o repositório Projeto_BDEM_2016
# c) salve o script com outro nome (script_BDEM.R) e commit com a mensagem "script BDEM" e envie para o repositório Projeto_BDEM_2016

# Ao inserir os comandos em cada Tarefa de cada Etapa, mantenha as linhas de comentários e orientações colocadas pela professora


##################################
# ETAPA 1: BANCO DE DADOS DO SIM
##################################
# Você deve criar e estar na branch SIM antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1. Leitura do banco de dados SIM_2016 com 1309774 linhas e 87 colunas com o nome de dados_sim
# Verificar se a leitura foi feita corretamente e a estrutura dos dados

dados_sim = read.csv2(file ="SIM_2016.csv")
dados_sim

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SIM - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# As colunas serão: 1, 3, 9, 10, 11, 14, 17, 35, 47
# Nomes das respectivas variáveis: CONTADOR, TIPOBITO, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, CAUSABAS
library(dplyr)

dados_sim_1 = dados_sim|>
  select(1, 3, 9,10,11,14,17,35,47)

dados_sim_1

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SIM - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF

dados_sim_2 = dados_sim_1 |>
  filter(CODMUNRES %/% 10000 == 50)


# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11:8344      12:3763     13:16799    14:2157      15:38557     16:2995     17:7490
# 21:34362     22:19187    23:54276    24:21922     25:28041     26:66928    27:20769    28:13516     29:88094
# 31:135257    32:22868    33:141089   35:296359
# 41:74740     42:40270    43:87583
# 50:16749     51:17535    52:38074    53:12050 


# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SIM - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis:
# TIPOBITO, SEXO, RACACOR, ESC2010, TPMORTEOCO, CAUSABAS
vet = c('TIPOBITO', 'SEXO', 'RACACOR', 'ESC2010', 'TPMORTEOCO', 'CAUSABAS')

summary(dados_sim_2[vet])

dados_sim_2$TIPOBITO
dados_sim_2$SEXO
dados_sim_2$RACACOR
dados_sim_2$ESC2010
dados_sim_2$TPMORTEOCO
dados_sim_2$CAUSABAS

# Avalie também os valores das variável IDADE (não estranhe mas idade é composta de um dígito inicial que indica a unidade de medida)
# Unidades de medida a serem consideradas em IDADE: 0: minutos, 1: horas, 2: dias, 3: meses, 4: anos, 5: idade maior que 100 anos

dados_sim_2$IDADE
summary(dados_sim_2$IDADE)

# Atenção: a unidade de medida de IDADE no DICIONÀRIO do SIM está errada
# O propósito das avaliações acima é verificar se as categorias estão de acordo com o dicionário do SIM ou se aparecem categorias estranhas


# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SIM - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# Verifique o dicionário do SIM para identificar qual o código das categorias de cada variável
    
summary(dados_sim_2)
dados_sim_2 = dados_sim_2 |>
  mutate(across(where(is.numeric) & -CONTADOR, ~ na_if(., 9)),
         across(where(is.character), ~ na_if(., '9')))


# Em variáveis quantitativas como IDADE verificar se existem valores como 9999 para NA
dados_sim_2 = dados_sim_2 |>
  mutate(IDADE = na_if(IDADE,999))

# Ao terminar a Tarefa 5 commit com a mensagem "script BDEM - SIM - tarefas 1 a 5" e envie para o repositório Projeto_BDEM_2016


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a PRIMEIRA LETRA da legenda é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis dentro do banco de dados

dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c('Fetal','Não fetal'))
dados_sim_2$SEXO = factor(dados_sim_2$SEXO, levels = c(1,2), labels = c('Masculino', 'Feminino'))
dados_sim_2$RACACOR = factor(dados_sim_2$RACACOR, levels = c(1,2,3,4,5), labels = c('Branca', 'Preta','Amarela','Parda','Indígena'))
dados_sim_2$ESC2010 = factor(dados_sim_2$ESC2010, levels = c(0, 1,2,3,4,5), labels = c('Sem escolaridade', "Fundamental I", 'Fundamental II', 'Médio', 'Superior incompleto','Superior completo'))
dados_sim_2$TPMORTEOCO = factor(dados_sim_2$TPMORTEOCO, levels = c(1,2,3,4,5,8),
                                labels = c('Na gravidez',
                                           'No parto', 'No abortamento',
                                           'Até 42 dias após o término do parto',
                                           'De 43 dias a 1 ano após o término da gestação',
                                           'Não ocorreu nestes períodos'))
summary(dados_sim_2)

# Ao terminar a Tarefa 6 commit com a mensagem "script BDEM - SIM - tarefas 1 a 6" e envie para o repositório Projeto_BDEM_2016


# Tarefa 7. Criar um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 - SIM.pdf”
# Atenção: a ordem das variáveis do arquivo deve ser respeitada
library(dplyr)

SIM_MS = dados_sim_2 |>
  group_by(CODMUNRES) |>
  summarise(
    #	Identificadores do banco de dados
    ANO = 2016,
    NIVEL = "MUNICIPIO",
    
    # Informações gerais
    TO = n(),
    
    TORC = sum(complete.cases(dados_sim[dados_sim$CODMUNRES == first(CODMUNRES), ])),
    
    TORCR = sum(complete.cases(dados_sim_1[dados_sim_1$CODMUNRES == first(CODMUNRES), ])),
    
    TO_NN = sum(substr(CAUSABAS, 1, 1) %in% c("V", "W", "X", "Y"), na.rm = TRUE),
    
    TO_N = sum(!substr(CAUSABAS, 1, 1) %in% c("V", "W", "X", "Y"), na.rm = TRUE),
    
    TO_CB_I = sum(substr(CAUSABAS, 1, 1) %in% c("A", "B"), na.rm = TRUE),
    
    TO_CB_N = sum( substr(CAUSABAS, 1, 1) == "C" | 
                     (substr(CAUSABAS, 1, 3) >= "D00" & substr(CAUSABAS, 1, 3) <= "D89" & substr(CAUSABAS, 1, 3) != "D49"), na.rm = TRUE),
    
    TO_CB_C = sum( substr(CAUSABAS, 1, 3) >= "I00" & substr(CAUSABAS, 1, 3) <= "I99",na.rm = TRUE),
    
    TO_CB_R = sum( substr(CAUSABAS, 1, 3) >= "J00" & substr(CAUSABAS, 1, 3) <= "J99",na.rm = TRUE),
    
    TO_CB_O = sum(!substr(CAUSABAS, 1, 1) %in% c("V", "W", "X", "Y") &  !substr(CAUSABAS, 1, 1) %in% c("A", "B", "C", "I", "J") & !(substr(CAUSABAS, 1, 1) == "D" 
                                                                                                                                    & as.numeric(substr(CAUSABAS, 2, 3)) <= 48) & !(substr(CAUSABAS, 1, 1) == "D" & as.numeric(substr(CAUSABAS, 2, 3)) >= 50), na.rm = TRUE),
    
    TO_M = sum(SEXO == 'Masculino'),
    TO_F = sum(SEXO == 'Feminino'),
    
    TO_F_IF = sum( SEXO =='Feminino' & IDADE >= 415 & IDADE <= 449, na.rm = TRUE),
    
    # Informações fetais e neonatais
    
    TO_FT = sum(TIPOBITO == "Fetal"),
    
    TO_NT = sum(TIPOBITO == "Não fetal" & IDADE >= 200 & IDADE <= 227, na.rm = TRUE),
    
    TO_NT_P = sum(TIPOBITO == "Não fetal" & IDADE >= 200 & IDADE <= 206, na.rm = TRUE),
    
    TO_NT_T = sum(TIPOBITO == "Não fetal" & IDADE >= 207 & IDADE <= 227, na.rm = TRUE),
    
    TO_PNT = sum(TIPOBITO == 'Não fetal' & IDADE >=  288 & IDADE <= 331, na.rm = TRUE),
    
    TONT_B = sum(TIPOBITO == 'Não fetal'& IDADE >= 200 & IDADE<=227 &RACACOR == "Branca", na.rm = TRUE),
    
    TONT_PT = sum(TIPOBITO == 'Não fetal'& IDADE >= 200 & IDADE<=227 & RACACOR == "Preta", na.rm = TRUE),
    
    TONT_A = sum(TIPOBITO == 'Não fetal'& IDADE >= 200 & IDADE<=227 & RACACOR == "Amarela", na.rm = TRUE),
    
    TONT_PD = sum(TIPOBITO == 'Não fetal'& IDADE >= 200 & IDADE<=227 & RACACOR == "Parda", na.rm = TRUE),
    
    TONT_I = sum(TIPOBITO == 'Não fetal'& IDADE >= 200 & IDADE<=227 & RACACOR == "Indígena", na.rm = TRUE),
    
    #Informações maternas
    
    TO_MT = sum(TPMORTEOCO != "Não ocorreu nestes períodos", na.rm = TRUE),
    
    TO_MT_DG = sum(TPMORTEOCO == "Na gravidez",na.rm = TRUE),
    
    TO_MT_PT = sum(TPMORTEOCO == 'No parto',na.rm = TRUE),
    
    TO_MT_AB = sum(TPMORTEOCO == 'No abortamento',na.rm = TRUE),
    
    TO_MT_42 = sum (TPMORTEOCO == 'Até 42 dias após o término do parto', na.rm = TRUE),
    
    TO_MT_43 = sum(TPMORTEOCO == 'De 43 dias a 1 ano após o término da gestação',na.rm = TRUE),
    
    TO_MT_P =  sum(TPMORTEOCO == "Na gravidez" | TPMORTEOCO == "No parto" | TPMORTEOCO == "No abortamento" | TPMORTEOCO == "Até 42 dias após o término do parto", na.rm = TRUE),
    
    TO_MT_P_I = sum((TPMORTEOCO == "Na gravidez" | TPMORTEOCO == "No parto" | TPMORTEOCO == "No abortamento" | TPMORTEOCO == "Até 42 dias após o término do parto") & IDADE >= 415 & IDADE <= 449, na.rm = TRUE),
    
    TO_MT_P_ES = sum((TPMORTEOCO == "Na gravidez" | TPMORTEOCO == "No parto" | TPMORTEOCO == "No abortamento" | TPMORTEOCO == "Até 42 dias após o término do parto") & ESC2010 == "Sem escolaridade", na.rm = TRUE),
    
    TO_MT_P_EFI = sum((TPMORTEOCO == "Na gravidez" | TPMORTEOCO == "No parto" | TPMORTEOCO == "No abortamento" | TPMORTEOCO == "Até 42 dias após o término do parto") & ESC2010 == "Fundamental I", na.rm = TRUE),
    
    TO_MT_P_EFII = sum((TPMORTEOCO == "Na gravidez" | TPMORTEOCO == "No parto" | TPMORTEOCO == "No abortamento" | TPMORTEOCO == "Até 42 dias após o término do parto") & ESC2010 == "Fundamental II", na.rm = TRUE),
    
    TO_MT_P_EM = sum((TPMORTEOCO == "Na gravidez" | TPMORTEOCO == "No parto" | TPMORTEOCO == "No abortamento" | TPMORTEOCO == "Até 42 dias após o término do parto") & ESC2010 == "Médio", na.rm = TRUE),
    
    TO_MT_P_ESI = sum((TPMORTEOCO == "Na gravidez" | TPMORTEOCO == "No parto" | TPMORTEOCO == "No abortamento" | TPMORTEOCO == "Até 42 dias após o término do parto") & ESC2010 == "Superior incompleto", na.rm = TRUE),
    
    TO_MT_P_ESC = sum((TPMORTEOCO == "Na gravidez" | TPMORTEOCO == "No parto" | TPMORTEOCO == "No abortamento" | TPMORTEOCO == "Até 42 dias após o término do parto") & ESC2010 == "Superior completo", na.rm = TRUE)
    
  )|>
  select(ANO, NIVEL, CODMUNRES, TO, TORC, TORCR, TO_NN,TO_N, TO_CB_I,TO_CB_N, TO_CB_C, TO_CB_R,TO_CB_O, TO_M, TO_F, TO_F_IF, TO_FT, TO_NT,
         TO_NT_P, TO_NT_T, TO_PNT,TONT_B,TONT_PT,TONT_A,TONT_PD,TONT_I, TO_MT, TO_MT_DG,TO_MT_PT,TO_MT_AB,TO_MT_42,TO_MT_43,TO_MT_P,TO_MT_P_I,
         TO_MT_P_ES,TO_MT_P_EFI,TO_MT_P_EFII,TO_MT_P_EM,TO_MT_P_ESI, TO_MT_P_ESC)
SIM_MS
glimpse(SIM_MS)

# Ao terminar a Tarefa 7 commit com a mensagem "script BDEM - SIM - tarefas 1 a 7" e envie para o repositório Projeto_BDEM_2016


# Tarefa 8. Exportar o banco de dados com o nome SIM_UF.csv (Exemplo: SIM_RJ.csv)
write.csv2(SIM_MS, 'SIM_MS.csv')

# Ao terminar a Tarefa 8 fazer um commit com o comentário "dados SIM_UF 2016 e script - SIM - tarefas 1 a 8"  e envie para o repositório Projeto_BDEM_2016



####################################
# ETAPA 2: BANCO DE DADOS DO SINASC
####################################
# Você deve criar e estar na branch SINASC antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1. Leitura do banco de dados SINASC_2016 com 2857800 linhas e 61 colunas com o nome de dados_sinasc

dados_sinasc = read.csv2('SINASC_2016.csv')

dim(dados_sinasc)

# Verificar se a leitura foi feita corretamente e a estrutura dos dados
library(dplyr)

glimpse(dados_sinasc)

# Por uma questão de padronização coloque todos os nomes das variáveis em letra maiúscula,
# usando o comando names(dados_sinasc) = toupper(names(dados_sinasc))

names(dados_sinasc) = toupper(names(dados_sinasc))
glimpse(dados_sinasc)

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SINASC - tarefa 1" e envie para o repositório Projeto_BDEM_2016

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# As colunas serão 3, 4, 5, 6, 11, 12, 13, 14, 18, 20, 21, 22, 23, 34, 37, 43, 47, 58, 59, 60, 61
# Nomes das respectivas variáveis: CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO, 
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK, CONTADOR

dados_sinasc_1 = dados_sinasc|>
  select(3, 4, 5, 6, 11, 12, 13, 14, 18, 20, 21, 22, 23, 34, 37, 43, 47, 58, 59, 60, 61)

glimpse(dados_sinasc_1)


# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

dados_sinasc_2 = dados_sinasc_1 |>
  filter(CODMUNRES %/% 10000 == 50)

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 26602     12: 15773     13: 76703     14: 11376     15: 137681    16: 15521      17: 23870
# 21: 110493    22: 46986     23: 126246    24: 45366     25: 56083     26: 130733     27: 48164     28: 32218     29: 199830
# 31: 253520    32: 53413     33: 219129    35: 601437     
# 41: 155066    42: 95313     43: 141411
# 50: 42432     51: 53531     52: 95563     53: 43340 

dim(dados_sinasc_2)
glimpse(dados_sinasc_2)

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE) 
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON) 
table(dados_sinasc_2$PARIDADE) 
table(dados_sinasc_2$KOTELCHUCK)

# Avalie também os valores das variáveis quantitativas de IDADEMAE, SEMAGESTAC, APGAR5 e PESO

dados_sinasc_2 |> select('IDADEMAE', 'SEMAGESTAC', 'APGAR5', 'PESO') |> summary()

# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016

# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# Verifique o dicionário do SINASC para identificar qual o código das categorias de cada variável
# KOTELCHUCK = 9 significa "Não informado"   TPROBSON = 11 significa "Não classificado por falta de informação"
# Em variáveis quantitativas como IDADEMAE verificar se existem valores como 9999 para NA

dados_sinasc_2 = dados_sinasc_2 |>
  mutate(
    ESTCIVMAE = na_if(ESTCIVMAE, 9),
    GESTACAO   = na_if(GESTACAO, 9),
    GRAVIDEZ   = na_if(GRAVIDEZ, 9),
    PARTO      = na_if(PARTO, 9),
    SEXO       = na_if(SEXO,0),
    IDANOMAL = na_if(IDANOMAL,9),
    ESCMAE2010 = na_if(ESCMAE2010,9),
    TPAPRESENT = na_if(TPAPRESENT,9),
    KOTELCHUCK = na_if(KOTELCHUCK,9),
    TPROBSON = na_if(TPROBSON, 11),
    LOCNASC    = na_if(LOCNASC, 9),
    APGAR5 = na_if(APGAR5,99)
  )

# Ao terminar a Tarefa 5 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 5" e envie para o repositório Projeto_BDEM_2016


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da legenda é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis dentro do banco de dados

dados_sinasc_2 = dados_sinasc_2 |>
  mutate(
    LOCNASC = factor(LOCNASC,levels = c(1,2,3,4,5), labels = c('Hospital','Outros estabelecimentos de saúde','Domicílio','Outros','Aldeia indígena')),
    
    ESTCIVMAE = factor(ESTCIVMAE, levels = c(1:5), labels = c('Solteira','Casada','Viúva','Separada judicialmente/divorciada','União estável')),
    
    GESTACAO = factor(GESTACAO, levels= c(1:6), labels = c('Menos de 22 semanas', '22 a 27 semanas','28 a 31 semanas','32 a 36 semanas','37 a 41 semanas','42 semanas e mais')),
    GRAVIDEZ = factor(GRAVIDEZ, levels = c(1:3), labels = c('Única', 'Dupla','Tripla ou mais')),
    PARTO = factor(PARTO, levels = c(1,2), labels = c('Vaginal','Cesário')),
    SEXO = factor(SEXO,levels = c(1,2), labels = c('Masculino','Feminino')),
    RACACOR = factor(RACACOR, levels = c(1:5), labels = c('Branca','Preta','Amarela','Parda','Indígena')),
    IDANOMAL =  factor(IDANOMAL, levels = c(1,2), labels = c('Sim', 'Não')),
    ESCMAE2010 = factor(ESCMAE2010, levels = c(0:5), labels = c('Sem escolaridade','Fundamental I','Fundamental II','Médio','Superior incompleto','Superior completo')),
    RACACORMAE = factor(RACACORMAE, levels = c(1:5), labels = c('Branca','Preta','Amarela','Parda','Indígena')),
    TPAPRESENT = factor(TPAPRESENT, levels = c(1:3), labels = c('Cefálico','Pélvica ou podálica','Transversa')),
    # TPROBSON = factor(levels = c(1:10, levels = c()))
    PARIDADE = factor(PARIDADE, levels = c(0,1), labels = c('Nulípara','Multípara')),
    KOTELCHUCK = factor(KOTELCHUCK, levels = c(1,2,3,4,5),
                        labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))
  )


# Ao terminar a Tarefa 6 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 6" e envie para o repositório Projeto_BDEM_2016


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia

dados_sinasc_2 = dados_sinasc_2 |>
  mutate(
    F_PESO = factor(
      case_when(
        PESO < 2500 ~ "Baixo peso",
        PESO >= 2500 & PESO < 4000 ~ "Peso normal",
        PESO >= 4000 ~ "Macrossomia"
      ),
      levels = c("Baixo peso", "Peso normal", "Macrossomia"))
  )


# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+

dados_sinasc_2 = dados_sinasc_2 |>
  mutate(
    F_IDADE = cut(
      IDADEMAE,
      breaks = c(0, 14, 19, 24, 29, 34, 39, 44, 49, 100),
      labels = c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"),
    )
  )

# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal

dados_sinasc_2 = dados_sinasc_2 |>
  mutate(
    F_APGAR5 = factor(
      case_when(
        APGAR5 < 7 ~ "Baixo",
        APGAR5 >= 7 ~ "Normal"
      ),
      levels = c("Baixo", "Normal")
    )
  )


# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$PEREG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES

dados_sinasc_2 = dados_sinasc_2 |>
  mutate(
    PEREG = factor(
      case_when(
        CODMUNNASC == CODMUNRES ~ "Não",
        CODMUNNASC != CODMUNRES ~ "Sim"
      ),
      levels = c("Não", "Sim")
    )
  )


# nova variável: dados_sinasc_2$ESTCIV: Sem companheiro: ESTCIVMAE 1, 3 ou 4, Com companheiro: ESTCIVMAE 2 ou 5

dados_sinasc_2 = dados_sinasc_2 |>
  mutate(
    ESTCIV = factor(
      case_when(
        ESTCIVMAE %in% c('Solteira', 'Viúva', 'Separada judicialmente/divorciada') ~ "Sem companheiro",
        ESTCIVMAE %in% c('Casada', 'União estável') ~ "Com companheiro"
      ),
      levels = c("Sem companheiro", "Com companheiro")
    )
  )


# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator
summary(dados_sinasc_2)

# Ao terminar a Tarefa 7 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 7" e envie para o repositório Projeto_BDEM_2016


# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional

tabela_pig_brasil = read.csv2('Tabela_PIG_Brasil.csv')

dados_sinasc_2 = dados_sinasc_2 |>
  left_join(
    tabela_pig_brasil, 
    by = c("SEMAGESTAC", "SEXO")
  )


# Criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

dados_sinasc_2 = dados_sinasc_2 |>
  mutate(
    F_PIG = factor(
      case_when(
        (GRAVIDEZ == "Única") & PESO < PESO_P10 ~ "PIG",
        (GRAVIDEZ == "Única") & PESO >= PESO_P10 & PESO <= PESO_P90 ~ "AIG",
        (GRAVIDEZ == "Única") & PESO > PESO_P90 ~ "GIG"
      ),
      levels = c("PIG", "AIG", "GIG")
    )
  )
dados_sinasc_2

# Ao terminar a Tarefa 8 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 8" e envie para o repositório Projeto_BDEM_2016


# Tarefa 9. Criar um banco de dados, de nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 9 - SINASC.pdf”
# Atenção: a ordem das variáveis do arquivo deve ser respeitada

library(dplyr)
SINASC_MS = dados_sinasc_2 |>
  group_by(CODMUNRES)|>
  summarise( 
    
    # Descrição
    ANO = 2016,
    NIVEL = 'MUNICIPIO',
    #CODMUNRES = dados_sinasc_2$CODMUNRES,
    
    # Informações sobre os nascimentos
    TN = n(),
    TNRC = sum(complete.cases(dados_sinasc[dados_sinasc$CODMUNRES == first(CODMUNRES), ])),
    TNRCR = sum(complete.cases(dados_sinasc_1[dados_sinasc_1$CODMUNRES == first(CODMUNRES), ])),
    
    # Informações sobre as gestantes
    TGI_15 = sum(IDADEMAE < 15, na.rm = TRUE), 
    TGI_15_19 = sum( IDADEMAE >= 15 & IDADEMAE<=19, na.rm = TRUE),
    TGI_20_24 = sum( IDADEMAE >= 20 & IDADEMAE <= 24, na.rm = TRUE),
    TGI_25_29 = sum( IDADEMAE >= 25 & IDADEMAE <= 29, na.rm = TRUE),
    TGI_30_34 = sum( IDADEMAE >= 30 & IDADEMAE <= 34, na.rm = TRUE),
    TGI_35_39 = sum( IDADEMAE >= 35 & IDADEMAE <= 39, na.rm = TRUE),
    TGI_40_44 = sum( IDADEMAE >= 40 & IDADEMAE <= 44, na.rm = TRUE),
    TGI_45_49 = sum( IDADEMAE >= 45 & IDADEMAE <= 49, na.rm = TRUE),
    TGI_50 = sum( IDADEMAE >= 50, na.rm = TRUE),
    TGIF = sum(IDADEMAE >= 15 & IDADEMAE <= 49, na.rm = TRUE),
    IM_P25 = quantile(IDADEMAE, probs = 0.25, na.rm = TRUE),
    IM_P50 = quantile(IDADEMAE, probs = 0.5, na.rm = TRUE),
    IM_P75 = quantile(IDADEMAE, probs = 0.75, na.rm = TRUE),
    IM_MD = mean(IDADEMAE, na.rm = TRUE),
    IM_DP = sd (IDADEMAE, na.rm = TRUE),
    EM_S = sum(ESCMAE2010 == 'Sem escolaridade', na.rm =TRUE),
    EM_FI = sum(ESCMAE2010 == "Fundamental I", na.rm = TRUE),
    EM_FII = sum(ESCMAE2010 == "Fundamental II", na.rm = TRUE),
    EM_M = sum(ESCMAE2010 == "Médio", na.rm = TRUE),
    EM_SI = sum(ESCMAE2010 == "Superior incompleto", na.rm = TRUE),
    EM_SC = sum(ESCMAE2010 == "Superior completo", na.rm = TRUE),
    TGRC_B = sum(RACACORMAE == 'Branca', na.rm = TRUE),
    TGRC_PT = sum(RACACORMAE == 'Preta', na.rm = TRUE),
    TGRC_A = sum(RACACORMAE == 'Amarela', na.rm = TRUE),
    TGRC_PD = sum(RACACORMAE == 'Parda', na.rm = TRUE),
    TGRC_I = sum(RACACORMAE == 'Indígena', na.rm = TRUE),
    TGSC = sum(ESTCIV == 'Sem companheiro', na.rm = TRUE),
    TGCC = sum (ESTCIV == 'Com companheiro', na.rm = TRUE),
    TGPRI = sum(PARIDADE == 'Nulípara', na.rm = TRUE),
    TGNPRI = sum(PARIDADE == 'Multípara', na.rm = TRUE),
    
    # Informações sobre as gestações
    TGU = sum(GRAVIDEZ == 'Única', na.rm = TRUE),
    TGG = sum(GRAVIDEZ != 'Única', na.rm = TRUE),
    TGD_22 = sum(SEMAGESTAC < 22, na.rm = TRUE),
    TGD_22_27 = sum(SEMAGESTAC >= 22 & SEMAGESTAC <= 27, na.rm = TRUE),
    TGD_28_31 = sum(SEMAGESTAC >= 28 & SEMAGESTAC <= 31, na.rm = TRUE),
    TGD_32_36 = sum(SEMAGESTAC >= 32 & SEMAGESTAC <= 36, na.rm = TRUE),
    TGD_37_41 = sum(SEMAGESTAC >= 37 & SEMAGESTAC <= 41, na.rm = TRUE),
    TGD_42 = sum(SEMAGESTAC >= 42, na.rm = TRUE),
    TGD_PRT = sum(GESTACAO == '32 a 36 semanas' | GESTACAO == '28 a 31 semanas' | GESTACAO == '22 a 27 semanas' | GESTACAO == 'Menos de 22 semanas', na.rm = TRUE),
    TGD_AT = sum(GESTACAO == '37 a 41 semanas', na.rm = TRUE),
    TGD_PST = sum(GESTACAO == '42 semanas e mais', na.rm = TRUE),
    DG_P25 = quantile(SEMAGESTAC, probs = 0.25, na.rm = TRUE),
    DG_P50 = quantile(SEMAGESTAC, probs = 0.5, na.rm = TRUE),
    DG_P75 = quantile(SEMAGESTAC, probs = 0.75, na.rm = TRUE),
    DG_MD = mean(SEMAGESTAC, na.rm = TRUE),
    DG_DP = sd(SEMAGESTAC, na.rm = TRUE),
    TKC_NR = sum(KOTELCHUCK == 'Não realizou pré-natal', na.rm = TRUE),
    TKC_ID = sum(KOTELCHUCK == 'Inadequado', na.rm = TRUE),
    TKC_IT = sum(KOTELCHUCK == "Intermediário", na.rm = TRUE),
    TKC_AD = sum(KOTELCHUCK == "Adequado" , na.rm = TRUE),
    TKC_MAD = sum(KOTELCHUCK == "Mais que adequado",na.rm = TRUE),
    
    # Informações sobre o parto
    TGPRG_S = sum(PEREG == 'Sim', na.rm = TRUE),
    TGPRG_N = sum(PEREG == 'Não', na.rm = TRUE),
    TPV = sum(PARTO == 'Vaginal', na.rm = TRUE),
    TPC = sum(PARTO == 'Cesário', na.rm = TRUE),
    TRAP_C = sum(TPAPRESENT == 'Cefálico', na.rm = TRUE),
    TRAP_P = sum(TPAPRESENT == 'Pélvica ou podálica', na.rm = TRUE),
    TRAP_T = sum(TPAPRESENT == 'Transversa', na.rm = TRUE),
    TGROB_1 = sum(TPROBSON == 1, na.rm = TRUE),
    TGROB_2 = sum(TPROBSON == 2, na.rm = TRUE), 
    TGROB_3 = sum(TPROBSON == 3,na.rm = TRUE),
    TGROB_4 = sum(TPROBSON == 4, na.rm = TRUE),
    TGROB_5 = sum(TPROBSON == 5, na.rm = TRUE),
    TGROB_6 = sum(TPROBSON == 6, na.rm = TRUE),
    TGROB_7 = sum(TPROBSON == 7, na.rm = TRUE),
    TGROB_8 = sum(TPROBSON == 8, na.rm = TRUE),
    TGROB_9 = sum(TPROBSON == 9, na.rm = TRUE),
    TGROB_10 = sum(TPROBSON == 10, na.rm = TRUE),
    TNLOC_H = sum(LOCNASC == 'Hospital', na.rm = TRUE),
    TNLOC_ES = sum(LOCNASC == 'Outros estabelecimentos de saúde',na.rm = TRUE),
    TNLOC_D = sum(LOCNASC == 'Domicílio', na.rm = TRUE),
    TNLOC_O = sum(LOCNASC == 'Outros', na.rm = TRUE),
    TNLOC_AI = sum(LOCNASC == 'Aldeia indígena', na.rm = TRUE),
    
    # Informações sobre os recém-nascidos
    TRS_M = sum(SEXO == 'Masculino', na.rm = TRUE),
    TRS_F = sum(SEXO == 'Feminino', na.rm = TRUE),
    TRRC_B = sum(RACACOR == 'Branca', na.rm = TRUE),
    TRRC_PT = sum(RACACOR == 'Preta', na.rm = TRUE),
    TRRC_A = sum(RACACOR == 'Amarela', na.rm = TRUE),
    TRRC_PD = sum(RACACOR == 'Parda', na.rm = TRUE),
    TRRC_I = sum(RACACOR == 'Indígena', na.rm = TRUE),
    TRP_BP = sum(PESO < 2500, na.rm = TRUE),
    TRP_N = sum(PESO >= 2500 & PESO <4000, na.rm = TRUE),
    TRP_M = sum(PESO >= 4000, na.rm = TRUE),
    PESO_P25 = quantile(PESO, probs = 0.25, na.rm = TRUE),
    PESO_P50 = quantile(PESO, probs = 0.50, na.rm = TRUE),
    PESO_P75 = quantile(PESO, probs = 0.75, na.rm = TRUE),
    PESO_MD = mean(PESO, na.rm = TRUE),
    PESO_DP = sd(PESO, na.rm = TRUE),
    TRPIG_P = sum(GRAVIDEZ == 'Única' & F_PIG == 'PIG', na.rm = TRUE),
    TRPIG_A =  sum(GRAVIDEZ == 'Única' & F_PIG == 'AIG', na.rm = TRUE),
    TRPIG_G =  sum(GRAVIDEZ == 'Única' & F_PIG == 'GIG', na.rm = TRUE),
    TRAPG5_B = sum(APGAR5 < 7, na.rm = TRUE),
    TRAPG5_N = sum(APGAR5 >= 7, na.rm = TRUE),
    APG5_MD = mean(APGAR5, na.rm = TRUE),
    APG5_DP = sd(APGAR5, na.rm = TRUE),
    TRAC = sum(IDANOMAL == 'Sim', na.rm = TRUE),
    TRSAC = sum(IDANOMAL == 'Não', na.rm = TRUE)
  )|>
  select(
    ANO, NIVEL, CODMUNRES, TN, TNRC, TNRCR, TGI_15, TGI_15_19, TGI_20_24, 
    TGI_25_29, TGI_30_34, TGI_35_39, TGI_40_44, TGI_45_49, TGI_50, TGIF, 
    IM_P25, IM_P50, IM_P75, IM_MD, IM_DP, EM_S, EM_FI, EM_FII, EM_M, EM_SI, 
    EM_SC, TGRC_B, TGRC_PT, TGRC_A, TGRC_PD, TGRC_I, TGSC, TGCC, TGPRI, 
    TGNPRI, TGU, TGG, TGD_22, TGD_22_27, TGD_28_31, TGD_32_36, TGD_37_41, 
    TGD_42, TGD_PRT, TGD_AT, TGD_PST, DG_P25, DG_P50, DG_P75, DG_MD, DG_DP, 
    TKC_NR, TKC_ID, TKC_IT, TKC_AD, TKC_MAD, TGPRG_S, TGPRG_N, TPV, TPC, 
    TRAP_C, TRAP_P, TRAP_T, TGROB_1, TGROB_2, TGROB_3, TGROB_4, TGROB_5, 
    TGROB_6, TGROB_7, TGROB_8, TGROB_9, TGROB_10, TNLOC_H, TNLOC_ES, 
    TNLOC_D, TNLOC_O, TNLOC_AI, TRS_M, TRS_F, TRRC_B, TRRC_PT, TRRC_A, 
    TRRC_PD, TRRC_I, TRP_BP, TRP_N, TRP_M, PESO_P25, PESO_P50, PESO_P75, 
    PESO_MD, PESO_DP, TRPIG_P, TRPIG_A, TRPIG_G, TRAPG5_B, TRAPG5_N, 
    APG5_MD, APG5_DP, TRAC, TRSAC
  )

glimpse(SINASC_MS)


# Ao terminar a Tarefa 9 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 9" e envie para o repositório Projeto_BDEM_2016


# Tarefa 10. Exportar o banco de dados com o nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv)
# Ao terminar a Tarefa 10 commit com o comentário "dados SINASC_UF 2016 e script - SIM - tarefas 1 a 10"  e envie para o repositório Projeto_BDEM_2016



####################################
# ETAPA 3: BANCOS DE DADOS DO SIDRA
####################################
# Você deve criar e estar na branch SIDRA antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler os bancos de dados abaixo listados com os respectivos nomes
# dados_sidra_1 para população residente estimada - UF e municípios - 2016 - SIDRA - tabela_6579.csv
# dados_sidra_2 para população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv
# dados_sidra_3 para população residente censo 2010 - por faixa etária - UF - SIDRA - tabela_1552.csv
# dados_sidra_4 para população residente censo 2010 - por faixa etária e sexo - municípios - SIDRA - tabela_1552.csv
# Atenção que agora os arquivos têm nomes e códigos (com 7 dígitos) dos municípios (e alguns UF)

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados


# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SIDRA - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Criar uma nova variável de nome CODUF com os códigos da UF nos bancos dados_sidra_1, dados_sidra_2, dados_sidra_4


# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Selecionar em dados_sidra_ 1 a dados_sidra_4 a UF de responsabilidade do aluno 
# e chamar os bancos de dados, respectivamente por sidra_1, sidra_2, sidra_3 e sidra_4


# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4: Criar um banco de dados, de nome SIDRA_UF.csv (Exemplo: SIDRA_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 4 - SIDRA.pdf”

# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5:Exportar o banco de dados com o nome SIDRA_UF.csv (Exemplo: SIDRA_RJ.csv)
# Ao terminar a Tarefa 5 commit com o comentário "dados SIDRA_UF 2016 e script - SIDRA - tarefas 1 a 5"  e envie para o repositório Projeto_BDEM_2016


####################################
# ETAPA 4: BANCOS DE DADOS DO ATLAS
####################################
# Você deve criar e estar na branch ATLAS antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler os bancos de dados abaixo listados com os respectivos nomes
# codigos_IBGE_2010 para códigos dos municípios - 2010.csv
# dados_atlas_1 para IDHM - 2010 (CENSO) e 2016 (PNAD) - total e por sexo - UF - Atlas Brasil.csv
# dados_atlas_2 para IDHM - 2010 - municípios - Atlas Brasil.csv
# Atenção que agora alguns arquivos só têm os nomes dos municípios e das UFs, mas não têm os códigos

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - ATLAS - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2: Manipular o banco de dados e criar o banco de dados ATLAS_UF

# Criar o banco UF_codigo tipo tabela de correspondência
UF_codigo = data.frame(
  UF = c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins",
         "Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba",
         "Pernambuco","Alagoas","Sergipe","Bahia","Minas Gerais",
         "Espírito Santo","Rio de Janeiro","São Paulo","Paraná",
         "Santa Catarina","Rio Grande do Sul","Mato Grosso do Sul",
         "Mato Grosso","Goiás","Distrito Federal"),
  
  SIGLA = c("RO","AC","AM","RR","PA","AP","TO",
            "MA","PI","CE","RN","PB","PE","AL",
            "SE","BA","MG","ES","RJ","SP",
            "PR","SC","RS","MS","MT","GO","DF"),
  
  CODUF = c(11,12,13,14,15,16,17,
            21,22,23,24,25,26,27,
            28,29,31,32,33,35,
            41,42,43,50,51,52,53)
)

# Retirar de dados_atlas_1 a linha do Brasil e adicionar (com merge by UF) as colunas de UF_codigo

# Criar o banco linha_estado somente com as linhas da UF e com as seguintes colunas:
# ANO=2016, NIVEL=UF, CODMUNRES, IDHM_A, IDHM_CA, IDHM_CA_M e IDHM_CA_F 

# Selecionar de linha_estado a UF da responsabilidade do aluno por CODMUNRES

# Criar em dados_atlas_2 a coluna com UF

# Retirar (UF) da variável município

# Acrescentar em codigos_IBGE_2010 a variável CODUF baseado nos dois primeiros dígitos de CODMUNRES

# Acrescentar a codigos_IBGE_2010 as variáveis de UF_codigo (merge by CODUF)

# Associar dados_atlas_2 a codigos_IBGE_2010 e nomear o novo arquivo por atlas_municipio
# Neste caso o merge será by.x = c("município","UF") e by.y = c("município","SIGLA")

# Remover de atlas_municipio a coluna UF.y criada no merge

# Selecionar somente a UF de responsabilidade do aluno através dos dois primeiros dógitos de CODMUNRES

# Criar banco ATLAS_MUNICIPIO com as linhas dos municípios e com as seguintes variáveis:
# ANO=2016, NIVEL=MUNICIPIO, CODMUNRES, IDHM_A=NA, IDHM_CA, IDHM_CA_M=NA, IDHM_CA_F=NA

# Criar banco final ATLAS_UF "juntando" os bancos linha_estado e ATLAS_MUNICIPIO


# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - ATLAS - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Exportar o banco de dados com o nome ATLAS_UF.csv (Exemplo: ATLAS_RJ.csv)
# Ao terminar a Tarefa 3 commit com o comentário "dados ATLAS_UF 2016 e script - ATLAS - tarefas 1 a 3"  e envie para o repositório Projeto_BDEM_2016



####################################
# ETAPA 5: BANCOS DE DADOS DO SINISA
####################################
# Você deve criar e estar na branch SINISA antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler o bancos de dados abaixo listado com os respectivo nome
# dados_sinisa para agua e esgoto - município - 2016.csv
# Atenção que o arquivo tem códigos e nomes de municípios e muitos NAs. 
# Repare que os valores estão com o milhar indicado por ponto, o que não deve acontecer para o R não entender como decimal

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados
# Remover a pontuação de milhar e converter para formato numérico

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SINISA - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Reduzir dados_sinisa apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinisa_1

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SINISA - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Criar um banco de dados, de nome SINISA_UF.csv (Exemplo: SINISA_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 3 - SINISA.pdf”

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SINISA - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Exportar o banco de dados com o nome SINISA_UF.csv (Exemplo: SINISA_RJ.csv)
# Ao terminar a Tarefa 4 commit com o comentário "dados SINISA_UF 2016 e script - SINISA - tarefas 1 a 4"  e enviar para o repositório Projeto_BDEM_2016



################################
# ETAPA 6: CRIAÇÃO DE BDEM_UF
################################
# Você deve estar agora em main e antes de inserir qualquer comando desta ETAPA
# deverá fazer os merges de cada uma das 5 branches. A cada merge pode fazer o comentário "merge da branch TAL"
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Agregar os arquivos SIDRA_UF, ATLAS_UF, SINASC_UF, SIM_UF, SINISA_UF no banco BDEM_UF (Exemplo: BDEM_RJ)
# Leitura dos 5 bancos de dados expeortados das etapas anteriores

# Agregação dos bancos
# Lembre-se que SIDRA e ATLAS tem CODMUNRES com 7 dígitos e SINASC, SIM e SINISA com 6 dígitos
# Além disso dentro do merge all = TRUE garante a manutenção de qualquer município presente em um dos bancos envolvidos no merge


# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - BDEM - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2: Inserir os seguintes indicadores epidemiológicos (com apenas dias casas decimais) no BDEM_UF:
# TFG: Taxa de fecundidade geral
# TMG: Taxa de mortalidade geral
# RMM: Razão de mortalidade materna
# TMM: Taxa de mortalidade materna
# TMM_P: Taxa de mortalidade materna em até 42 dias
# TMN: Taxa de mortalidade neonatal
# TMN_P: Taxa de mortalidade neonatal precoce
# TMN_T: Taxa de mortalidade neonatal tardia
# TMI: Taxa de mortalidade infantil

# Conferir o banco BDEM_UF após inserção dos indicadores

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - BDEM - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3: Exportar o banco de dados com o nome BDEM_UF.csv (Exemplo: BDEM_RJ.csv)
# Ao terminar a Tarefa 3 commit com o comentário "dados BDEM_UF 2016 e script - BDEM - tarefas 1 a 3"  e enviar para o repositório Projeto_BDEM_2016
 