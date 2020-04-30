######################################################################
###   Tutorial Programação Funcional em R para Cientistas Sociais  ###
######################################################################


setwd("~/Dropbox/repositories/tutorial_r_funcional")


#criando um banco com dados aleatórios

set.seed(16)

dados<-data.frame(col1= round(rnorm(100),2),
               col2= round(rnorm(100),2),
               col3= round(rnorm(100),2))

#digamos que quero calcular a média de cada coluna. Vamos supor que o R não tem uma função
# para calcular a média. Como faríamos isso?

#Vamos calcular a média para a primeira coluna de bd
sum(dados$col1)/length(dados$col1)

#Ok, temos a média da primeira coluna. Como calcularemos as demais médias?

#criando a função minha_media

minha_media<-function(coluna){
  sum(coluna)/length(coluna)
}

#testando
minha_media(dados$col1)

#ok, mas ainda assim continuo calculando a media de cada uma das colunas. Como faço para
#calcular de todas as colunas de uma vez?

output<-vector("double", ncol(dados))
for (i in seq_along(bd)){
  output[[i]]<-minha_media(bd[[i]])
}

output

lapply(dados, minha_media)

#se você salvar essa operação em um objeto a classe dele será uma lista
resultado<-lapply(dados, minha_media)

#vamos verificar?
class(resultado)

#como eu acesso o resultado da média da segunda coluna?
resultado[[2]]

#teremos o mesmo resultado utilizando a função map do purrr
library(purrr)
resultados2<-map(dados, minha_media)

#qual é o resultado da média da primeira coluna desse objeto que criamos com o map?
resultados2[[1]]

#agora vou aplicar o map à minha lista para calcular as médias das colunas dos dois dataframes
# VERIFICAR ISSO AQUI

#Vou ter que fazer outra função

nova_media<- function(lista){
  temp<-sum(lista[[i]])/length(lista[[i]])
}

map(minha_lista, nova_media)


###################################################################
library(pacman)
p_load(electionsBR, janitor, tidyverse)

#vetor salvando os anos que quero fazer download
anos<-c(1998,2002,2006,2010)

#fazendo download com lapply
rj_list<-lapply(anos, function(anos) party_mun_zone_fed(anos, uf="RJ"))

#fazendo download com map
rj_list<-map(anos, ~party_mun_zone_fed(.x,"RJ"))

#como acessar os bancos? Dar exemplo antes de nomeá-los

#dando nome para os bancos
names(rj_list)<-c('el98', 'el02', 'el06', 'el10')
names(rj_list)

#Vou precisar colocar a descrição do cargo em minúsculo pois mais adiante esses nomes
#serão incluídos nos títulos dos gráficos

#Como eu faria isso com loop for?
for(i in seq_along(rj_list)){
  rj_list[[i]]$DESCRICAO_CARGO<-tolower(rj_list[[i]]$DESCRICAO_CARGO)
}

#Como faria isso com map e dplyr? Primeiro vamos pensar como faríamos para apenas um banco.
# Vamos fazer para o banco de 1998
#Salvando apenas o banco de 1998 em um dataframe
el98<-rj_list$el98

#Para colocar todas os cargos em letras minúsculas neste banco eu faria o seguinte:
el98<-el98%>%
  mutate(DESCRICAO_CARGO=tolower(DESCRICAO_CARGO))

#Para ser possível alterar todos os bancos de apenas uma vez é necessário primeiro criar
#uma função com a alteração que você quer realizar e depois aplicar esta função à toda a lista
#utilizando a função map do purr. Como resultado teremos a alteração da variável DESCRICAO_CARGO
#para todos os bancos da lista.

#Vamos fazer então a função que chamarei de "minuscula":

minuscula<-function(lista_rj){
   temp<-lista_rj%>%
    mutate(DESCRICAO_CARGO=tolower(DESCRICAO_CARGO))
}

#Mais adiante terei que fazer a mesma operação com o banco com dados de comparecimento.
#Como eu já sei que a variável DESCRICAO_CARGO é a mesma nos dois bancos decidi
# inclui-la na função que receberá apenas um argumento que será a lista em que estão
#os bancos que queremos alterar. Desta forma, esta mesma função poderá ser utilizada tanto nesta
# lista de banco de votações quanto na que trabalharemos a seguir de total de comparecimento.

#Com a função pronta podemos aplicá-la à lista com a função map do purrr.

rj_list<-map(rj_list, minuscula)

#Vamos verificar? Vamos dar um View no banco de 1998

View(rj_list$el98)

#verificando se as dimensões dos bancos são as mesmas
map(rj_list, dim)

#guardando o nome de todas as colunas
nome_col<-map(rj_list, colnames)

#verificando as colunas são iguais em todos os bancos
nome_col$el98==nome_col$el02
nome_col$el02==nome_col$el06
nome_col$el06==nome_col$el10

#verificando os cargos existentes em cada banco na lista.
#Vou usar a função tabyl do janitor. Como ela é uma função que
#recebe dois argumentos, a lista e a variável que quero verificar
#devemos abri-la como função anônima, colocando um til antes da função
#e indicando, dentro da função tabyl, a lista que será utilizada como .x. A função
# map entenderá que .x é a lista rj_list já incluída como argumento da função map.
#Então o código terá essa cara.

map(rj_list, ~tabyl(.x, DESCRICAO_CARGO))


#Ok, podemos começar a estruturar o banco. Quero trabalhar apenas com eleições majoritárias
#(governador e senador), no primeiro turno (no caso de governador).
#Quero ver a evolução da porcentagem de votos recebidos por todos os partidos no Rio de Janeiro
# para estes dois cargos e comparar os partidos em cada eleição.
#O que preciso fazer:
# 1) Selecionar as variáveis de interesse;
#ANO_ELEICAO, NUM_TURNO, DESCRICAO_CARGO, SIGLA_PARTIDO, QTDE_VOTOS_NOMINAIS
# 2) Filtrar apenas primeiro turno e os cargos de interesse (GOVERNADOR, SENADOR)
# 3) Agrupar as linhas e somar o total de votos por partido

# Como seria se eu fizesse isso tudo para apenas um banco?

glimpse(el98)

maj98<-el98%>%
  select(ANO_ELEICAO, NUM_TURNO, DESCRICAO_CARGO, SIGLA_PARTIDO, QTDE_VOTOS_NOMINAIS)%>%
  filter(NUM_TURNO==1 & DESCRICAO_CARGO %in% c("governador", "senador"))%>%
  group_by(DESCRICAO_CARGO, SIGLA_PARTIDO)%>%
  summarise(votos=sum(QTDE_VOTOS_NOMINAIS))

#Como fazer isso para todos os bancos dentro da lista? Criar uma função e aplicá-la à
#lista utilizando map.

#Criando a função:

limpa_voto<-function(lista_votos){
  obj_temp<-lista_votos%>%
    select(NUM_TURNO, DESCRICAO_CARGO, SIGLA_PARTIDO, QTDE_VOTOS_NOMINAIS)%>%
    filter(NUM_TURNO==1 & DESCRICAO_CARGO %in% c("governador", "senador"))%>%
    group_by(DESCRICAO_CARGO, SIGLA_PARTIDO)%>%
    summarise(votos=sum(QTDE_VOTOS_NOMINAIS))
}

#Finalmente aplico a função à minha lista. Para manter os dados brutos salvarei
#o resultado em uma outra lista que chamarei de maj_rj

maj_rj<-map(rj_list, limpa_voto)

#Para calcular a porcentagem preciso baixar os dados de comparecimento. Eles estão
#no banco detalhes de votação do TSE e pode ser baixado pelo pacote electionsBR com
# a função details_mun_zone_fed. Vou utilizar a função map para isso.

comp_list<-map(anos, ~details_mun_zone_fed(.x,"RJ"))

#Nomeando os bancos dentro da lista. Vou dar o mesmo nome que dei aos bancos na lista de votos
names(comp_list)<-c('el98', 'el02', 'el06', 'el10')
names(comp_list)

#Verificando se as dimensões dos bancos são iguais
map(comp_list, dim)

#Verificando se as colunas são iguais. Vou fazer o mesmo processo que fiz no banco de
#votação

#Salvando o nome de todas as colunas em uma lista com a função map
nome_col_comp<-map(comp_rj, colnames)

#Verificando se os nomes das colunas são iguais em todos os bancos
nome_col_comp$el98==nome_col_comp$el02
nome_col_comp$el02==nome_col_comp$el06
nome_col_comp$el06==nome_col_comp$el10

#Agora preciso colocar os cargos em letra minúscula como fiz com a lista de votação.
# Aqui não tem segredo; vamos utilizar a mesma função "minuscula" já criada e aplicá-la a lista
#comp_list com map.

comp_list<-map(comp_list, minuscula)

#Hora de limpar o banco. O que precisamos fazer?
#1)Temos que selecionar as variáveis que precisamos:
#NUM_TURNO, DESCRICAO_CARGO, QTD_COMPARECIMENTO;
# 2) Filtrar apenas primeiro turno e os cargos de interesse (governador, senador);
# 3) Agrupar as linhas e somar o total de votos por cargo. Como estamos trabalhando com primeiro
#turno os valores serão iguais para os dois cargos

#Farei como fiz para tratar os bancos de votação: primeiro farei uma função para fazer a
#limpeza e depois aplicarei a função à lista com map

limpa_comp<-function(lista_comp){
  temp<-lista_comp %>%
  select(NUM_TURNO, DESCRICAO_CARGO, QTD_COMPARECIMENTO)%>%
  filter(NUM_TURNO==1 & DESCRICAO_CARGO %in% c("governador", "senador"))%>%
  group_by(DESCRICAO_CARGO)%>%
  summarise(comparecimento=sum(QTD_COMPARECIMENTO))
}

#salvarei os resultados em uma lista separada
comp_rj<-map(comp_list, limpa_comp)

#Agora preciso juntar os totais de comparecimento com cada banco/ano
# Descrever aqui o que esta função está fazendo!!!
calcula_comp<-function(lista_maj, lista_comp){
  lista_maj%>%
  left_join(lista_comp)%>%
    mutate(porc_voto=(round(votos*100/comparecimento, 2)))
}

#Juntando os bancos aplicando o map. Como temos multiplos argumentos como
#input da função usaremos a função map2, que permite inserir dois argumentos

maj_rj<-map2(.x = maj_rj, .y = comp_rj, ~ calcula_comp(.x,.y))

#Pronto! Os bancos estão finalmente prontos! Hora de plotar os gráficos
#Primeiro queremos fazer um gráfico de barras comparando a votação dos seis partidos
#mais bem votados em cada eleição para cada cargo. Assim, queremos plotar ao total oito gráficos:
# quatro para senador (1998, 2002, 2006, 2010) e quatro para governador (idem).

.x<-c("governador")
.y<-anos[1]

el98%>%
  filter(DESCRICAO_CARGO==.x)%>%
  top_n(5)%>%
  arrange(desc(porc_voto))%>%
  mutate(SIGLA_PARTIDO=factor(SIGLA_PARTIDO, levels = unique(SIGLA_PARTIDO)))%>%
  ggplot()+
  geom_bar(aes(x=SIGLA_PARTIDO, y=porc_voto), stat = "identity", width = 0.7, fill="turquoise4")+
  labs(title=glue::glue("Votação para ", .x," no estado do Rio de Janeiro em ", .y),
       x= " ",
       y= "(%)",
       caption = "Fonte: TSE") +
  theme(plot.title = element_text(size=12))+
  theme_bw()


#fazendo a função
 gf_barras<-function(banco, ano, cargo){
   temp<-banco%>%
     filter(DESCRICAO_CARGO== cargo)%>%
     top_n(5)%>%
     arrange(desc(porc_voto))%>%
     mutate(SIGLA_PARTIDO=factor(SIGLA_PARTIDO, levels = unique(SIGLA_PARTIDO)))%>%
     ggplot()+
     geom_bar(aes(x=SIGLA_PARTIDO, y=porc_voto), stat = "identity", width = 0.7, fill="turquoise4")+
     labs(title=glue::glue("Votação para ", cargo," no estado do Rio de Janeiro em ", ano),
          x= " ",
          y= "(%)",
          caption = "Fonte: TSE") +
     theme(plot.title = element_text(size=12))+
     theme_bw()
 }

 #plotando governador
map2(.x= maj_rj,
     .y= anos,
     .f= ~gf_barras(.x, .y, "governador"))

#plotando senador
map2(.x= maj_rj,
     .y= anos,
     .f= ~gf_barras(.x, .y, "senador"))

#Mas e se eu quiser mudar as cores dos gráficos para cada cargo? Se eu quiser esse verde para
#governador e vermelho para senador? Uma alternativa seria elaborar a função de forma que a
#cor do gráfico seria um argumento. Adicionaríamos além de banco, ano e cargo um argumento cor
# lembrando que aí deve ser inserido uma entrada de cor que o ggplot aceita 

gf_barras_cor<-function(banco, ano, cargo, cores){
  temp<-banco%>%
    filter(DESCRICAO_CARGO== cargo)%>%
    top_n(5)%>%
    arrange(desc(porc_voto))%>%
    mutate(SIGLA_PARTIDO=factor(SIGLA_PARTIDO, levels = unique(SIGLA_PARTIDO)))%>%
    ggplot()+
    geom_bar(aes(x=SIGLA_PARTIDO, y=porc_voto), stat = "identity", width = 0.7, fill=cores)+
    labs(title=glue::glue("Votação para ", cargo," no estado do Rio de Janeiro em ", ano),
         x= " ",
         y= "(%)",
         caption = "Fonte: TSE") +
    theme(plot.title = element_text(size=12))+
    theme_bw()
}

#Agora vou usar essa função para alterar as cores dos gráficos de senador para vermelho.

plot_sen<-map2(.x=maj_rj,
               .y=anos,
               .f= ~gf_barras_cor(.x, .y, "senador", "tomato4"))

#salvando os gráficos em pdfs separados por cargo

pdf("plot_sen.pdf")
plot_gov
dev.off()



