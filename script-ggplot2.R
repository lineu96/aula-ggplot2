
# ==============================================================================
# Visualização de Dados e o ggplot2
# A Gramática por Trás dos Gráficos
# ==============================================================================
# Prof. Me Lineu Alberto Cavazani de Freitas
# ==============================================================================
# Os comandos a seguir são uma introdução à gramática dos gráficos implementada
# no pacote ggplot2
# ==============================================================================
rm(list = ls())
# ==============================================================================

# Carregar os pacotes essenciais
library(ggplot2) # implementação da gramática dos gráficos
library(dplyr) # para manipulação de dados
library(plotly) # Para interatividade

# Carregar o dataset 'mpg' disponível no pacote ggplot2
data(mpg)
help(mpg)

# Função ggplot()
?ggplot()

# ==============================================================================
# 1. ANÁLISE UNIVARIADA (Foco: Geometrias e Eixos X/Y)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1. Histograma (Quantitativa)
# ------------------------------------------------------------------------------
# Versão Básica (GoG Mínima: Data, X, Geom). Tema cinza padrão.
histograma <- ggplot(mpg, aes(x = hwy)) + 
  geom_histogram()
histograma

# Versão Customizada: Adição de 'fill' (cor), 'color' (contorno), 'binwidth' e 'theme'
histograma_final <- ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 2, fill = "darkred", color = "white") + 
  theme_minimal() + 
  labs(title = "Distribuição de Consumo (Highway)",
       x = "Milhas/Galão (hwy)", y = "Contagem de Veículos")
histograma_final

# ------------------------------------------------------------------------------
# 1.2. Gráfico de Densidade (Quantitativa)
# ------------------------------------------------------------------------------
# Versão Básica (GoG Mínima)
densidade <- ggplot(mpg, aes(x = hwy)) +
  geom_density()
densidade

# Versão Customizada: Adição de 'fill', 'alpha' (transparência) e 'theme'
densidade_final <- ggplot(mpg, aes(x = hwy)) +
  geom_density(fill = "steelblue", alpha = 0.6) + 
  theme_bw() +
  labs(title = "Gráfico de Densidade de Hwy", y = "Densidade")
densidade_final

# ------------------------------------------------------------------------------
# 1.3. Boxplot (Quantitativa)
# ------------------------------------------------------------------------------
# Versão Básica (GoG Mínima)
boxplot <- ggplot(mpg, aes(x = "", y = hwy)) +
  geom_boxplot()
boxplot

# Versão Customizada: Customização de cor do box e 'outlier.shape'
boxplot_final <- ggplot(mpg, aes(x = "", y = hwy)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen",
               outlier.color = "red", outlier.shape = 4) + 
  theme_classic() +
  labs(title = "Boxplot: Resumo da Distribuição de Hwy", x = NULL)
boxplot_final

# ------------------------------------------------------------------------------
# 1.4. Barras Verticais e Horizontais (Qualitativa)
# ------------------------------------------------------------------------------
# Versão Básica Vertical (GoG Mínima: Apenas X). Usa 'stat="count"' implícito.
barras_verticais <- ggplot(mpg, aes(x = class)) +
  geom_bar()
barras_verticais

# Versão Básica Horizontal: Adição de 'coord_flip()' à versão básica
barras_horizontais <- p_bar_vert_basico + coord_flip()
barras_horizontais

# Versão Customizada: Mapeamento de 'fill' (estética) e 'theme'
barras_verticais_final <- ggplot(mpg, aes(x = class)) + 
  geom_bar() +
  theme_minimal() +
  labs(title = "Contagem de Veículos por Classe") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) 
barras_verticais_final

# ------------------------------------------------------------------------------
# 1.5. Gráfico de Setores (Coordenadas)
# ------------------------------------------------------------------------------
# Versão Básica (Usa 'coord_polar()' para converter barras em fatias)
setores <- ggplot(mpg, aes(x = "", fill = class)) + 
  geom_bar(width = 1) +
  coord_polar("y", start = 0)
setores

# Versão Customizada: alterando a paleta de cores e o tema
setores_final <- ggplot(mpg, aes(x = "", fill = class)) + 
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set1") + 
  theme_void() + 
  labs(title = "Distribuição de Classes de Veículos",
       fill = "Classe")
setores_final

# ==============================================================================
# 2. ANÁLISE BIVARIADA (Foco: Estéticas Adicionais e Posição)
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1. Diagrama de Dispersão (Quantitativa x Quantitativa)
# ------------------------------------------------------------------------------
# Variáveis: 'displ' e 'hwy'

# Versão Básica: Apenas pontos (GoG Mínima)
dispersão <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()
dispersão

# Versão Customizada: Mapeamento de 'color', 'shape', 'size', 'alpha' e 'geom_smooth'
dispersao_final <- ggplot(mpg, aes(x = displ, y = hwy, color = factor(cyl))) + # Cor por 'cyl'
  geom_point(aes(shape = drv, size = cty), alpha = 0.7) + # 'shape', 'size', 'alpha'
  geom_smooth(method = "lm", se = FALSE, color = "black") + # Linha de tendência
  theme_classic() +
  labs(title = "Relação entre Cilindrada e Consumo",
       color = "Cilindros", shape = "Tração", size = "Consumo Urbano")
dispersao_final

# ------------------------------------------------------------------------------
# 2.2. Boxplot e Violino por Grupos (Quantitativa x Qualitativa)
# ------------------------------------------------------------------------------
# Boxplot por Níveis (Básico)
boxplot_niveis <- ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot()
boxplot_niveis

# Versão Customizada (Violino + Jitter): 'fill' estético e 'alpha'
violino <- ggplot(mpg, aes(x = class, y = hwy, fill = class)) +
  geom_violin(alpha = 0.7) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.3) + # Adiciona os pontos
  theme_light() +
  labs(title = "Distribuição de Consumo por Classe de Veículo", x = "Classe do Veículo")
violino

# ------------------------------------------------------------------------------
# 2.3. Barras Lado a Lado (Qualitativa x Qualitativa)
# ------------------------------------------------------------------------------
# Versão Básica: 'fill' e 'position="dodge"'
barras_lado_a_lado <- ggplot(mpg, aes(x = class, fill = drv)) +
  geom_bar(position = "dodge")
barras_lado_a_lado

# ------------------------------------------------------------------------------
# 2.4. Barras Empilhadas (Contagem e Proporção)
# ------------------------------------------------------------------------------
# Versão Básica (Contagem Empilhada: position = "stack" - Padrão)
barras_empilhadas <- ggplot(mpg, aes(x = class, fill = drv)) +
  geom_bar() 
barras_empilhadas

# Versão Customizada (Proporção Empilhada): 'position = "fill"'
barras_empilhadas2 <- ggplot(mpg, aes(x = class, fill = drv)) +
  geom_bar(position = "fill") + 
  theme_classic() +
  labs(title = "Proporção de Tipos de Tração por Classe",
       y = "Proporção")
barras_empilhadas2

# ==============================================================================
# 3. COMPLEMENTOS AVANÇADOS (Linhas, Group, Linetype, Facet_Grid)
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1. geom_line, geom_path e Estética 'group'/'linetype'
# ------------------------------------------------------------------------------
# Simular dados de série temporal para melhor demonstração
serie_temporal <- data.frame(
  Ano = rep(2010:2015, each = 3),
  Trimestre = rep(1:3, 6),
  Regiao = rep(c("Norte", "Sul", "Leste"), times = 6),
  Valor = c(rnorm(9, 10, 2), rnorm(9, 15, 3))
)

# Versão Básica: geom_line (Mapeamento X e Y)
linhas <- ggplot(df_ts, aes(x = Ano, y = Valor)) +
  geom_line()
linhas 
# A linha está conectando todos os pontos!

# Versão Customizada: Uso explícito de 'group' e 'linetype'
linhas_final <- ggplot(serie_temporal, aes(x = Ano, y = Valor, color = Regiao, group = Regiao)) + 
  geom_line(aes(linetype = Regiao), linewidth = 1) + # 'linetype' explora o estilo da linha
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Série Temporal: Variação de Valor por Região")
linhas_final

# ------------------------------------------------------------------------------
# 3.2. Facet_Grid (Divisão por Duas Variáveis)
# ------------------------------------------------------------------------------
# Facet_Grid: Linha ~ Coluna
# Exemplo: mpg por Tração (drv) e Cilindros (cyl)

painel <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl) + # Linha ~ Coluna
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  theme_bw() +
  labs(title = "Relação Displ x Hwy, Facetada por Tração e Cilindros")
painel

# ==============================================================================
# 4. INTERATIVIDADE (ggplotly)
# ==============================================================================

# Adiciona um mapeamento 'text' para ter rótulos personalizados no hover
interativo <- ggplot(mpg, aes(x = displ, y = hwy, color = class,
                                     text = paste("Modelo:", model,
                                                  "\nClasse:", class,
                                                  "\nConsumo:", hwy))) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Dispersão Interativa de Consumo")

# Converte o objeto ggplot em um objeto plotly
interativo <- ggplotly(interativo, tooltip = "text")
interativo

# ==============================================================================
