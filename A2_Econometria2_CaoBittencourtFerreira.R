# 1. INTRODUÇÃO
# 1.1. PACOTES
# install.packages('tidyverse')
# install.packages('plyr')
# install.packages('stargazer')
# install.packages('gghighlight')
# install.packages('ggridges')
# install.packages('viridis')
# install.packages('naniar')
# install.packages('scales')
# install.packages('vars')
# install.packages('patchwork')

library(tidyverse)
library(plyr)
library(stargazer)
library(gghighlight)
library(ggridges)
library(viridis)
library(naniar)
library(scales)
library(vars)
library(patchwork)
select <- dplyr::select
summarise <- dplyr::summarise

# 1.2. TEMAS
theme_set(theme_bw(base_size = 16) +
            theme(plot.title = element_text(size = rel(1),
                                            face = 'bold',
                                            hjust = 0.5,
                                            vjust = 3),
                  plot.subtitle = element_text(size = rel(1),
                                               face = 'italic',
                                               hjust = 0.5,
                                               vjust = 3),
                  plot.margin = margin(20,20,20,20),
                  panel.grid = element_line(size = rel(0.2)),
                  axis.title.x = element_text(size = rel(0.85),
                                              hjust = 0.5,
                                              vjust = -3),
                  axis.title.y = element_text(size = rel(0.85),
                                              hjust = 0.5,
                                              vjust = 3),
                  axis.text.x = element_text(size = rel(0.8)),
                  axis.text.y = element_text(size = rel(0.8)),
                  strip.text.x = element_text(size = rel(0.8)),
                  strip.text.y = element_text(size = rel(0.8)),
                  legend.title = element_text(size = rel(0.8)),
                  legend.text = element_text(size = rel(0.7)),
                  legend.background = element_blank(),
                  legend.position = 'right',
                  legend.direction = 'vertical',
                  legend.box.background = element_blank()))

theme_ridges(font_size = 16) +
  theme(plot.title = element_text(size = rel(1),
                                  face = 'bold',
                                  hjust = 0.5,
                                  vjust = 3),
        plot.margin = margin(20,20,20,20),
        panel.grid = element_line(size = rel(0.2)),
        axis.title.x = element_text(size = rel(0.8),
                                    hjust = 0.5,
                                    vjust = -3),
        axis.title.y = element_text(size = rel(0.8),
                                    hjust = 0.5,
                                    vjust = 3),
        axis.text.x = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.8)),
        strip.text.x = element_text(size = rel(0.8)),
        strip.text.y = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.7)),
        legend.background = element_blank(),
        legend.position = 'right',
        legend.direction = 'vertical',
        legend.box.background = element_blank()) -> theme_ridges.2


# 2. DADOS
# 2.1. NOMES DAS VARIÁVEIS
# 2.1.1. GROSS DOMESTIC PRODUCT PER CAPITA (USD) = GDP => LOG(GDP) = LGDP
# 2.1.2. LGDS (%GDP) = GDS => LOG(GDS*GDP) = LGDS

# 2.2. LINKS (FONTE = BANCO MUNDIAL)
link.gdp <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQCK9f22bpEOuLU5PO8169ChRlpgXhhZPXk4AfW49RmGfmfWbgXprld05JBrXnkBw/pub?gid=1981923119&single=true&output=csv'
link.gds <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQOkxWXJjjmTUwKuQpc6bUp7h-w8rbW8SgUUhxZi1HJkv8N2-m6tREAH7D6KLBBCw/pub?gid=769240644&single=true&output=csv'


# 2.3. DATAFRAMES
df.gdp <- read.csv(url(link.gdp),
                   na.strings = c('..','...','NA','N/A',''))

df.gds <- read.csv(url(link.gds),
                   na.strings = c('..','...','NA','N/A',''))


# 3. LIMPEZA DOS DADOS
# 3.1. EXPLORAÇÃO PRELIMINAR
# glimpse(df.gdp)
# str(df.gdp)

# glimpse(df.gds)
# str(df.gds)

# 3.2. FORMATO WIDE -> LONG 
df.gdp %>% 
  gather(.,
         Year,
         GDP,
         names(df.gdp[5:ncol(.)])) %>% 
  select(-Series.Name,
         -Series.Code) -> df.gdp

df.gds %>% 
  gather(.,
         Year,
         GDS,
         names(df.gds[5:ncol(.)])) %>% 
  select(-Series.Name,
         -Series.Code) -> df.gds

# 3.3. UNIFICAÇÃO DAS BASES DE DADOS
plyr::join_all(list(df.gdp,
                    df.gds),
               type = 'right',
               by = c('Country.Code' = 'Country.Code',
                      'Year' = 'Year')) %>% 
  select(-5) -> df.joined
   
# 3.4. PEQUENAS CORREÇÕES
# 3.4.1. VALORES NUMÉRICOS E POUPANÇA COMO PORCENTAGEM
df.joined %>%
  mutate_at(vars(4:ncol(.)),
            funs(as.character)) %>% 
  mutate_at(vars(4:ncol(.)),
            funs(str_replace(.,',','.'))) %>% 
  mutate_at(vars(4:ncol(.)),
            funs(as.numeric)) %>%  
  mutate(GDS = GDS/100) -> df.joined

# 3.4.2. NOMES CORRETOS DOS ANOS E REMOÇÃO DO ANO 2020
df.joined %>%
  mutate(Year = str_remove(Year,
                           pattern = 'X'),
         Year = factor(Year)) %>% 
  filter(Year != '2020') %>%
  mutate(Year = factor(Year)) %>% 
  relocate(Year,
           .before = Country.Code) -> df.joined

# 3.4.3. FATORES
df.joined %>%
  mutate(Country.Code = factor(Country.Code),
         Country.Name = factor(Country.Name)) -> df.joined

# 4. MANIPULAÇÃO DE DADOS
# 4.1. MACRO-REGIÕES
list(Americas = c('ARG','BHS','BRB','BLZ','BOL','BRA','CAN',
                  'CHL','COL','CRI','CUB','DMA','DOM','ECU',
                  'SLV','GTM','GUY','HTI','HND','JAM','MEX',
                  'NIC','PAN','PRY','PER','LCA','VCT','SUR',
                  'TTO','USA','URY','VEN'),
     `Asia-Pacific` = c('AFG','AUS','BGD','BTN','BRN','KHM','CHN',
                        'FJI','HKG','IND','IDN','JPN','KAZ','KIR',
                        'PRK','KOR','KGZ','LAO','MAC','MYS','MDV',
                        'FSM','MNG','MMR','NPL','NZL','PAK','PNG',
                        'PHL','WSM','SGP','SLB','LKA','TJK','THA',
                        'TLS','TON','TKM','UZB','VUT','VNM'),
     Europe = c('ALB','ARM','AUT','AZE','BLR','BEL','BIH',
                'BGR','HRV','CYP','CZE','DNK','EST','FIN',
                'FRA','GEO','DEU','GRC','HUN','ISL','IRL',
                'ITA','XKX','LVA','LIE','LTU','LUX','MLT',
                'MDA','MNE','NLD','MKD','NOR','POL','PRT',
                'ROU','RUS','SRB','SVK','SVN','ESP','SWE',
                'CHE','TUR','UKR','GBR'),
     `Middle East and North Africa` = c('DZA','BHR','EGY','IRN','IRQ','ISR','JOR',
                                        'KWT','LBN','LBY','MAR','OMN','QAT','SAU',
                                        'SYR','TUN','ARE','YEM'),
     `Sub-Saharan Africa` = c('AGO','BEN','BWA','BFA','BDI','CPV','CMR',
                              'CAF','TCD','COM','COD','COG','CIV','DJI',
                              'GNQ','ERI','SWZ','ETH','GAB','GMB','GHA',
                              'GIN','GNB','KEN','LSO','LBR','MDG','MWI',
                              'MLI','MRT','MUS','MOZ','NAM','NER','NGA',
                              'RWA','STP','SEN','SYC','SLE','ZAF','SSD',
                              'TZA','TGO','UGA','ZMB','ZWE')) -> macro.regions

df.joined %>% 
  mutate(Region = case_when(Country.Code %in% macro.regions$Americas ~ 'Americas',
                            Country.Code %in% macro.regions$`Asia-Pacific` ~ 'Asia-Pacific',
                            Country.Code %in% macro.regions$Europe ~ 'Europe',
                            Country.Code %in% macro.regions$`Middle East and North Africa` ~ 'Middle East and North Africa',
                            Country.Code %in% macro.regions$`Sub-Saharan Africa` ~ 'Sub-Saharan Africa')) %>%
  relocate(Region,
           .after = Country.Name) %>%
  relocate(GDS,
           .after = Region) %>%
  filter(!is.na(Region)) %>%
  mutate(Country.Code = factor(Country.Code),
         Country.Name = factor(Country.Name),
         Region = factor(Region)) -> df.joined

# 4.2. BACKUP
df.joined -> df.joined.back
# df.joined.back -> df.joined


# 5. AJUSTES FINAIS
# 5.1. INVESTIGAÇÃO PRELIMINAR
vis_miss(df.joined)

gg_miss_var(df.joined,
            show_pct = T) + 
  scale_y_continuous(limits = c(0,100))

gg_miss_var(df.joined,
            facet = Year,
            show_pct = T) + 
  scale_y_continuous(limits = c(0,100))

gg_miss_var(df.joined,
            facet = Region,
            show_pct = T) + 
  scale_y_continuous(limits = c(0,100))

gg_miss_fct(df.joined,
            fct = Region)

gg_miss_fct(df.joined,
            fct = Year)

gg_miss_fct(df.joined,
            fct = Country.Code)

# 5.1.1. GRÃFICOS REGIONAIS PARA SELEÇÃO DE PAÍSES (NA)
# for(region in levels(df.joined$Region)){
#   
#   pretty.red <- '#E22712'
#   pretty.blue <- viridis::plasma(1)
#   
#   df.joined %>%
#     filter(Region == region) %>%
#     group_by(Country.Code) %>%
#     summarise(GDP = sum(is.na(GDP))/length(unique(Year)),
#               GDS = sum(is.na(GDS))/length(unique(Year)),
#               NNA = sum(GDP,
#                         GDS)) %>%
#     mutate(Country.Code = fct_reorder(Country.Code,
#                                       NNA)) %>%
#     pivot_longer(.,
#                  cols = c(GDP,
#                           GDS),
#                  names_to = 'variable') %>%
#     ggplot(.,aes(x = variable,
#                  y = Country.Code,
#                  fill = value >= 0.3)) +
#     geom_tile(color = 'white',
#               size = 0.8) +
#     geom_text(aes(label = paste0(100*round(value,4),
#                                  '%')),
#                   color = 'white',
#                   fontface = 'bold',
#                   size = 3) +
#     gghighlight(value >= 0.3,
#                 unhighlighted_params = list(color = 'white',
#                                             label = NA)) +
#     scale_fill_manual(values = pretty.red) +
#     guides(fill = F) +
#     labs(title = paste0('Missing Values (',
#                         region,
#                         ')'),
#          subtitle = 'Missingness Threshold = 30%',
#          x = 'Variable',
#          y = 'Country Code\n') +
#     theme(axis.text.y = element_text(size = rel(0.6),
#                                      face = 'bold')) -> missing.values.30
# 
#   ggsave(missing.values.30,
#          file = paste0('Missing Values (',
#                        region,
#                        ', 30).pdf'),
#          height = 11.69,
#          width = 8.27,
#          units = 'in')
#   ggsave(missing.values.30,
#          file = paste0('Missing Values (',
#                        region,
#                        ', 30).png'),
#          height = 11.69,
#          width = 8.27,
#          units = 'in')
# 
#   df.joined %>%
#     filter(Region == region) %>%
#     group_by(Country.Code) %>%
#     summarise(GDP = sum(is.na(GDP))/length(unique(Year)),
#               GDS = sum(is.na(GDS))/length(unique(Year)),
#               NNA = sum(GDP,
#                         GDS)) %>%
#     mutate(Country.Code = fct_reorder(Country.Code,
#                                       NNA)) %>%
#     pivot_longer(.,
#                  cols = c(GDP,
#                           GDS),
#                  names_to = 'variable') %>%
#     ggplot(.,aes(x = variable,
#                  y = Country.Code,
#                  fill = value)) +
#     geom_tile(color = 'white',
#               size = 0.8) +
#     geom_text(aes(label = paste0(100*round(value,4),
#                                  '%')),
#               color = 'white',
#               fontface = 'bold',
#               size = 3) +
#     gghighlight(value < 0.3,
#                 unhighlighted_params = list(color = 'white',
#                                             label = NA)) +
#     scale_fill_viridis(option = 'plasma') +
#     guides(fill = F) +
#     labs(title = paste0('Valid Data (',
#                         region,
#                         ')'),
#          subtitle = 'Missingness Threshold = 30%',
#          x = 'Variable',
#          y = 'Country Code\n') +
#     theme(axis.text.y = element_text(size = rel(0.6),
#                                      face = 'bold')) -> valid.data
# 
#   ggsave(valid.data,
#          file = paste0('Valid Data (',
#                        region,
#                        ').pdf'),
#          height = 11.69,
#          width = 8.27,
#          units = 'in')
#   ggsave(valid.data,
#          file = paste0('Valid Data (',
#                        region,
#                        ').png'),
#          height = 11.69,
#          width = 8.27,
#          units = 'in')
# }

# 5.2. MEDIDAS
# 5.2.1. A FIM DE NÃO ENVIESAR A ANÁLISE TENDO EM VISTA A AMOSTRA PEQUENA, OPTA-SE POR REMOVER QUALQUER PAÍS COM VARIÁVEIS MACROECONÔMICAS MINIMAMENTE INDISPONÍVEIS (NA>30%)
df.joined %>% 
  group_by(Country.Code) %>%
  filter(sum(is.na(GDP))/length(unique(Year)) < 0.3,
         sum(is.na(GDS))/length(unique(Year)) < 0.3) %>%
  ungroup(.) %>%
  mutate(Country.Code = factor(Country.Code),
         Country.Name = factor(Country.Name)) -> df.joined

# 5.2.2 POUPANÇA NÃO-POSITIVA => PARA PERMITIR A LOG-TRANSFORMAÇÃO, REMOVE-SE OS PAÍSES EM QUE AO MENOS 30%GDS <= 0 E SUBSTITUI-SE A POUPANÇA REGIONAL ANUAL MÍNIMA NOS DEMAIS 
df.joined %>%
  group_by(Country.Code) %>%
  filter(sum(GDS <= 0,
  na.rm = T)/length(unique(Year)) < 0.3) %>%
  ungroup(.) %>%
  mutate(GDS = replace(GDS, 
                       GDS <= 0, 
                       'Negative.GDS'),
         Country.Code = factor(Country.Code),
         Country.Name = factor(Country.Name)) %>% 
  group_by(Year,
           Region) %>%
  mutate_at(vars(GDS),
            funs(replace(.,
                         . == 'Negative.GDS',
                         min(.,
                             na.rm = T)))) %>% 
  mutate(GDS = as.numeric(GDS)) -> df.joined 

# 5.2.3. POUPANÇA TOTAL: GDS = 100% NA MAURITANIA (1961 - 1965), I.E. CLARAMENTE UM ERRO CONTÃBIL => SUBSTITUIÇÃO PELA MEDIANA REGIONAL ANUAL
df.joined %>%
  mutate(GDS = replace(GDS,
                       GDS == 1,
                       NA)) -> df.joined

# 5.2.4. DEMAIS PAÍSES => SUBSTITUIR A MEDIANA REGIONAL ANUAL
df.joined %>%
  group_by(Year,
           Region) %>%
  mutate_at(vars(GDP,
                 GDS),
            funs(replace(.,
                         is.na(.),
                         median(.,
                                na.rm = T)))) -> df.joined

# 5.4. LOG-TRANSFORMAÇÃO DE VARIÁVEIS
df.joined %>%
  mutate(LGDS = log(GDS*GDP),
         LGDP = log(GDP)) -> df.joined

# 5.5. ORDEM ALFABÉTICA DE ACORDO COM O NOME DOS PAÍSES (DIFERENTE DA ORDEM DE ACORDO COM O CÓDIGO DOS PAÍSES POR CONTA DOS CÕDIGOS ISO DOS PAÍSES)
df.joined %>% 
  arrange(Year,
          Country.Name) %>%
  mutate(Country.Code = factor(Country.Code,
                               unique(Country.Code))) -> df.joined


# 6. MODELO VAR
# 6.1. NÚMERO ÓTIMO DE LAGS
# lags.criteria.region <- function(df){
  df %>%
    dlply(.,'Country.Code',
          function(x){VARselect(x %>%
                                  ungroup(.) %>%
                                  select(LGDS,
                                         LGDP) %>%
                                  as.matrix(.),
                                lag.max = 15)}) %>%
    sapply(.,
           function(x){sapply(x$selection,
                              median)}) %>%
    t(.) %>%
    as.data.frame(.[,]) %>%
    mutate(Country.Code = levels(df$Country.Code),
           Country.Name = levels(df$Country.Name),
           Region = case_when(Country.Code %in% macro.regions$Americas ~ 'Americas',
                              Country.Code %in% macro.regions$`Asia-Pacific` ~ 'Asia-Pacific',
                              Country.Code %in% macro.regions$Europe ~ 'Europe',
                              Country.Code %in% macro.regions$`Middle East and North Africa` ~ 'Middle East and North Africa',
                              Country.Code %in% macro.regions$`Sub-Saharan Africa` ~ 'Sub-Saharan Africa')) %>%
    pivot_longer(., 
                 cols = c('AIC(n)',
                          'HQ(n)',
                          'SC(n)','FPE(n)'),
                 names_to = 'Criteria',
                 values_to = 'Value') %>% 
    group_by(Region,
             Criteria) %>%
    summarise(Max = max(Value) %>% floor(.),
              Min = min(Value) %>% floor(.),
              Mean = mean(Value) %>% floor(.),
              Median = median(Value) %>% floor(.)) %>% 
    return(.)
}
# lags.criteria <- function(df){
  df %>%
    dlply(.,'Country.Code',
          function(x){VARselect(x %>%
                                  ungroup(.) %>%
                                  select(LGDS,
                                         LGDP) %>%
                                  as.matrix(.),
                                lag.max = 15)}) %>%
    sapply(.,
           function(x){sapply(x$selection,
                              median)}) %>%
    t(.) %>%
    as.data.frame(.[,]) %>%
    mutate(Country.Code = levels(df$Country.Code),
           Country.Name = levels(df$Country.Name),
           Region = case_when(Country.Code %in% macro.regions$Americas ~ 'Americas',
                              Country.Code %in% macro.regions$`Asia-Pacific` ~ 'Asia-Pacific',
                              Country.Code %in% macro.regions$Europe ~ 'Europe',
                              Country.Code %in% macro.regions$`Middle East and North Africa` ~ 'Middle East and North Africa',
                              Country.Code %in% macro.regions$`Sub-Saharan Africa` ~ 'Sub-Saharan Africa')) %>%
    pivot_longer(., 
                 cols = c('AIC(n)',
                          'HQ(n)',
                          'SC(n)','FPE(n)'),
                 names_to = 'Criteria',
                 values_to = 'Value') %>% 
    group_by(Criteria) %>%
    summarise(Max = max(Value) %>% floor(.),
              Min = min(Value) %>% floor(.),
              Mean = mean(Value) %>% floor(.),
              Median = median(Value) %>% floor(.)) %>% 
    return(.)
}
# lags.region <- function(df){
  df %>%
    dlply(.,'Country.Code',
          function(x){VARselect(x %>%
                                  ungroup(.) %>%
                                  select(LGDS,
                                         LGDP) %>%
                                  as.matrix(.),
                                lag.max = 15)}) %>%
    sapply(.,
           function(x){sapply(x$selection,
                              median)}) %>%
    t(.) %>%
    as.data.frame(.[,]) %>%
    mutate(Country.Code = levels(df$Country.Code),
           Country.Name = levels(df$Country.Name),
           Region = case_when(Country.Code %in% macro.regions$Americas ~ 'Americas',
                              Country.Code %in% macro.regions$`Asia-Pacific` ~ 'Asia-Pacific',
                              Country.Code %in% macro.regions$Europe ~ 'Europe',
                              Country.Code %in% macro.regions$`Middle East and North Africa` ~ 'Middle East and North Africa',
                              Country.Code %in% macro.regions$`Sub-Saharan Africa` ~ 'Sub-Saharan Africa')) %>%
    pivot_longer(., 
                 cols = c('AIC(n)',
                          'HQ(n)',
                          'SC(n)','FPE(n)'),
                 names_to = 'Criteria',
                 values_to = 'Value') %>% 
    group_by(Region) %>%
    summarise(Max = max(Value) %>% floor(.),
              Min = min(Value) %>% floor(.),
              Mean = mean(Value) %>% floor(.),
              Median = median(Value) %>% floor(.)) %>% 
    return(.)
}
# lags.all <- function(df){
  df %>%
    dlply(.,'Country.Code',
          function(x){VARselect(x %>%
                                  ungroup(.) %>%
                                  select(LGDS,
                                         LGDP) %>%
                                  as.matrix(.),
                                lag.max = 15)}) %>%
    sapply(.,
           function(x){sapply(x$selection,
                              median)}) %>%
    t(.) %>%
    as.data.frame(.[,]) %>%
    mutate(Country.Code = levels(df$Country.Code),
           Country.Name = levels(df$Country.Name),
           Region = case_when(Country.Code %in% macro.regions$Americas ~ 'Americas',
                              Country.Code %in% macro.regions$`Asia-Pacific` ~ 'Asia-Pacific',
                              Country.Code %in% macro.regions$Europe ~ 'Europe',
                              Country.Code %in% macro.regions$`Middle East and North Africa` ~ 'Middle East and North Africa',
                              Country.Code %in% macro.regions$`Sub-Saharan Africa` ~ 'Sub-Saharan Africa')) %>%
    pivot_longer(., 
                 cols = c('AIC(n)',
                          'HQ(n)',
                          'SC(n)','FPE(n)'),
                 names_to = 'Criteria',
                 values_to = 'Value') %>% 
    summarise(Max = max(Value) %>% floor(.),
              Min = min(Value) %>% floor(.),
              Mean = mean(Value) %>% floor(.),
              Median = median(Value) %>% floor(.)) %>% 
    return(.)
}
# 
# lags.criteria.region(df.joined)
# lags.criteria(df.joined)
# lags.region(df.joined)
# lags.all(df.joined)


# ----------------------
# for (lags in 1:15){
#   df.joined %>%
#     dlply(.,'Country.Code',
#           function(x){VAR(x %>%
#                             ungroup(.) %>%
#                             select(LGDS,
#                                    LGDP) %>%
#                             as.matrix(.),
#                           p = lags,
#                           type = 'const')}) -> models.var
#   
#   # 6.3. ESTABILIDADE DOS MODELOS
#   tryapply(models.var,
#            roots) %>%
#     lapply(., abs) %>%
#     lapply(., function(x){all(x < 1)}) %>%
#     unlist(.) %>% which(.) %>% names(.) -> valid.var.countries
#   
#   # 6.4. MODELOS ESTÁVEIS
#   # 6.4.1. GERAIS
#   df.joined %>%
#     filter(Country.Code %in% valid.var.countries) %>% 
#     dlply(.,'Country.Code',
#           function(x){VAR(x %>%
#                             ungroup(.) %>%
#                             select(LGDS,
#                                    LGDP) %>%
#                             as.matrix(.),
#                           p = lags,
#                           type = 'const')}) -> models.var
#   
#   lapply(models.var,
#          causality) %>%
#     lapply(.,function(x){x$Granger$p.value}) %>% 
#     as.data.frame(.) %>% 
#     gather(.,
#            Country.Code,
#            `Granger Causality Test`) %>% 
#     mutate(H0 = 'H0: LGDS does not cause LGDP') -> Causality.Test.Granger_LGDS.LGDP
#   
#   # 6.5.2.2. LGDP => LGDS
#   lapply(models.var.2,
#          causality) %>%
#     lapply(.,function(x){x$Granger$p.value}) %>%
#     as.data.frame(.) %>% 
#     gather(.,
#            Country.Code,
#            `Granger Causality Test`) %>% 
#     mutate(H0 = 'H0: LGDP does not cause LGDS') -> Causality.Test.Granger_LGDP.LGDS
#   
#   # 6.5.2.3. AGREGADOS PARA PLOTAGEM
#   plyr::join_all(list(Causality.Test.Granger_LGDP.LGDS,
#                       Causality.Test.Granger_LGDS.LGDP),
#                  type = 'full',
#                  by = c('Country.Code' = 'Country.Code',
#                         'H0' = 'H0')) -> Causality.Test.All
#   
#   
#   # 7.3.3. RESUMO DE TODAS AS CAUSALIDADES DE GRANGER SIGNIFICATIVAS
#   Causality.Test.All %>% 
#     gather(`Causality Test`,
#            `Test P-Value`,
#            -Country.Code,
#            -H0) %>%
#     mutate(`P-Value.Code` = findInterval(`Test P-Value`,
#                                          vec = c(0.01, 0.05, 0.1),
#                                          rightmost.closed = T,
#                                          left.open = T)) %>% 
#     mutate(Country.Code = fct_reorder(Country.Code,
#                                       `Test P-Value`,
#                                       sum,
#                                       .desc = T),
#            `P-Value.Color` = case_when(`P-Value.Code` == 0 ~ 'p < 0.01',
#                                        `P-Value.Code` == 1 ~ 'p < 0.05',
#                                        `P-Value.Code` == 2 ~ 'p < 0.1',
#                                        `P-Value.Code` == 3 ~ 'Not Significant'),
#            `P-Value.Color` = factor(`P-Value.Color`)) %>% 
#     ggplot(.,aes(x = `Causality Test`,
#                  y = Country.Code,
#                  fill = `P-Value.Color`)) +
#     geom_tile(color = 'white',
#               size = 0.8) +
#     geom_text(aes(label = round(`Test P-Value`,3)),
#               color = 'white',
#               fontface = 'bold',
#               size = 2.5) +
#     gghighlight(`Test P-Value` < 0.1,
#                 unhighlighted_params = list(color = 'white',
#                                             label = NA)) +
#     facet_wrap(facets = vars(H0)) +
#     scale_fill_viridis(option = 'viridis',
#                        direction = -1,
#                        discrete = T) +
#     guides(fill = guide_legend(override.aes = list(size = 3))) +
#     labs(title = paste0('VAR(', lags, ') Granger Causality Tests'),
#          subtitle = 'Testing Bidirectional Causation',
#          fill = 'P-Value for\nRejecting H0',
#          x = 'Causality Test',
#          y = 'Country Code\n') + 
#     theme(axis.text.y = element_text(size = rel(0.6),
#                                      face = 'bold')) -> Granger.Causality.Tests
#   ggsave(Granger.Causality.Tests,
#          file = paste0('VAR(', lags, ') Granger Causality Tests.pdf'),
#          height = 11.69,
#          width = 8.27,
#          units = 'in')
#   
#   
#   
#   # models.var %>%
#   #   var.regression.summary(.) %>%
#   #   mutate(`P-Value.Code` = findInterval(`P-Value`,
#   #                                        vec = c(0.01, 0.05, 0.1),
#   #                                        rightmost.closed = T,
#   #                                        left.open = T)) %>% 
#   #   mutate(Country.Code = fct_reorder(Country.Code,
#   #                                     `P-Value`,
#   #                                     sum,
#   #                                     .desc = T),
#   #          `P-Value.Color` = case_when(`P-Value.Code` == 0 ~ 'p < 0.01',
#   #                                      `P-Value.Code` == 1 ~ 'p < 0.05',
#   #                                      `P-Value.Code` == 2 ~ 'p < 0.1',
#   #                                      `P-Value.Code` == 3 ~ 'Not Significant'),
#   #          `P-Value.Color` = factor(`P-Value.Color`)) %>% 
#   #   ggplot(.,aes(x = VAR.Coefficient,
#   #                y = Country.Code,
#   #                fill = `P-Value.Color`)) +
#   #   geom_tile(color = 'white',
#   #             size = 0.8) +
#   #   gghighlight(`P-Value` < 0.1,
#   #               unhighlighted_params = list(color = 'white',
#   #                                           label = NA)) +
#   #   facet_wrap(facets = vars(Variable)) +
#   #   scale_fill_viridis(option = 'viridis',
#   #                      direction = -1,
#   #                      discrete = T) +
#   #   scale_x_discrete(labels = function(x){str_replace_all(x,'.l','\ L') %>% str_to_upper(.) %>% str_sort(., numeric = T)}) +
#   #   guides(fill = guide_legend(title.position = 'top',
#   #                              title.hjust = 0,
#   #                              size = 3,
#   #                              nrow = 1)) +
#   #   labs(title = paste0('VAR(', lags, ') Models by Country'),
#   #        subtitle = 'Regression Equations for LGDP and LGDS',
#   #        fill = 'Significance of VAR Coefficients',
#   #        x = 'VAR Coefficients',
#   #        y = 'Country Code\n') + 
#   #   theme(axis.text = element_text(size = rel(0.6)),
#   #         axis.text.x = element_text(angle = ifelse(lags > 2,
#   #                                                   yes = 90,
#   #                                                   no = 0),
#   #                                    vjust = ifelse(lags > 2,
#   #                                                   yes = 0.5,
#   #                                                   no = 0)),
#   #         legend.position = 'bottom') -> LGDP.LGDS.VAR
#   # ggsave(LGDP.LGDS.VAR,
#   #        file = paste0('VAR(', lags, ') ', 'Results Summary.pdf'),
#   #        height = 11.69,
#   #        width = 8.27,
#   #        units = 'in')
#   # ggsave(LGDP.LGDS.VAR,
#   #        file = paste0('VAR(', lags, ') ', 'Results Summary.png'),
#   #        height = 11.69,
#   #        width = 8.27,
#   #        units = 'in')
# 
#   
# }
# ---------------------

lags <- 3
# lags <- 10

# 6.2. MODELOS VAR POR PAÍS
df.joined %>%
  dlply(.,'Country.Code',
        function(x){VAR(x %>%
                          ungroup(.) %>%
                          select(LGDS,
                                 LGDP) %>%
                          as.matrix(.),
                        p = lags,
                        type = 'const')}) -> models.var
  
# 6.3. ESTABILIDADE DOS MODELOS
tryapply(models.var,
         roots) %>%
  lapply(., abs) %>%
  lapply(., function(x){all(x < 1)}) %>%
  unlist(.) %>% which(.) %>% names(.) -> valid.var.countries
  
# 6.4. MODELOS ESTÁVEIS
# 6.4.1. GERAIS
df.joined %>%
  filter(Country.Code %in% valid.var.countries) %>% 
  dlply(.,'Country.Code',
        function(x){VAR(x %>%
                          ungroup(.) %>%
                          select(LGDS,
                                 LGDP) %>%
                          as.matrix(.),
                        p = lags,
                        type = 'const')}) -> models.var 

df.joined %>%
  filter(Country.Code %in% valid.var.countries) %>%
  dlply(.,'Country.Code',
        function(x){VAR(x %>%
                          ungroup(.) %>%
                          select(LGDP,
                                 LGDS) %>%
                          as.matrix(.),
                        p = lags,
                        type = 'const')}) -> models.var.2 

# 6.4.2. REGIONAIS
list(`Americas` = df.joined %>%
       filter(Country.Code %in% valid.var.countries,
              Region == 'Americas') %>% 
       dlply(.,'Country.Code',
             function(x){VAR(x %>%
                               ungroup(.) %>%
                               select(LGDS,
                                      LGDP) %>%
                               as.matrix(.),
                             p = lags,
                             type = 'const')}),
     `Asia-Pacific` = df.joined %>%
       filter(Country.Code %in% valid.var.countries,
              Region == 'Asia-Pacific') %>% 
       dlply(.,'Country.Code',
             function(x){VAR(x %>%
                               ungroup(.) %>%
                               select(LGDS,
                                      LGDP) %>%
                               as.matrix(.),
                             p = lags,
                             type = 'const')}),
     `Europe` = df.joined %>%
       filter(Country.Code %in% valid.var.countries,
              Region == 'Europe') %>% 
       dlply(.,'Country.Code',
             function(x){VAR(x %>%
                               ungroup(.) %>%
                               select(LGDS,
                                      LGDP) %>%
                               as.matrix(.),
                             p = lags,
                             type = 'const')}),
     `Middle East and North Africa` = df.joined %>%
       filter(Country.Code %in% valid.var.countries,
              Region == 'Middle East and North Africa') %>% 
       dlply(.,'Country.Code',
             function(x){VAR(x %>%
                               ungroup(.) %>%
                               select(LGDS,
                                      LGDP) %>%
                               as.matrix(.),
                             p = lags,
                             type = 'const')}),
     `Sub-Saharan Africa` = df.joined %>%
       filter(Country.Code %in% valid.var.countries,
              Region == 'Sub-Saharan Africa') %>% 
       dlply(.,'Country.Code',
             function(x){VAR(x %>%
                               ungroup(.) %>%
                               select(LGDS,
                                      LGDP) %>%
                               as.matrix(.),
                             p = lags,
                             type = 'const')})) -> models.var.regions

# 6.5. RESULTADOS
# 6.5.1. REGRESSÕES
for(country in names(models.var)){
  df.joined %>%
    filter(Country.Code == country) %>%
    ungroup(.) %>%
    select(Country.Name) %>%
    deframe(.) %>%
    unique(.) -> country.name

    stargazer(models.var[[country]]$varresult[1],
              models.var[[country]]$varresult[2],
              type = 'html',
              title = paste0('VAR Model (',
                             lags,
                             ') - ',
                             country.name),
              column.labels = c(names(models.var[[country]]$varresult[1]),
                                names(models.var[[country]]$varresult[2])),
              dep.var.labels = '',
              model.numbers = F,
              align = T) -> model.stargazer
    cat(model.stargazer,
        file = paste0('ModelosVAR(',lags,').doc'),
        append = T)
    }

# 6.5.2. CAUSALIDADE DE GRANGER
# 6.5.2.1. LGDS  => LGDP
lapply(models.var,
       causality) %>%
  lapply(.,function(x){x$Granger$p.value}) %>% 
  as.data.frame(.) %>% 
  gather(.,
         Country.Code,
         `Granger Causality Test`) %>% 
  mutate(H0 = 'H0: LGDS does not cause LGDP') -> Causality.Test.Granger_LGDS.LGDP
  
# 6.5.2.2. LGDP => LGDS
lapply(models.var.2,
       causality) %>%
  lapply(.,function(x){x$Granger$p.value}) %>%
  as.data.frame(.) %>% 
  gather(.,
         Country.Code,
         `Granger Causality Test`) %>% 
  mutate(H0 = 'H0: LGDP does not cause LGDS') -> Causality.Test.Granger_LGDP.LGDS

# 6.5.2.3. AGREGADOS PARA PLOTAGEM
plyr::join_all(list(Causality.Test.Granger_LGDP.LGDS,
                    Causality.Test.Granger_LGDS.LGDP),
               type = 'full',
               by = c('Country.Code' = 'Country.Code',
                      'H0' = 'H0')) -> Causality.Test.All


# 7. VISUALIZAÇÃO
# 7.1. CORES
# 7.1.1. INDEPENDENTES
pretty.black <- '#0e1111'
pretty.yellow <- cividis(1, begin = 1)
pretty.red <- '#E22712'
pretty.blue <- viridis::plasma(1)

# 7.1.2. CORES PARA REGIÕES
df.joined %>%
  ungroup(.) %>% 
  select(Region) %>%
  unique(.) %>%
  deframe(.) -> region.names

region.pal <- c(viridis::plasma(1,begin = 0.85),
                viridis::viridis(1,begin = 0.65),
                viridis::magma(1),
                '#E22712',
                viridis::plasma(1))

names(region.pal) <- region.names

# 7.2. VISUALIZAÇÕES PRELIMINARES
# 7.2.1. LGDP (DENSIDADES REGIONAIS)
df.joined %>%
  mutate(Region = fct_reorder(Region,
                              LGDP,
                              median)) %>%
  group_by(Region) %>%
  ggplot(.,aes(x = LGDP,
               y = Region,
               fill = Region)) +
  geom_density_ridges(scale = 1.7,
                      alpha = 0.8,
                      size = 0.8) +
  scale_fill_manual(values = region.pal) +
  guides(fill = F) +
  labs(title = 'LGDP Density by Region',
       x = 'LGDP',
       y = 'Region') +
  theme_ridges.2 -> LGDP.Density
ggsave(LGDP.Density,
       file = 'LGDP (Density).pdf',
       width = 11.69,
       height = 8.27,
       units = 'in')
ggsave(LGDP.Density,
       file = 'LGDP (Density).png',
       width = 11.69,
       height = 8.27,
       units = 'in')

# 7.2.2. LGDS (DENSIDADES REGIONAIS)
df.joined %>%
  mutate(Region = fct_reorder(Region,
                              LGDS,
                              median)) %>%
  group_by(Region) %>%
  ggplot(.,aes(x = LGDS,
               y = Region,
               fill = Region)) +
  geom_density_ridges(scale = 1.7,
                      alpha = 0.8,
                      size = 0.8) +
  scale_fill_manual(values = region.pal) +
  guides(fill = F) +
  labs(title = 'LGDS Density by Region',
       x = 'LGDS',
       y = 'Region') +
  theme_ridges.2 -> LGDS.Density
ggsave(LGDS.Density,
       file = 'LGDS (Density).pdf',
       width = 11.69,
       height = 8.27,
       units = 'in')
ggsave(LGDS.Density,
       file = 'LGDS (Density).png',
       width = 11.69,
       height = 8.27,
       units = 'in')

# 7.2.3. GDS (BOXPLOT)
df.joined %>%
  mutate(Region = fct_reorder(Region,
                              GDS,
                              median)) %>%
  group_by(Region) %>%
  ggplot(.,aes(y = GDS,
               x = Region,
               color = Region,
               fill = Region)) +
  geom_boxplot(size = 1.2,
               alpha = 0.8) +
  scale_color_manual(values = region.pal) +
  scale_fill_manual(values = region.pal) +
  scale_y_continuous(limits = c(NA,0.25),
                     labels = scales::percent) +
  guides(color = F,
         fill = F) +
  labs(title = 'GDS (%GDP) by Region',
       x = 'Region',
       y = 'GDS (%GDP)') -> GDS.Boxplot
ggsave(GDS.Boxplot,
       file = 'LGDPxGDS (Boxplot).pdf',
       width = 11.69,
       height = 8.27,
       units = 'in')
ggsave(GDS.Boxplot,
       file = 'LGDPxGDS (Boxplot).png',
       width = 11.69,
       height = 8.27,
       units = 'in')

# 7.2.4. LGDP x GDS (SCATTERPLOT)
df.joined %>%
  mutate(Region = fct_reorder(Region,
                              LGDP,
                              median,
                              .desc = T)) %>%
  ggplot(.,aes(x = GDS,
               y = LGDP)) +
  geom_point(aes(color = Region),
             alpha = 0.4,
             size = 3) + 
  geom_smooth(method = 'lm',
              color = pretty.black,
              size = 1.2) +
  scale_color_manual(values = region.pal) +
  scale_x_continuous(limits = c(NA,0.25),
                     labels = scales::percent) +
  guides(color = guide_legend(override.aes = list(alpha = 1,
                                                  size = 5))) +
  labs(title = 'LGDP and GDS (%GDP) by Region',
       x = 'GDS (%GDP)',
       y = 'LGDP') -> LGDP.GDS.Scatterplot
ggsave(LGDP.GDS.Scatterplot,
       file = 'LGDPxGDS (Scatterplot).pdf',
       width = 11.69,
       height = 8.27,
       units = 'in')
ggsave(LGDP.GDS.Scatterplot,
       file = 'LGDPxGDS (Scatterplot).png',
       width = 11.69,
       height = 8.27,
       units = 'in')

# 7.3. CAUSALIDADE DE GRANGER
# 7.3.1. LGDS  => LGDP
Causality.Test.Granger_LGDS.LGDP %>% 
  mutate(`P-Value.Code` = findInterval(`Granger Causality Test`,
                                       vec = c(0.01, 0.05, 0.1),
                                       rightmost.closed = T,
                                       left.open = T)) %>% 
  mutate(Country.Code = fct_reorder(Country.Code,
                                    `Granger Causality Test`),
         `P-Value.Color` = case_when(`P-Value.Code` == 0 ~ 'p < 0.01',
                                     `P-Value.Code` == 1 ~ 'p < 0.05',
                                     `P-Value.Code` == 2 ~ 'p < 0.1',
                                     `P-Value.Code` == 3 ~ 'Not Significant'),
         `P-Value.Color` = factor(`P-Value.Color`)) %>% 
  ggplot(.,aes(x = Country.Code,
               y = `Granger Causality Test`,
               fill = `P-Value.Color`,
               color = `P-Value.Color`)) +
  geom_bar(stat = 'identity',
           color = 'white') +
  gghighlight(`Granger Causality Test` < 0.1,
              unhighlighted_params = list(color = 'white',
                                          label = NA)) +
  geom_text(aes(label = round(`Granger Causality Test`,3)),
            fontface = 'bold',
            hjust = -0.25,
            size = 2.5) +
  coord_flip(ylim = c(0,1)) +
  scale_fill_viridis(option = 'viridis',
                     direction = -1,
                     discrete = T) +
  scale_color_viridis(option = 'viridis',
                      direction = -1,
                      discrete = T) +
  guides(fill = guide_legend(override.aes = list(size = 3))) +
  labs(title = paste0('VAR(', lags, ') Granger Causality Tests'),
       subtitle = unique(Causality.Test.Granger_LGDS.LGDP$H0),
       x = 'Country Code\n',
       y = 'Granger Test P-Value',
       fill = 'P-Value for\nRejecting H0') + 
  theme(axis.text.y = element_text(size = rel(0.6),
                                   face = 'bold')) -> LGDS.LGDP.Granger
ggsave(LGDS.LGDP.Granger,
       file = paste0('VAR(', lags, ') Granger Causality Tests (LGDS causes LGDP).pdf'),
       height = 11.69,
       width = 8.27,
       units = 'in')
ggsave(LGDS.LGDP.Granger,
       file = paste0('VAR(', lags, ') Granger Causality Tests (LGDS causes LGDP).png'),
       height = 11.69,
       width = 8.27,
       units = 'in')

# 7.3.2. LGDP => LGDS
Causality.Test.Granger_LGDP.LGDS %>% 
  mutate(`P-Value.Code` = findInterval(`Granger Causality Test`,
                                       vec = c(0.01, 0.05, 0.1),
                                       rightmost.closed = T,
                                       left.open = T)) %>% 
  mutate(Country.Code = fct_reorder(Country.Code,
                                    `Granger Causality Test`),
         `P-Value.Color` = case_when(`P-Value.Code` == 0 ~ 'p < 0.01',
                                     `P-Value.Code` == 1 ~ 'p < 0.05',
                                     `P-Value.Code` == 2 ~ 'p < 0.1',
                                     `P-Value.Code` == 3 ~ 'Not Significant'),
         `P-Value.Color` = factor(`P-Value.Color`)) %>% 
  ggplot(.,aes(x = Country.Code,
               y = `Granger Causality Test`,
               fill = `P-Value.Color`,
               color = `P-Value.Color`)) +
  geom_bar(stat = 'identity',
           color = 'white') +
  gghighlight(`Granger Causality Test` < 0.1,
              unhighlighted_params = list(color = 'white',
                                          label = NA)) +
  geom_text(aes(label = round(`Granger Causality Test`,3)),
            fontface = 'bold',
            hjust = -0.25,
            size = 2.5) +
  coord_flip(ylim = c(0,1)) + 
  scale_fill_viridis(option = 'viridis',
                     direction = -1,
                     discrete = T) +
  scale_color_viridis(option = 'viridis',
                      direction = -1,
                      discrete = T) +
  guides(fill = guide_legend(override.aes = list(size = 3))) +
  labs(title = paste0('VAR(', lags, ') Granger Causality Tests'),
       subtitle = unique(Causality.Test.Granger_LGDP.LGDS$H0),
       x = 'Country Code\n',
       y = 'Granger Test P-Value',
       fill = 'P-Value for\nRejecting H0') + 
  theme(axis.text.y = element_text(size = rel(0.6),
                                   face = 'bold')) -> LGDP.LGDS.Granger
ggsave(LGDP.LGDS.Granger,
       file = paste0('VAR(', lags, ') Granger Causality Tests (LGDP causes LGDS).pdf'),
       height = 11.69,
       width = 8.27,
       units = 'in')
ggsave(LGDP.LGDS.Granger,
       file = paste0('VAR(', lags, ') Granger Causality Tests (LGDP causes LGDS).png'),
       height = 11.69,
       width = 8.27,
       units = 'in')

# 7.3.3. RESUMO DE TODAS AS CAUSALIDADES DE GRANGER SIGNIFICATIVAS
# 7.3.3.1. GERAL
Causality.Test.All %>% 
  gather(`Causality Test`,
         `Test P-Value`,
         -Country.Code,
         -H0) %>%
  mutate(`P-Value.Code` = findInterval(`Test P-Value`,
                                       vec = c(0.01, 0.05, 0.1),
                                       rightmost.closed = T,
                                       left.open = T)) %>% 
  mutate(Country.Code = fct_reorder(Country.Code,
                                    `Test P-Value`,
                                    sum,
                                    .desc = T),
         `P-Value.Color` = case_when(`P-Value.Code` == 0 ~ 'p < 0.01',
                                     `P-Value.Code` == 1 ~ 'p < 0.05',
                                     `P-Value.Code` == 2 ~ 'p < 0.1',
                                     `P-Value.Code` == 3 ~ 'Not Significant'),
         `P-Value.Color` = factor(`P-Value.Color`)) %>% 
  ggplot(.,aes(x = `Causality Test`,
               y = Country.Code,
               fill = `P-Value.Color`)) +
  geom_tile(color = 'white',
            size = 0.8) +
  geom_text(aes(label = round(`Test P-Value`,3)),
            color = 'white',
            fontface = 'bold',
            size = 2.5) +
  gghighlight(`Test P-Value` < 0.1,
              unhighlighted_params = list(color = 'white',
                                          label = NA)) +
  facet_wrap(facets = vars(H0)) +
  scale_fill_viridis(option = 'viridis',
                     direction = -1,
                     discrete = T) +
  guides(fill = guide_legend(title.position = 'top',
                             title.hjust = 0.5,
                             size = 3,
                             nrow = 1)) +
  labs(title = paste0('VAR(', lags, ') Granger Causality Tests'),
       subtitle = 'Testing Bidirectional Causation',
       fill = 'P-Value for Rejecting H0',
       x = 'P-Value',
       y = 'Country Code\n') + 
  theme(axis.text.y = element_text(size = rel(0.6),
                                   face = 'bold'),
        legend.position = 'bottom') -> Granger.Causality.Tests
ggsave(Granger.Causality.Tests,
       file = paste0('VAR (', lags, ') Granger Causality Tests.pdf'),
       height = 11.69,
       width = 8.27,
       units = 'in')
ggsave(Granger.Causality.Tests,
       file = paste0('VAR (', lags, ') Granger Causality Tests.png'),
       height = 11.69,
       width = 8.27,
       units = 'in')


# # 7.3.3.2. REGIONAL
# for (region in names(macro.regions)){
#   Causality.Test.All %>% 
#     mutate(Region = case_when(Country.Code %in% macro.regions$Americas ~ 'Americas',
#                               Country.Code %in% macro.regions$`Asia-Pacific` ~ 'Asia-Pacific',
#                               Country.Code %in% macro.regions$Europe ~ 'Europe',
#                               Country.Code %in% macro.regions$`Middle East and North Africa` ~ 'Middle East and North Africa',
#                               Country.Code %in% macro.regions$`Sub-Saharan Africa` ~ 'Sub-Saharan Africa')) %>%
#     filter(Region == region) %>%
#     select(-Region) %>%
#     gather(`Causality Test`,
#            `Test P-Value`,
#            -Country.Code,
#            -H0) %>% 
#     mutate(`P-Value.Code` = findInterval(`Test P-Value`,
#                                          vec = c(0.01, 0.05, 0.1),
#                                          rightmost.closed = T,
#                                          left.open = T)) %>% 
#     mutate(Country.Code = fct_reorder(Country.Code,
#                                       `Test P-Value`,
#                                       sum,
#                                       .desc = T),
#            `P-Value.Color` = case_when(`P-Value.Code` == 0 ~ 'p < 0.01',
#                                        `P-Value.Code` == 1 ~ 'p < 0.05',
#                                        `P-Value.Code` == 2 ~ 'p < 0.1',
#                                        `P-Value.Code` == 3 ~ 'Not Significant'),
#            `P-Value.Color` = factor(`P-Value.Color`)) %>% 
#     ggplot(.,aes(x = `Causality Test`,
#                  y = Country.Code,
#                  fill = `P-Value.Color`)) +
#     geom_tile(color = 'white',
#               size = 0.8) +
#     geom_text(aes(label = round(`Test P-Value`,3)),
#               color = 'white',
#               fontface = 'bold',
#               size = 2.5) +
#     gghighlight(`Test P-Value` < 0.1,
#                 unhighlighted_params = list(color = 'white',
#                                             label = NA)) +
#     facet_wrap(facets = vars(H0)) +
#     scale_fill_viridis(option = 'viridis',
#                        direction = -1,
#                        discrete = T) +
#     guides(fill = guide_legend(title.position = 'top',
#                                title.hjust = 0.5,
#                                size = 3,
#                                nrow = 1)) +
#     labs(title = paste0('VAR(', lags, ') Granger Causality Tests (', region, ')'),
#          subtitle = 'Testing Bidirectional Causation',
#          fill = 'P-Value for Rejecting H0',
#          x = 'P-Value',
#          y = 'Country Code\n') + 
#     theme(axis.text.y = element_text(size = rel(0.6)),
#           legend.position = 'bottom') -> Granger.Causality.Tests
#   ggsave(Granger.Causality.Tests,
#          file = paste0('VAR (', lags, ') Granger Causality Tests(', region, ').pdf'),
#          width = 11.69,
#          height = 8.27,
#          units = 'in')
#   ggsave(Granger.Causality.Tests,
#          file = paste0('VAR (', lags, ') Granger Causality Tests(', region, ').png'),
#          width = 11.69,
#          height = 8.27,
#          units = 'in')
# }

# 7.4. RESUMO DE TODAS AS REGRESSÕES VAR
var.regression.summary <- function(model){
  plyr::join(plyr::join_all(list(sapply(model, function(x){
    summary(x)$varresult$LGDP$coefficients[,1]}) %>%
      t(.) %>%
      as.data.frame(.[,]) %>%
      mutate(Country.Code = names(model),
             Country.Code = factor(Country.Code)) %>%
      pivot_longer(.,
                   cols = c('LGDS.l1',
                            'LGDP.l1',
                            'const'),
                   names_to = 'VAR.Coefficient',
                   values_to = 'Value'),
    
    sapply(model, function(x){
      summary(x)$varresult$LGDP$coefficients[,4]}) %>%
      t(.) %>%
      as.data.frame(.[,]) %>%
      mutate(Country.Code = names(model),
             Country.Code = factor(Country.Code)) %>%
      pivot_longer(.,
                   cols = names(model[[1]][[1]][[1]][[1]]),
                   names_to = 'VAR.Coefficient',
                   values_to = 'P-Value')),
    
    type = 'full',
    by = c('Country.Code' = 'Country.Code',
           'VAR.Coefficient' = 'VAR.Coefficient')) %>%
      mutate(Variable = 'LGDP',
             Variable = factor(Variable)),
    plyr::join_all(list(sapply(model, function(x){
      summary(x)$varresult$LGDS$coefficients[,1]}) %>%
        t(.) %>%
        as.data.frame(.[,]) %>%
        mutate(Country.Code = names(model),
               Country.Code = factor(Country.Code)) %>%
        pivot_longer(.,
                     cols = names(model[[1]][[1]][[1]][[1]]),
                     names_to = 'VAR.Coefficient',
                     values_to = 'Value'),
      
      sapply(model, function(x){
        summary(x)$varresult$LGDS$coefficients[,4]}) %>%
        t(.) %>%
        as.data.frame(.[,]) %>%
        mutate(Country.Code = names(model),
               Country.Code = factor(Country.Code)) %>%
        pivot_longer(.,
                     cols = names(model[[1]][[1]][[1]][[1]]),
                     names_to = 'VAR.Coefficient',
                     values_to = 'P-Value')),
      
      type = 'full',
      by = c('Country.Code' = 'Country.Code',
             'VAR.Coefficient' = 'VAR.Coefficient')) %>%
      mutate(Variable = 'LGDS',
             Variable = factor(Variable)),
    type = 'full',
    by = c('Country.Code' = 'Country.Code',
           'VAR.Coefficient' = 'VAR.Coefficient',
           'Variable' = 'Variable')) %>%
    return(.)
}


# 7.4.1. GERAL
models.var %>%
  var.regression.summary(.) %>%
  mutate(`P-Value.Code` = findInterval(`P-Value`,
                                       vec = c(0.01, 0.05, 0.1),
                                       rightmost.closed = T,
                                       left.open = T)) %>% 
  mutate(Country.Code = fct_reorder(Country.Code,
                                    `P-Value`,
                                    sum,
                                    .desc = T),
         `P-Value.Color` = case_when(`P-Value.Code` == 0 ~ 'p < 0.01',
                                     `P-Value.Code` == 1 ~ 'p < 0.05',
                                     `P-Value.Code` == 2 ~ 'p < 0.1',
                                     `P-Value.Code` == 3 ~ 'Not Significant'),
         `P-Value.Color` = factor(`P-Value.Color`)) %>% 
  ggplot(.,aes(x = VAR.Coefficient,
               y = Country.Code,
               fill = `P-Value.Color`)) +
  geom_tile(color = 'white',
            size = 0.8) +
  gghighlight(`P-Value` < 0.1,
              unhighlighted_params = list(color = 'white',
                                          label = NA)) +
  facet_wrap(facets = vars(Variable)) +
  scale_fill_viridis(option = 'viridis',
                     direction = -1,
                     discrete = T) +
  scale_x_discrete(labels = function(x){str_replace_all(x,'.l','\ L') %>% str_to_upper(.) %>% str_sort(., numeric = T)}) +
  guides(fill = guide_legend(title.position = 'top',
                             title.hjust = 0.5,
                             size = 3,
                             nrow = 1)) +
  labs(title = paste0('VAR(', lags, ') Models by Country'),
       subtitle = 'Regression Equations for LGDP and LGDS',
       fill = 'Significance of VAR Coefficients',
       x = 'VAR Coefficients',
       y = 'Country Code\n') + 
  theme(axis.text = element_text(size = rel(0.6),
                                 face = 'bold'),
        axis.text.x = element_text(angle = ifelse(lags > 2,
                                                  yes = 90,
                                                  no = 0),
                                   vjust = ifelse(lags > 2,
                                                  yes = 0.5,
                                                  no = 0)),
        legend.position = 'bottom') -> LGDP.LGDS.VAR
ggsave(LGDP.LGDS.VAR,
       file = paste0('VAR(', lags, ') ', 'Results Summary.pdf'),
       height = 11.69,
       width = 8.27,
       units = 'in')
ggsave(LGDP.LGDS.VAR,
       file = paste0('VAR(', lags, ') ', 'Results Summary.png'),
       height = 11.69,
       width = 8.27,
       units = 'in')

# 7.4.2. REGIONAL
for(region in names(models.var.regions)){
  models.var.regions[[region]] %>% 
    var.regression.summary(.) %>%
    mutate(`P-Value.Code` = findInterval(`P-Value`,
                                         vec = c(0.01, 0.05, 0.1),
                                         rightmost.closed = T,
                                         left.open = T)) %>% 
    mutate(Country.Code = fct_reorder(Country.Code,
                                      `P-Value`,
                                      sum,
                                      .desc = T),
           `P-Value.Color` = case_when(`P-Value.Code` == 0 ~ 'p < 0.01',
                                       `P-Value.Code` == 1 ~ 'p < 0.05',
                                       `P-Value.Code` == 2 ~ 'p < 0.1',
                                       `P-Value.Code` == 3 ~ 'Not Significant'),
           `P-Value.Color` = factor(`P-Value.Color`)) %>% 
    ggplot(.,aes(x = VAR.Coefficient,
                 y = Country.Code,
                 fill = `P-Value.Color`)) +
    geom_tile(color = 'white',
              size = 0.8) +
    gghighlight(`P-Value` < 0.1,
                unhighlighted_params = list(color = 'white',
                                            label = NA)) +
    facet_wrap(facets = vars(Variable)) +
    scale_fill_viridis(option = 'viridis',
                       direction = -1,
                       discrete = T) +
    scale_x_discrete(labels = function(x){str_replace_all(x,'.l','\ L') %>% str_to_upper(.) %>% str_sort(., numeric = T)}) +
    guides(fill = guide_legend(title.position = 'top',
                               title.hjust = 0.5,
                               size = 3,
                               nrow = 1)) +
    labs(title = paste0('VAR(', lags, ') Models by Country (', region, ')'),
         subtitle = 'Regression Equations for LGDP and LGDS',
         fill = 'Significance of VAR Coefficients',
         x = 'VAR Coefficients',
         y = 'Country Code\n') + 
    theme(axis.text = element_text(size = rel(0.6),
                                   face = 'bold'),
          axis.text.x = element_text(angle = ifelse(lags > 3,
                                                    yes = 90,
                                                    no = 0),
                                     vjust = ifelse(lags > 2,
                                                    yes = 0.5,
                                                    no = 0)),
          legend.position = 'bottom') -> LGDP.LGDS.VAR
  
  ggsave(LGDP.LGDS.VAR,
         file = paste0('VAR(', lags,') ','Results Summary','(',region,').pdf'),
         width = 11.69,
         height = 8.27,
         units = 'in')
  
  ggsave(LGDP.LGDS.VAR,
         file = paste0('VAR(', lags,') ','Results Summary','(',region,').png'),
         width = 11.69,
         height = 8.27,
         units = 'in')
}

# 7.5. FUNÇÕES DE RESPOSTA A IMPULSO (IRF)
# -------------------------------------------------------------------------------
# `extract_varirf()` extracts the impulse reponse vector, along with the upper and 
# lower confidence interval vectors, created by the `irf()` function in the `vars`
# package and puts them into a tidy dataframe that allows for easier 
# impulse-reponse function plotting, particularly with the ggplot2. `extract_varirf()`
# accepts single or multiple 'varirf' list objects created by `irf()`, provided they 
# are created from the same dataset and of the same length. For additional details
# and examples of usage, please consult:
# mentalbreaks.rbind.io/posts/impulse-reponse-plots-with-vars-and-ggplot2
# 
# @anguyen1210
# -------------------------------------------------------------------------------
extract_varirf <- function(...){
  
  varirf_object <- list(...) #list one or more varirf input objects
  
  get_vec_length <- function(list_item){nrow(list_item[[1]][[1]])}
  
  if (!("varirf" %in% mapply(class, varirf_object))){
    stop("this function only accepts 'varirf' class objects")
  }
  
  if (length(unique(mapply(class, varirf_object)))!=1){
    stop("all input items must be 'varirf' class objects")
  }    
  if (length(unique(mapply(get_vec_length, varirf_object)))!=1){
    stop("all irf vectors must have the same length")   
  }  
  
  period <- as.data.frame(0:(nrow(varirf_object[[1]][[1]][[1]])-1)) 
  names(period) <- "period"
  
  for (l in 1:length(varirf_object)){
    for (i in 1:3){
      for (j in 1:dim(varirf_object[[l]][[i]][[1]])[2]){
        for (k in 1:length(varirf_object[[l]][[1]])){
          temp_colname <- paste(names(varirf_object[[l]][i]), #vector type (irf, lower, or upper)
                                names(varirf_object[[l]][[i]])[k], #impulse name
                                colnames(varirf_object[[l]][[i]][[k]])[j], #response name
                                sep = "_")
          
          temp <- as.data.frame(varirf_object[[l]][[i]][[k]][, j]) #extracts the vector
          
          names(temp) <- temp_colname #add the column name (vectortype_impulse_reponse)
          period <- cbind(period, temp) 
        }
        
      }
    }
  }
  names(period) <- tolower(names(period))
  return(period)
}


for(country in names(models.var)){
  models.var[[country]] %>%
    irf(., n.ahead = 10) %>%
    extract_varirf(.) %>%
    ggplot(aes(x = period,
               y = irf_lgdp_lgdp,
               ymin = lower_lgdp_lgdp,
               ymax = upper_lgdp_lgdp)) +
    geom_hline(yintercept = 0,
               color = pretty.black) +
    geom_ribbon(fill = 'grey',
                alpha = 0.2,
                color = 'grey10',
                linetype = 'dashed') +
    geom_line(aes(color = last(irf_lgdp_lgdp) >= 0),
              size = 1.2) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = c('TRUE' = pretty.blue,
                                  'FALSE' = pretty.red)) +
    guides(color = F) +
    labs(title = paste0('VAR(',
                        lags,
                        ') Orthogonal Impulse Response (',
                        country,
                        ')'),
         subtitle = 'Response to Shock in LGDP (95% CI)',
         x = 'Year',
         y = 'LGDP') -> IRF_LGDP.LGDP

  models.var[[country]] %>%
    irf(., n.ahead = 10) %>%
    extract_varirf(.) %>%
    ggplot(aes(x = period,
               y = irf_lgdp_lgds,
               ymin = lower_lgdp_lgds,
               ymax = upper_lgdp_lgds)) +
    geom_hline(yintercept = 0,
               color = pretty.black) +
    geom_ribbon(fill='grey',
                alpha= 0.2,
                color='grey10',
                linetype='dashed') +
    geom_line(aes(color = last(irf_lgdp_lgds) >= 0),
              size = 1.2) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = c('TRUE' = pretty.blue,
                                  'FALSE' = pretty.red)) +
    guides(color = F) +
    labs(title = paste0('VAR(',
                        lags,
                        ') Orthogonal Impulse Response (',
                        country,
                        ')'),
         subtitle = 'Response to Shock in LGDP (95% CI)',
         x = 'Year',
         y = 'LGDS') -> IRF_LGDP.LGDS

  models.var[[country]] %>%
    irf(., n.ahead = 10) %>%
    extract_varirf(.) %>%
    ggplot(aes(x = period,
               y = irf_lgds_lgdp,
               ymin = lower_lgds_lgdp,
               ymax = upper_lgds_lgdp)) +
    geom_hline(yintercept = 0,
               color = pretty.black) +
    geom_ribbon(fill='grey',
                alpha= 0.2,
                color='grey10',
                linetype='dashed') +
    geom_line(aes(color = last(irf_lgds_lgdp) >= 0),
              size = 1.2) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = c('TRUE' = pretty.blue,
                                  'FALSE' = pretty.red)) +
    guides(color = F) +
    labs(title = paste0('VAR(',
                        lags,
                        ') Orthogonal Impulse Response (',
                        country,
                        ')'),
         subtitle = 'Response to Shock in LGDS (95% CI)',
         x = 'Year',
         y = 'LGDP') -> IRF_LGDS.LGDP

  models.var[[country]] %>%
    irf(., n.ahead = 10) %>%
    extract_varirf(.) %>%
    ggplot(aes(x = period,
               y = irf_lgds_lgds,
               ymin = lower_lgds_lgds,
               ymax = upper_lgds_lgds)) +
    geom_hline(yintercept = 0,
               color = pretty.black) +
    geom_ribbon(fill='grey',
                alpha= 0.2,
                color='grey10',
                linetype='dashed') +
    geom_line(aes(color = last(irf_lgds_lgds) >= 0),
              size = 1.2) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = c('TRUE' = pretty.blue,
                                  'FALSE' = pretty.red)) +
    guides(color = F) +
    labs(title = paste0('VAR(',
                        lags,
                        ') Orthogonal Impulse Response (',
                        country,
                        ')'),
         subtitle = 'Response to Shock in LGDS (95% CI)',
         x = 'Year',
         y = 'LGDS') -> IRF_LGDS.LGDS
  
  (IRF_LGDP.LGDP / IRF_LGDP.LGDS) | (IRF_LGDS.LGDP / IRF_LGDS.LGDS)

  ggsave((IRF_LGDP.LGDP / IRF_LGDP.LGDS) | (IRF_LGDS.LGDP / IRF_LGDS.LGDS),
         file = paste0('VAR(',
                       lags,
                       ') ',
                       'IRF (',
                       country,
                       ').pdf'),
         height = 8.27,
         width = 11.69,
         units = 'in')

  ggsave((IRF_LGDP.LGDP / IRF_LGDP.LGDS) | (IRF_LGDS.LGDP / IRF_LGDS.LGDS),
         file = paste0('VAR(',
                       lags,
                       ') ',
                       'IRF (',
                       country,
                       ').png'),
         height = 8.27,
         width = 11.69,
         units = 'in')
  }
