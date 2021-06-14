library(tidyr)
library(plyr) # volgorde van packages laden is belangrijk! plyr voor dplyr vanwege conflicterende functies
library(dplyr)
library(readxl)
library(readr)
library(zoo)
library(plotly)
library(cbsodataR)

setwd("C:/Users/basva/Dropbox (Birch Consultants)/EVER - Birch-UU-UvT/C. Werkdocumenten/Dashboards/Dashboard - weerbaarheid & wendbaarheid")

# ----LADEN TABELLEN ----
# hoofdcategorieën BRC
BRChoofd <- read.csv2("brc_hoofd.csv", encoding = 'UTF-8') %>%
  select(BRC.Omschrijving.Beroepsgroep.Niv.1, BRC.Omschrijving.Beroepsgroep.Niv.2, BRC.Omschrijving.Beroepsgroep.Niv.3) %>%
  rename("Niveau1" = BRC.Omschrijving.Beroepsgroep.Niv.1, "Niveau2" = BRC.Omschrijving.Beroepsgroep.Niv.2, "Niveau3" = BRC.Omschrijving.Beroepsgroep.Niv.3)

# Verbinding hoofdcategorieen met beroepscodes UWV
BRCregister <- read_xlsx("BRC-ISCO register mapping.xlsx", sheet = "BRC2014-ISCO-Register") %>%
  select(BEROEP_CD_UWV, BEROEPNAAM_UWV, BEROEPNAAM_BRC) %>%
  drop_na(BEROEP_CD_UWV) %>%
  left_join(BRChoofd, by = c("BEROEPNAAM_BRC" = "Niveau3")) %>%
  rename("Niveau3" = BEROEPNAAM_BRC)

# CBS <- cbs_get_datasets() %>%
#   filter(grepl("NED|ned", Identifier), grepl("Regio", Title))

# Odata verbinding met CBS, regiotabel
Regio <- cbs_get_data(id = "84929NED") %>%
  select(2:3, 9:10, 27:28, 50, 52:54)

# CBS laadt onnodige whitespace in tabellen, hiermee halen we dat weg
Regio <- as.data.frame(apply(Regio, 2, trimws))

# Koppeltabel maken voor naamgeving gemeenten UWV en CBS
UWV_CBS <- read_xlsx("Gemeente_CBS-UWV.xlsx", sheet = "Gemeenten2021_UWV")

Regio <- UWV_CBS %>% 
  inner_join(Regio, by = c("GM_Code" = "Code_1")) %>%
  rename(GEMEENTE = Naam_2, COROP = Naam_9, PROVINCIE = Naam_27)

# load vacaturedata 
Dir <- "Vacaturedata"
Files <- list.files(path = Dir, pattern = "UWV*", full.names = TRUE)

Bron <- ldply(Files, read.csv2) %>%
  filter(REC_TYPE == "Vacature") %>%
  select(PEILDATUM, BEROEP_CD, GEMEENTE, AANTAL) %>%
  mutate(PEILDATUM = as.Date(PEILDATUM, format = "%d-%m-%Y"),
         KWARTAAL = as.yearqtr(PEILDATUM)) %>%
  rename(GM_UWV = GEMEENTE)

# Tabel die als bron dient voor wendbaarheid en weerbaarheid
Vacatures <- filter(Bron, KWARTAAL == "2020 Q1" | KWARTAAL == "2021 Q1")

Tijd_Vac <- Bron %>%
  inner_join(Regio, by = "GM_UWV") %>%
  group_by(PEILDATUM, PROVINCIE, COROP, GEMEENTE) %>%
  summarise(AANTAL = sum(AANTAL))

GM_totaal <- Vacatures %>%
  inner_join(Regio, by= "GM_UWV") %>%
  select(KWARTAAL, PEILDATUM, PROVINCIE, COROP, GEMEENTE, AANTAL) %>%
  group_by(KWARTAAL, PEILDATUM, PROVINCIE, COROP, GEMEENTE) %>%
  summarise(AANTAL = sum(AANTAL)) %>%
  ungroup() %>%
  group_by(KWARTAAL, PROVINCIE, COROP, GEMEENTE) %>%
  summarise(totaal = mean(AANTAL)) # kwartaalgemiddelde van som beroepsgroepen per peildatum per gemeente in Q1 2020 (baseline churn)

GM_tot20 <- filter(GM_totaal, KWARTAAL == "2020 Q1") # Q1 2020 is de baseline voor de analyse

#---- WEERBAARHEID ----

# weerbaarheid = bounceback vacaturepeil tov vorig jaar. In Nederland
Weer_NL <- Vacatures %>% 
  group_by(KWARTAAL, PEILDATUM) %>%
  summarise(AANTAL = sum(AANTAL)) %>%
  ungroup() %>%
  group_by(KWARTAAL) %>%
  summarise(GEM = mean(AANTAL)) %>% # kwartaalgemiddelde van som beroepsgroepen per peildatum in Q1 2020 (baseline)
  pivot_wider(names_from = KWARTAAL, values_from = GEM) %>%
  mutate(NL_groei = (`2021 Q1` - `2020 Q1`)/`2020 Q1`)

# weerbaarheid per regio, CBS gemeenten (kan ook COROP)
Weer_Regio <- Vacatures %>%
  inner_join(Regio, by = "GM_UWV") %>%
  select(KWARTAAL, PEILDATUM, PROVINCIE, COROP, GEMEENTE, AANTAL) %>%
  # filter(GEMEENTE == "Kaag en Braassem") %>%
  group_by(KWARTAAL, PEILDATUM, PROVINCIE, COROP, GEMEENTE) %>%
  summarise(AANTAL = sum(AANTAL)) %>%
  ungroup() %>%
  group_by(KWARTAAL, PROVINCIE, COROP, GEMEENTE) %>%
  summarise(GEM = mean(AANTAL)) %>%
  pivot_wider(names_from = KWARTAAL, values_from = GEM) %>%
  mutate(Reg_groei = (`2021 Q1` - `2020 Q1`)/`2020 Q1`)

# Provincie groei
Weer_Provincie <- Weer_Regio %>%
  group_by(PROVINCIE) %>%
  summarise(`2020 Q1` = sum(`2020 Q1`),
            `2021 Q1`= sum(`2021 Q1`)) %>%
  mutate(Prov_groei = (`2021 Q1` - `2020 Q1`)/`2020 Q1`)

#---- CHURN ----
# wendbaarheid = verschil in portfolio vacatures
Wend_NL <- Vacatures %>% 
  left_join(BRCregister, by = c("BEROEP_CD" = "BEROEP_CD_UWV")) %>%
  group_by(KWARTAAL, PEILDATUM, Niveau2) %>%
  summarise(AANTAL = sum(AANTAL)) %>%
  ungroup() %>%
  mutate(Niveau1 = ifelse(is.na(Niveau2), "Overig", Niveau2)) %>%
  group_by(KWARTAAL, Niveau2) %>%
  summarise(GEM = mean(AANTAL)) %>%
  ungroup() %>%
  pivot_wider(names_from = KWARTAAL, values_from = GEM) %>%
  mutate(`2021 Q1` = ifelse(is.na(`2021 Q1`), 0, `2021 Q1`),
         `2020 Q1` = ifelse(is.na(`2020 Q1`), 0, `2020 Q1`),
         totaal20 = Weer_NL$`2020 Q1`, 
         verschil2021 = abs(`2021 Q1` - `2020 Q1`),
         churn = verschil2021/totaal20) %>%
  summarise(NL_churn = sum(churn))

# regionale wendbaarheid
Niv_Regio <- Vacatures %>% 
  inner_join(Regio, by = "GM_UWV") %>%
  left_join(BRCregister, by = c("BEROEP_CD" = "BEROEP_CD_UWV")) %>%
  select(KWARTAAL, PEILDATUM, GEMEENTE, Niveau2, AANTAL) %>%
  # filter(GEMEENTE == "Kaag en Braassem") %>%
  group_by(KWARTAAL, PEILDATUM, GEMEENTE, Niveau2) %>%
  summarise(AANTAL = sum(AANTAL)) %>%
  ungroup() %>%
  mutate(Niveau2 = ifelse(is.na(Niveau2), "Overig", Niveau2)) %>%
  group_by(KWARTAAL, GEMEENTE, Niveau2) %>%
  mutate(GEM = mean(AANTAL)) %>%
  ungroup() %>%
  select(-PEILDATUM, -AANTAL) %>%
  distinct() %>%
  pivot_wider(names_from = KWARTAAL, values_from = GEM) %>%
  mutate(`2021 Q1` = ifelse(is.na(`2021 Q1`), 0, `2021 Q1`),
         `2020 Q1` = ifelse(is.na(`2020 Q1`), 0, `2020 Q1`)) %>%
  left_join(GM_tot20, by = "GEMEENTE") %>%
  group_by(GEMEENTE) %>%
  mutate(verschil2021 = abs(`2021 Q1` - `2020 Q1`),
         churn = verschil2021/totaal)

Wend_Regio <- summarise(Niv_Regio, Reg_churn = sum(churn))

Wend_Provincie <- Niv_Regio %>%
  group_by(PROVINCIE, Niveau2) %>%
  summarise(`2020 Q1` = sum(`2020 Q1`),
            `2021 Q1`= sum(`2021 Q1`),
            totaal = sum(totaal)) %>%
  mutate(verschil2021 = abs(`2021 Q1` - `2020 Q1`),
         churn = verschil2021/totaal) %>%
  ungroup() %>%
  group_by(PROVINCIE) %>% 
  summarise(Prov_churn = sum(churn))

# Tabel voor demonstratie verschillen in Niveau1 beroepsgroepen absoluut
Niveau1_Totaal <- Niv_Regio %>%
  select(PROVINCIE, COROP, GEMEENTE, Niveau2, `2020 Q1`, `2021 Q1`) %>%
  inner_join(BRChoofd, by = "Niveau2") %>%
  select(-Niveau3) %>%
  distinct() %>%
  group_by(GEMEENTE, Niveau1) %>%
  mutate(`2020 Q1` = sum(`2020 Q1`),
         `2021 Q1` = sum(`2021 Q1`)) %>%
  select(-Niveau2) %>%
  distinct() %>%
  pivot_longer(cols = `2020 Q1`:`2021 Q1`, names_to = "KWARTAAL", values_to = "AANTAL")


#samenvoegen weerbaarheid indicator en wendbaarheid indicator
Weer_Wend <- Wend_Regio %>%
  left_join(Weer_Regio, by = "GEMEENTE") %>%
  mutate(NL_groei = Weer_NL$NL_groei,
         NL_churn = Wend_NL$NL_churn,
         Reg_FaChurn = Reg_churn / abs(Reg_groei),
         Reg_ExChurn = Reg_churn - abs(Reg_groei),
         NL_FaChurn = NL_churn / NL_groei,
         NL_ExChurn = NL_churn - abs(NL_groei),
         Delta_ExChurn = Reg_ExChurn / NL_ExChurn,
         Delta_groei = (1+Reg_groei) / (1+NL_groei)) %>%
  left_join(Regio, by = "GEMEENTE") %>%
  select(-GM_UWV) %>%
  distinct()

# ---- write RDS ----
Data_WW <- list(Regio = Regio,
                Tijd_Vac = Tijd_Vac,
                Niveau1_Totaal = Niveau1_Totaal,
                Weer_NL = Weer_NL,
                Weer_Provincie = Weer_Provincie,
                Wend_Provincie = Wend_Provincie,
                Wend_NL = Wend_NL,
                Weer_Wend = Weer_Wend)

write_rds(Data_WW, "Data_WW.rds")


#---- testground voor grafieken ----

fig <- Tijd_Vac %>%
  filter(PROVINCIE == "Flevoland") %>%
  group_by(PEILDATUM) %>%
  summarise(AANTAL = sum(AANTAL)) %>%
  plot_ly(
    x = ~PEILDATUM,
    y = ~AANTAL,
    type = "scatter",
    mode = "lines",
    line = list(color = "#C0411D", width = 3)) %>%
  layout(xaxis = list(type = "date", ticklabelmode = "period"))

fig

