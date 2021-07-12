############## UI scriptlibrary(shiny)
library(dplyr)
library(ggplot2)

# Chargement des bases de données Abondance + Pred
load('Biolit_Ab.RData')
load('Biolit_Pred.RData')






# Préparation Pred
Pred3= Pred2 %>% select(-Cover)
DF=cbind(df3,Pred3) %>% select(Abb, colnames(Pred3)) %>%  distinct()%>% group_by(Abb) %>%
  summarise_at(colnames(Pred3), mean) 

DF.unit=data.frame(Bath_unit='m',Pop_unit='popoulo',Nit_unit='µg/ml',Sal_unit=NA,
                   Chloa_new.mean_unit='mg',SSTcold_unit='°C',SST.M_unit='°C',
                   WE_unit='truc',WKE_unit='machin',TAMP_unit='biloute')

# Préparation Bio
StJacu=df3 %>%
  mutate(N.couv = case_when(recouv %in% c("4","5") ~ "Forte couverture",
                            recouv %in% c("2","3") ~ "Moyenne couverture",
                            TRUE ~ "Faible couverture"))     %>% 
  mutate(Tot=bigorneau+ calliostome+gibbulecomm+gibbuleombi+lit.comp.saxa+
           littospp+monodonte+nasse+pourpre+patelle) 
# mutate(Site=rep('All',nrow(df3)))


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel(div(img(src = "planete-mer.jpg", height = 100, width = 100),
                 img(src = "mnhn.jpg", height = 100, width = 100),
                 h1( "Données Biolit visualisation"))),
  
  tabsetPanel(
    tabPanel(title = "Variables Environnementales",
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", h2("Choix du parametre"),
                             choices=list("Temp moy"='SST.M',
                                          'Sal'='Sal',
                                          "Nitrate"='Nit'), selected='Bat'),
                 selectInput("Sites", h2("Choisir les sites"),
                             choices=list("St-Jacut"=c("IDB","ILC"),
                                          "FON"="FON",
                                          "EVE"="EVE",
                                          "ADG"="ADG",
                                          "LI"="LI",
                                          "GC","GC",
                                          "HSE","HSE"), selected=NULL, multiple=T)),
               
               mainPanel( plotOutput(outputId = "ParamPlot")))),
    
    # 
    # tabPanel(title="Données biologiques",
    #          sidebarLayout(
    #              sidebarPanel(
    #  radioButtons('Var_resp', h3('Selection de la variabiable Bio'),
    #                              choices=list('Abondance Totale'='Tot',
    #                                           'Richesse Specifique'='S')),
    #  selectInput("Sites_Ab", h2("Choisir les sites"),
    #              choices=list("St-Jacut"=c("IDB","ILC"),
    #                           "FON"="FON",
    #                           "EVE"="EVE",
    #                           "ADG"="ADG",
    #                           "LI"="LI",
    #                           "GC","GC",
    #                           "HSE","HSE"), selected='SEN', multiple=T)),
    #  
    #  mainPanel(plotOutput(outputId = "bioPlot"))
    #          )),
    
    tabPanel(title="Sur l'estran...",
             fluidRow(
               column(3,
                      selectInput("sel_spe", h4("Choisir le/les espèces"),
                                  choices=list("Monodonte (P.lineatus)"="monodonte",
                                               "Gibbule ombiliquée (S.umbilicalis)"="gibbuleombi",
                                               "Gibbule commune (S.pennanti)"="gibbulecomm",
                                               "Patelle (Patella spp.)"="patelle",
                                               "Littorines des rochers (Littorina saxatilis)"="lit.comp.saxa",
                                               "calliostome (Calliostoma zizyphinum)"="calliostome"),
                                  selected=NULL, multiple=T)),
               column( 8,
                       plotOutput(outputId="Estran_plot"))
               
               # column(4,
               #        img(src ="estran_zonation.png", height = 400, width = 250))
             ))
    # sidebarLayout(
    #             sidebarPanel(radioButtons('Var_resp', h3('Selection de la variabiable Bio'),
    #                                      choices=list('Abondance Totale'='Tot',
    #                              'Richesse Specifique'='S'))),
    #             mainPanel( plotOutput(outputId = "ParamPlot")))
    #         
  ))
