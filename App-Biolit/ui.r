############## UI scriptlibrary(shiny)




# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel(div(img(src = "logo BioLit.jpg", height = 80, width = 150),
                 img(src = "planete-mer.jpg", height = 100, width = 100),
                 img(src = "mnhn.jpg", height = 100, width = 100),
                 h1( "Données Biolit visualisation"))),
  
  tabsetPanel(
    tabPanel(title = "Variables Environnementales",
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", h2("Choix du parametre"),
                             choices=list("Temp moy"='SST',
                                          "Temp du mois le plus froid"="SSTco",
                                          "Matière en suspension"="TSM",
                                          'Sal'='Sal',
                                          "Nitrate"='Nit'), selected='Bat'),
                 selectInput("Sites", h2("Choisir les sites"),
                             choices=list("St-Jacut"=c("IDB","ILC"),
                                          "FON"="FON",
                                          "EVE"="EVE",
                                          "ADG"="ADG",
                                          "LI"="LI",
                                          "GC","GC",
                                          "PDF"="PDF",
                                          "HSE","HSE"), selected=NULL, multiple=T)),
                 # img(src='Carte_panel1.png', align = "left")),
               
               mainPanel( plotOutput(outputId = "ParamPlot"),
                          plotOutput(outputId = "RadarPlot"),
                          plotOutput(outputId = "Site_identity"),
                          
               ))),
    
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
             )),
    tabPanel(title = "Composition taxonomique",
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 radioButtons('data_type', h3('Selection du paramètre de composition:'),
                              choices=list('Abondances'='value',
                                           'Probabilité d occurrence'='PA')),
                 selectInput("Site_compo", h2("Choisir les sites"),
                             choices=list("St-Jacut"=c("IDB","ILC"),
                                          "FON"="FON",
                                          "EVE"="EVE",
                                          "ADG"="ADG",
                                          "LI"="LI",
                                          "GC","GC",
                                          "HSE","HSE"), selected=NULL, multiple=F)),
               
               mainPanel(plotOutput(outputId = "Rank_species"),
                         p(h1("Description: blablabla?")))),
             )
    # sidebarLayout(
    #             sidebarPanel(radioButtons('Var_resp', h3('Selection de la variabiable Bio'),
    #                                      choices=list('Abondance Totale'='Tot',
    #                              'Richesse Specifique'='S'))),
    #             mainPanel( plotOutput(outputId = "ParamPlot")))
    #         
  ))
