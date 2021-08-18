############## UI scriptlibrary(shiny)




# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel(div(img(src = "logo BioLit.jpg", height = 80, width = 150),
                 img(src = "planete-mer.jpg", height = 100, width = 100),
                 img(src = "mnhn.jpg", height = 100, width = 100),
                 h1( strong("ABB explorer:"),span(em(" Visualisation des données du programme 'Algues Brunes et Bigorneaux'"))))),
  
  # Table présentation
  tabsetPanel(
    tabPanel(title = "Présentation",
             sidebarLayout(
               sidebarPanel( h2("Biolit: Les observateurs du littoral", align="center"),
                             div(img(src = "ABB_field.jpg", height = 120, width = 150),
                                 img(src = "ABB_field2.jpg", height = 120, width = 130),
                                 img(src = "ABB_field3.jpg", height = 120, width = 160),
                                 img(src = "ABB_field4.jpg", height = 120, width = 100)),
                          strong('BioLit est un programme national de Sciences Participatives sur la biodiversité du littoral. 
                Le programme BioLit a été créé et est piloté par l’association Planète Mer 
              en collaboration avec la station du Muséum National d’Histoire Naturelle du CRESCO de Dinard.'),
                        ),
    
    mainPanel(  h2("Le programme 'Algues Brunes et Bigorneaux'", align="center"),
                br(),
                p('Situées dans la zone de balancement des marées, les habitats formés par les algues brunes associés aux estrans rocheux
                sont depuis 
                longtemps reconnus comme abritant une importante biodiversité. En plus de fournir de la nourriture et un abri
                pour se cacher, ces grandes algues participent au maintien d’un taux d’humidité important permettant ainsi aux
                espèces animales marines résidentes de se maintenir à l’air libre lors de la marée basse, sans se dessécher.' ),
                br(),
                p("Le programme 'ABB' a pour objectif d'améliorer les connaissances de ces habitats riches mais également sensibles
                aux altérations de l'environnement en 
                  réalisant depuis 2012 un suivi conjoint de l'état des canopées d'algues 
                  et de l'abondance de ces habitants sur la façade Manche-Atlantique"),
                br(),
                
                strong('"Algues Brunes et Bigorneaux en quelques chiffres :"'),
                br(),
                tags$ul(
                tags$li(em('2652 observations réalisées entre 2012 et 2020 qui ont permis d’améliorer la connaissance sur le fonctionnement et l’évolution des écosystèmes intertidaux rocheux
66 sites échantillonnés de la Manche jusqu’à l’Atlantique Nord')),
                tags$li(em('19 taxons de mollusques identifiés à travers 6 ceintures algales de l’étage médiolittoral')),
                tags$li(em('34 structures relais ont participé à la création de ce jeu de donnée '))),
                br(),
                h3(strong('Description des différents panneaux')),
                br(),
                h4("Panneaux 1: L'estran et son environnement"),
                em('Les estrans rocheux de la façade Manche-Atlantique sont soumis à des conditions environnementales
                bien différentes d’un estran à un autre. Cette variabilité environnementale conditionne
                  le développement des espèces côtières et agit sur la répartition des êtres vivants des littoraux marins.'),
                br(),
                p('Ce panneau permet de visualiser les conditions environnementales moyennes présentes au sein des estrans
                  échantillonnées dans le cadre du programme Algues Brunes et Bigorneaux. En séléctionnant un/des sites, 
                  il est alors possible de positionner ses caractéristiques par rapport à l’ensemble des sites du jeu de données.
                  La figure présente la distribution des valeurs moyennes retrouvées au sein des 69 estrans échantillonnées,
                  pour chaque paramètre environnemental s’exerçant sur un/des sites.'),
                br(),
                h4("Panneaux 2: L’estran et ses habitants"),
                em('Les données collectées ont permis d’identifier 
                       la répartition des mollusques suivant les ceintures d’algues brunes. '),
                br(),
                p('En choisissant une espèce de mollusque, il est possible :'),
                tags$ul(
                  tags$li('Visualiser son affinité avec le niveau de couverture du champ d’algue (cf. figure 2)'),
                  tags$li('Visualiser son affinité avec le type de ceinture algale décrivant
                          la zonation verticale de l’estran (cf. figure 3)')),
                br(),
                h4("Panneaux 3: D’un estran à l’autre, différentes compositions"),
                em('Les données collectées par les BioLitiens permettent de visualiser la composition des communautés 
                   de mollusque dans les ceintures d’algues, au sein des différents sites échantillonnés.'),
                br(),
                p('Ces figures permettent de comparer verticalement entre un site choisi et l’ensemble des sites :'),
                tags$ul(
                  tags$li('1-	L’abondance de chaque mollusque dans une couverture algale 
                          c’est-à-dire le nombre d’individus observés (cf. figure 4)'),
                  tags$li('2-	L’occurrence de chaque mollusque dans une couverture algale c’est-à-dire
                  la probabilité de rencontre d’un mollusque dans cette couverture algale (cf. figure 5)
Pour chaque site, les espèces rencontrées sont classées de la plus abondante/occurrence (rang1) au sein 
de chaque ceinture d’algue inventoriée, à la plus rare. Pour chaque ceinture d’algue, le nombre de quadrats
utilisé (n) a également été indiqué.'))


                
))),
  
  
  
  
  
  

    tabPanel(title = "Variables Environnementales",
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 # selectInput("variable", h2("Choix du parametre"),
                 #             choices=list("Temp moy"='SST',
                 #                          "Temp du mois le plus froid"="SSTco",
                 #                          "Matière en suspension"="TSM",
                 #                          'Sal'='Sal',
                 #                          "Nitrate"='Nit'), selected='Bat'),
                 selectInput("Sites", h2("Choisir les sites"),
                             choices=list("St-Jacut"=c("IDB","ILC"),
                                          "FON"="FON",
                                          "EVE"="EVE",
                                          "ADG"="ADG",
                                          "LI"="LI",
                                          "GC","GC",
                                          "PDF"="PDF",
                                          "HSE","HSE"), selected=NULL, multiple=T),
                 # img(src='Carte_panel1.png', align = "left")),
               h2("Situation géographique du site"),
                 plotOutput(outputId = "Site_identity")),
               
               mainPanel( 
                 # plotOutput(outputId = "ParamPlot"),
                          plotOutput(outputId = "RadarPlot"),
                          # plotOutput(outputId = "Site_identity"),
                          
               ))),
    
    tabPanel(title="Sur l'estran...",
             sidebarLayout(
               sidebarPanel(
                      selectInput("sel_spe", h4("Choisir le/les espèces"),
                                  choices=list("Monodonte (P.lineatus)"="monodonte",
                                               "Gibbule ombiliquée (S.umbilicalis)"="gibbuleombi",
                                               "Gibbule commune (S.pennanti)"="gibbulecomm",
                                               "Patelle (Patella spp.)"="patelle",
                                               "Littorines des rochers (Littorina saxatilis)"="lit.comp.saxa",
                                               "calliostome (Calliostoma zizyphinum)"="calliostome"),
                                  selected=NULL, multiple=T)),
               mainPanel(plotOutput(outputId="Estran_plot"),
           DT::dataTableOutput('table_spe')),
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
                                          "HSE","HSE"), selected=NULL, multiple=F),
                 h2("Situation géographique du site"),
                 plotOutput(outputId = "Site_identity_compo")),
               
               mainPanel(plotOutput(outputId = "Rank_species"),
                         p(h1("Description: blablabla?")))),
             ),
    
    tabPanel(title = "Bioindication et intégrité ecologique",
           
               
              p(h1("En construction...",align = "center")),
                   img(src = "chantier.jpg", height = 300, width = 500) ,
             
             # Sidebar with a slider input for number of bins 
            
    )
    # sidebarLayout(
    #             sidebarPanel(radioButtons('Var_resp', h3('Selection de la variabiable Bio'),
    #                                      choices=list('Abondance Totale'='Tot',
    #                              'Richesse Specifique'='S'))),
    #             mainPanel( plotOutput(outputId = "ParamPlot")))
    #         
  ))
