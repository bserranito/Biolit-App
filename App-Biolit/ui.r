


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
                                 img(src = "ABB_field2.jpg", height = 100, width = 120),
                                 img(src = "ABB_field3.jpg", height = 120, width = 160),
                                 img(src = "ABB_field4.jpg", height = 120, width = 100)),
                          strong('BioLit est un programme national de Sciences Participatives sur la biodiversité du littoral. 
                Le programme BioLit a été créé et est piloté par l’association Planète Mer 
              en collaboration avec la station du Muséum National d’Histoire Naturelle du CRESCO de Dinard.'),
                        ),
    
    mainPanel(h2("Le programme 'Algues Brunes et Bigorneaux'", align="center"),
                br(),
                p('Situées dans la zone de balancement des marées, les habitats formés par les algues brunes associés aux estrans rocheux
                sont depuis 
                longtemps reconnus comme abritant une importante biodiversité. En plus de fournir de la nourriture et un abri
                pour se cacher, ces grandes algues participent au maintien d’un taux d’humidité important permettant ainsi aux
                espèces animales marines résidentes de se maintenir à l’air libre lors de la marée basse, sans se dessécher.' ),
                br(),
                p("Le programme 'ABB' a pour objectif d'améliorer les connaissances de ces habitats riches mais également sensibles
                aux altérations de l'environnement en permettant aux 'Biolitiens' de
                  réaliser depuis 2012 un suivi conjoint de l'état des canopées d'algues 
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
                
                h4(strong("l'application ",span(em(strong("ABB explorer"))),span(strong("a pour enjeux de permettre à tous les bioliciens 
                 de pouvoir visualiser sous différents aspects les données receuillies et des récents résultats obtenus dans le cadre du programme ABB,
                afin de permettre à chacun de mieux comprendre le fonctionnement et la structure de ces habitats si particuliers.")))),
                br(),
                
                leaflet::leafletOutput(outputId="interactive_map"),
                
                
                h3(strong('Description des différents panneaux')),
                br(),
                h4("Panneau 1: L'estran et son environnement"),
                em('Les estrans rocheux de la façade Manche-Atlantique sont soumis à des conditions environnementales
                bien différentes d’un estran à un autre. Cette variabilité environnementale conditionne
                  le développement des espèces côtières et agit sur la répartition des êtres vivants des littoraux marins.'),
                br(),
                p('Ce panneau permet de visualiser les conditions environnementales moyennes présentes au sein des estrans
                  échantillonnées dans le cadre du programme Algues Brunes et Bigorneaux. En séléctionnant un/des sites, 
                  il est alors possible de positionner ses caractéristiques par rapport à l’ensemble des sites du jeu de données.
                  La figure présente la distribution des valeurs moyennes retrouvées au sein des 66 estrans échantillonnées,
                  pour chaque paramètre environnemental s’exerçant sur un/des sites.'),
                br(),
                h4("Panneau 2: L’estran et ses habitants"),
                em('Les données collectées ont permis d’identifier 
                       la répartition des mollusques suivant les ceintures d’algues brunes. '),
                br(),
                p('En choisissant une espèce de mollusque, il est possible :'),
                tags$ul(
                  tags$li('Visualiser son affinité avec le niveau de couverture du champ d’algue (cf. figure 2)'),
                  tags$li('Visualiser son affinité avec le type de ceinture algale décrivant
                          la zonation verticale de l’estran (cf. figure 3)')),
                br(),
                h4("Panneau 3: D’un estran à l’autre, différentes compositions"),
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
  
  
  
  
  
  

    tabPanel(title = "Profil Environnemental",
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 # selectInput("variable", h2("Choix du parametre"),
                 #             choices=list("Temp moy"='SST',
                 #                          "Temp du mois le plus froid"="SSTco",
                 #                          "Matière en suspension"="TSM",
                 #                          'Sal'='Sal',
                 #                          "Nitrate"='Nit'), selected='Bat'),
                 selectInput("Sites", h3("Choisir les sites"),
                             choices=list("Manche"=c("Ste Marguerite sur mer"="SMM",
                                                     "Bas Fort Blanc"="BFB",
                                                     "Quiberville"="QUI",
                                                     "Petit Ailly"="PEA",
                                                     "Pointe de la Loge"="PDL",
                                                     "Tatihou"="TAT",
                                                     "Blainville-sur-mer"="BSM",
                                                     "Bretteville"="BRE",
                                                     "Cale"="CAL",
                                                     "Pere Gus"="PEG"),
                                          "Bretagne NE"=c("Chausey-Grande Corbière"="GC",
                                                          "Chausey-Port Marie"="PM",
                                                          "L'Eventail"="EVE",
                                                          "Chausey-Houston SE"="HSE",
                                                          "Chausey-Longue Ile"="LI",
                                                          "Anse du Guesclin"="ADG",
                                                          "Fort National"="FON",
                                                          "Saint-Enogat"="SEN",
                                                          "Grand Bé"="GRB",
                                                          "Dame Jouanne"="DJO",
                                                          "Cap Fréhel"="CFR",
                                                          "Ilot st Michel"="ISM",
                                                          "Ilot de la Colombière"="ILC",
                                                          "La Piscine"="PIS",
                                                          "Piegu"="PIE",
                                                          "L'Islet"="ISL"),
                                          "Bretagne NO"=c("Pointe de l'Arcouest"="PDA",
                                                          "Pors-Ar-Goret"="PAG",
                                                          "La Bastille"="BAS",
                                                          "St Guirec/Bastille"="SGB",
                                                          "Kigner Bell"="KIB",
                                                          "Ile Verte"="IVE",
                                                          "Dellec"="DEL"),
                                          "Bretagne S"=c("Cabellou"="CAB",
                                                         "Glénan (St Nicolas)"="GLE",
                                                         "Trenez"="TRE",
                                                         "Port Navalo"="PNA",
                                                         "Kermorvan"="KER",
                                                         "Kercouedo"="KCD",
                                                         "Ile de Boed"="IBO",
                                                         "Fogeo"="FOG",
                                                         "Roche Vilaine"="ROV",
                                                         "Ile de Bailleron"="ILB",
                                                         "Cale de Gornevez"="CDG",
                                                         "Penvins"="PEN",
                                                         "Île Dumet"="IDU",
                                                         "St Goustan"="SGO",
                                                         "Baie des Marsouins"="BDM",
                                                         "La Tara"="TAR",
                                                         "Roche Percee"="ROP"),
                                          "Charente"=c("Loix"="LOI",
                                                       "Chef de Baie"="CDB",
                                                       "Vert Clos"="VEC",
                                                       "Les Minimes"="MIN",
                                                       "Pointe des Prouards"="PDP",
                                                       "Aytre"="AYT",
                                                       "Pointe du Chay"="PDC",
                                                       "Pont de la Chaume"="POC",
                                                       "Les boucholeurs"="BOU",
                                                       "Saint Jean-des-sables"="SJS",
                                                       "Pointe de la Fumée"="PDF",
                                                       "Platin"="PLA",
                                                       "Ile aux Mouettes"="ILM",
                                                       "Concheau de Suzac"="CDS")
                                  ), selected=NULL, multiple=T),
                 # img(src='Carte_panel1.png', align = "left")),
               h3("Situation géographique du site"),
               em('Cette carte intéractive vous permet de situer les stations sur la façade Manche-Atlantique'),
                 plotOutput(outputId = "Site_identity")),
               
               mainPanel( h4("Panneau 1: L'estran et son environnement", align="center"),
                          p(em("Cette figure permet de visualiser et de comparer les profils environnementaux des estrans échantillonnés.
                           En selectionant un site, le 'radar plot' précisera les valeurs moyennes de 11 variables environnementales structurant les milieux côtiers:")),
                            br(),
                            strong(em("Bath: bathymetrie moyenne du site, WKE: Energie cinétique liée à la houle, 
                                             WE: exposition aux vagues, TSM: matière en suspension,Coef Mar: amplitude des marée,
                                              SST: Température moyenne de surface des eaux, SSTco: Température de surface du moins le plus foid, 
                                              Sal: Salinité moyenne, Indice pop: Indice de densité de population, Nit: Concentrations en nitrite/nitrate des eaux,
                                              Chloa: Concentrations en chlorophyle a")),
                            br(),
                            p(em("(L'ensemble des sources relatives aux données environnementales sont compilées au sein de Serranito et al., 2021)")),
                 # plotOutput(outputId = "ParamPlot"),
                          plotOutput(outputId = "RadarPlot"),
                          # plotOutput(outputId = "Site_identity"),
                          
               ))),
    
    tabPanel(title="L'estran et ses habitants",
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
               mainPanel(h4("Panneau 2: L’estran et ses habitants", align='center'),
                 plotOutput(outputId="Estran_plot"),
           DT::dataTableOutput('table_spe')),
               # column(4,
               #        img(src ="estran_zonation.png", height = 400, width = 250))
             )),
    tabPanel(title = "D'un estran à l'autre...",
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 radioButtons('data_type', h3('Selection du paramètre de composition:'),
                              choices=list('Abondances'='value',
                                           'Probabilité d occurrence'='PA')),
                 selectInput("Site_compo", h2("Choisir le site"),
                               choices=list("Manche"=c("Ste Marguerite sur mer"="SMM",
                                                       "Bas Fort Blanc"="BFB",
                                                       "Quiberville"="QUI",
                                                       "Petit Ailly"="PEA",
                                                       "Pointe de la Loge"="PDL",
                                                       "Tatihou"="TAT",
                                                       "Blainville-sur-mer"="BSM",
                                                       "Bretteville"="BRE",
                                                       "Cale"="CAL",
                                                       "Pere Gus"="PEG"),
                                            "Bretagne NE"=c("Chausey-Grande Corbière"="GC",
                                                            "Chausey-Port Marie"="PM",
                                                            "L'Eventail"="EVE",
                                                            "Chausey-Houston SE"="HSE",
                                                            "Chausey-Longue Ile"="LI",
                                                            "Anse du Guesclin"="ADG",
                                                            "Fort National"="FON",
                                                            "Saint-Enogat"="SEN",
                                                            "Grand Bé"="GRB",
                                                            "Dame Jouanne"="DJO",
                                                            "Cap Fréhel"="CFR",
                                                            "Ilot st Michel"="ISM",
                                                            "Ilot de la Colombière"="ILC",
                                                            "La Piscine"="PIS",
                                                            "Piegu"="PIE",
                                                            "L'Islet"="ISL"),
                                            "Bretagne NO"=c("Pointe de l'Arcouest"="PDA",
                                                            "Pors-Ar-Goret"="PAG",
                                                            "La Bastille"="BAS",
                                                            "St Guirec/Bastille"="SGB",
                                                            "Kigner Bell"="KIB",
                                                            "Ile Verte"="IVE",
                                                            "Dellec"="DEL"),
                                            "Bretagne S"=c("Cabellou"="CAB",
                                                           "Glénan (St Nicolas)"="GLE",
                                                           "Trenez"="TRE",
                                                           "Port Navalo"="PNA",
                                                           "Kermorvan"="KER",
                                                           "Kercouedo"="KCD",
                                                           "Ile de Boed"="IBO",
                                                           "Fogeo"="FOG",
                                                           "Roche Vilaine"="ROV",
                                                           "Ile de Bailleron"="ILB",
                                                           "Cale de Gornevez"="CDG",
                                                           "Penvins"="PEN",
                                                           "Île Dumet"="IDU",
                                                           "St Goustan"="SGO",
                                                           "Baie des Marsouins"="BDM",
                                                           "La Tara"="TAR",
                                                           "Roche Percee"="ROP"),
                                            "Charente"=c("Loix"="LOI",
                                                         "Chef de Baie"="CDB",
                                                         "Vert Clos"="VEC",
                                                         "Les Minimes"="MIN",
                                                         "Pointe des Prouards"="PDP",
                                                         "Aytre"="AYT",
                                                         "Pointe du Chay"="PDC",
                                                         "Pont de la Chaume"="POC",
                                                         "Les boucholeurs"="BOU",
                                                         "Saint Jean-des-sables"="SJS",
                                                         "Pointe de la Fumée"="PDF",
                                                         "Platin"="PLA",
                                                         "Ile aux Mouettes"="ILM",
                                                         "Concheau de Suzac"="CDS")
                               ), selected=NULL, multiple=F),
                 h2("Situation géographique du site"),
                 em('Cette carte intéractive vous permet de situer les stations sur la façade Manche-Atlantique'),
                 plotOutput(outputId = "Site_identity_compo")),
               
               mainPanel(
                 h4("Panneau 3: D’un estran à l’autre, différentes compositions", align="center"),
                 em("Pel: Pelvetie, Fspir: Fucus spiralis, Fvesi: Fucus vesiculosus,
                 Anodo:Ascophyllum nodosum, Fser: Fucus spiralis"),
                 
                 plotOutput(
                   outputId = "Rank_species"),
                         )),
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
