#
#list of packages required
list.of.packages <- c("shiny","dplyr","ggplot2")
# ################# les Packages
library(shiny)
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
                    
                    mainPanel(plotOutput(outputId = "Rank_species")))),
# sidebarLayout(
#             sidebarPanel(radioButtons('Var_resp', h3('Selection de la variabiable Bio'),
#                                      choices=list('Abondance Totale'='Tot',
#                              'Richesse Specifique'='S'))),
#             mainPanel( plotOutput(outputId = "ParamPlot")))
#         
    ))
    
 # fluidRow(
 #        column(2,
 #            selectInput("variable", h3("Choix du parametre"),
 #                        choices=list("Temp moy"='SST.M',
 #                                     'Sal'='Sal',
 #                                     "Nitrate"='Nit'), selected='Bat')),
 #           column(3,
 #                  selectInput("Sites", h3("Choisir les sites"),
 #                                        choices=list("St-Jacut"=c("IDB","ILC"),
 #                                                     "FON"="FON",
 #                                                      "EVE"="EVE",
 #                                                     "ADG"="ADG",
 #                                                     "LI"="LI",
 #                                                     "GC","GC",
 #                                                     "HSE","HSE"), selected='SEN', multiple=T)),
 #      
        # Show a plot of the generated distribution
#         mainPanel( plotOutput(outputId = "ParamPlot"))
#     ),
#     #Radio Button
#     # fluidRow(column(2,
#     #                 radioButtons('Var_resp', h3('Selection de la variabiable Bio'),
#     #                 choices=list('Abondance Totale'='Tot',
#     #                              'Richesse Specifique'='S'))
#              )
# 
#     )
# )

# 
# server2 <- function(input, output) {
#     
#     output$ParamPlot <- renderPlot({
#         # Selectionne du parametre
#         # k=Selection
#         Var=unlist(DF[,input$variable])
#         
#         req(input$Sites) # Requiert une valeur de sites pour afficher la suite
#         
#          Sel.var=  as.data.frame(as.matrix(DF[which(DF$Abb %in% input$Sites),c('Abb',input$variable)])) %>%
#              rename(Val =input$variable) %>% as.data.frame()#
#          
#         
#         unit=get(paste0(input$variable,'_unit'),DF.unit) # Récupération de l'unité de la variable selectionnée
#         
#         
#         # Sel.var=DF %>% filter(Abb == 'SEN') %>% select(Abb,Nit)  %>% as.data.frame()
#         probs <- c(.25,.5, 0.75)
#         dens <- density(Var)
#         df.quant=data.frame(x=dens$x, y=dens$y)
#         quantiles <- quantile(Var, prob=probs)
#         df.quant$quant <- factor(findInterval(dens$x,quantiles))
#         # 
#         # 
#         df.test=df.quant %>%  group_by(quant) %>%
#             summarize(minX = min(x),
#                       maxX = max(x))
#         
#         ggplot()+
#             geom_rect(data=df.test,aes(xmin=minX, xmax=maxX, ymin=-.5,ymax=.5 , fill=quant))+
#             scale_fill_brewer(direction=1,name = "Distribution \n des valeurs", labels = c("0-25%", "25-50%", "50-75%","75-100%"))+
#             geom_hline(yintercept=-0.52,size=1.5)+
# 
#             geom_segment(data=Sel.var,aes(x=as.numeric(as.character(Val)),
#                                           xend=as.numeric(as.character(Val)),
#                                           y=0, yend=-.5, col=Abb),
#                          arrow = arrow(length = unit(0.5, "cm")),
#                          size=2,
#                          lineend = c('round'),
#                          linejoin = c('mitre'))+ # Vecteur déterminant la valeur de la station
#             guides(col=guide_legend(title="Site"))+
#             # scale_fill_discrete(name = "Distribution", labels = c("0-25%", "25-50%", "50-75%"))+
#             theme(panel.grid.major = element_blank(),
#                   panel.grid.minor = element_blank(),
#                   panel.border = element_blank(),
#                   panel.background = element_blank(),
#                   axis.ticks.length=unit(0.2,"inch"),
#                   axis.ticks.y=element_blank(),
#                   axis.text.x=element_text(size=12),
#                   axis.text.y=element_blank())+
#             ylab('')+ xlab(paste0('gradient de la variable (',unit,')'))
# 
#         
#         
#         
#         # ggplot()+
#         #     # geom_density(data=DATA,aes(Inor), alpha=.4)+
#         #     geom_line(data=df.quant,aes(x,y), size=1)+
#         #     geom_ribbon(data=df.quant,aes(x=x,ymin=0, ymax=y, fill=quant))+
#         #     # scale_x_continuous(breaks=quantiles) + 
#         #     # scale_fill_brewer(guide="none", palette="Spectral", direction=-1)+
#         #     theme_light()+
#         #      geom_vline(data=Sel.var, aes(xintercept=Sel.var[,2], col=Abb), linetype=2, size=1.5)+
#         #     scale_fill_brewer(direction=-1)+
#         #     guides(fill=F)+
#         #     # geom_vline(xintercept=(DF.tar$SST.M), linetype=2, size=1, col='red')+
#         #     # scale_fill_brewer(direction=-1)+
#         #     ylab('')+
#         #     xlab('')
#         })
#     # Plot 2
#      output$bioPlot <- renderPlot({
#          
#         # mutate(Site=case_when(Abb %in% input$Sites_Ab ~ "Stations", TRUE ~ "Autres"))
#          
#          Quant=as.vector(quantile(df3[,input$Var_resp] , probs = c(.25,.5,.75)))
#          
#          Sites= StJacu  %>% filter(Abb %in% input$Sites_Ab)
#          StJacu=StJacu %>% mutate(Site=rep('All',nrow(StJacu)))
#          Sites=Sites %>% mutate(Site=rep('Sites select',nrow(Sites)))
#          
#          nDF=rbind(StJacu,Sites)
#          
#          ggplot(data=nDF,aes_string('Site',y=input$Var_resp))+
#              # geom_jitter(size=1, shape=21)+
#              geom_violin(aes(fill=Site))+
#              geom_boxplot(alpha=.8, width=.1)+
#               # geom_split_violin(alpha=.8)+
#              # geom_hline(yintercept=Quant,linetype=2, size=.9)+
#              # # geom_split_violin()
#              # # geom_violin(aes(fill=substrat3), alpha=.2)+ 
#              # # # geom_jitter(data=subset(StJacu, Abb %in%   c("IDB","ILC")), col='green')+
#              # # geom_boxplot(data=subset(StJacu, Abb %in%   c("IDB","ILC")),width=.2)+
#               # scale_y_log10()+
#              facet_grid(substrat3~N.couv)+
#              theme_light()+
#              xlab('')
#     })
#      
#      # Plot 3
#      output$Estran_plot <- renderPlot({
#          
#          df3$substrat3 = with(df3, factor(substrat3, levels =
#                                               c('Pel','Fspir','Fvesi','Anodo','Fser')))
#          
#          DF_bio=melt(df3, id.var=c('Abb','AAAA','substrat3','date','longit', 'lat', 'recouv'), var='Spe')
#          # DF_bio=dcast(Abb + longit+lat+ date+ recouv + substrat3 + AAAA ~ Spe, data=df3, value.var='ab')
#          DF_bio=DF_bio %>% group_by(Spe) %>% mutate(Val=scale(value))
#          
#          
#          req(input$sel_spe) # Requiert une valeur de sites
#          DF_bio2=DF_bio %>% filter(Spe %in% input$sel_spe)
#          
#          p1=ggplot(data=DF_bio2,aes(recouv,Val, col=Spe))+
#              # geom_jitter()+
#              geom_smooth(aes(group=Spe), method='loess')+
#              theme_minimal()+
#              ylab('Fréquence des abondances (%)')+
#              xlab('Classe de recouvrement')
#          
#          p2=ggplot(data=DF_bio2,aes(substrat3,as.numeric(Val), col=Spe))+
#              # geom_jitter()+
#              stat_summary( aes(group=Spe, col=Spe),fun.y=mean, geom="line",size=1.5)+
#              # scale_y_log10()+
#              stat_summary( fun.data="mean_cl_normal",size=2)+
#              theme_minimal()+
#              ylab('Fréquence des abondances (%)')+
#              xlab('Ceinture algues brunes')
#          
#          ggarrange(p1,p2, ncol=1, common.legend=T)# mutate(Site=case_when(Abb %in% input$Sites_Ab ~ "Stations", TRUE ~ "Autres"))
#     })
# }



server <- function(input, output) {
    
    output$ParamPlot <- renderPlot({
        # Selectionne du parametre
        # k=Selection
        Var=unlist(DF[,input$variable])
        
        req(input$Sites) # Requiert une valeur de sites pour afficher la suite
        
        Sel.var=  as.data.frame(as.matrix(DF[which(DF$Abb %in% input$Sites),c('Abb',input$variable)])) %>%
            rename(Val =input$variable) %>% as.data.frame()#
        
        
        unit=get(paste0(input$variable,'_unit'),DF.unit) # Récupération de l'unité de la variable selectionnée
        
        
        # Sel.var=DF %>% filter(Abb == 'SEN') %>% select(Abb,Nit)  %>% as.data.frame()
        probs <- c(.25,.5, 0.75)
        dens <- density(Var)
        df.quant=data.frame(x=dens$x, y=dens$y)
        quantiles <- quantile(Var, prob=probs)
        df.quant$quant <- factor(findInterval(dens$x,quantiles))
        # 
        # 
        df.test=df.quant %>%  group_by(quant) %>%
            dplyr::summarize(minX = min(x),
                      maxX = max(x))
        
        ggplot()+
            geom_rect(data=df.test,aes(xmin=minX, xmax=maxX, ymin=-.5,ymax=.5 , fill=quant))+
            scale_fill_brewer(direction=1,name = "Distribution \n des valeurs", labels = c("0-25%", "25-50%", "50-75%","75-100%"))+
            geom_hline(yintercept=-0.52,size=1.5)+
            
            geom_segment(data=Sel.var,aes(x=as.numeric(as.character(Val)),
                                          xend=as.numeric(as.character(Val)),
                                          y=0, yend=-.5, col=Abb),
                         arrow = arrow(length = unit(0.5, "cm")),
                         size=2,
                         lineend = c('round'),
                         linejoin = c('mitre'))+ # Vecteur déterminant la valeur de la station
            guides(col=guide_legend(title="Site"))+
            # scale_fill_discrete(name = "Distribution", labels = c("0-25%", "25-50%", "50-75%"))+
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.ticks.length=unit(0.2,"inch"),
                  axis.ticks.y=element_blank(),
                  axis.text.x=element_text(size=12),
                  axis.text.y=element_blank())+
            ylab('')+ xlab(paste0('gradient de la variable (',unit,')'))
        
        
        
        
        # ggplot()+
        #     # geom_density(data=DATA,aes(Inor), alpha=.4)+
        #     geom_line(data=df.quant,aes(x,y), size=1)+
        #     geom_ribbon(data=df.quant,aes(x=x,ymin=0, ymax=y, fill=quant))+
        #     # scale_x_continuous(breaks=quantiles) + 
        #     # scale_fill_brewer(guide="none", palette="Spectral", direction=-1)+
        #     theme_light()+
        #      geom_vline(data=Sel.var, aes(xintercept=Sel.var[,2], col=Abb), linetype=2, size=1.5)+
        #     scale_fill_brewer(direction=-1)+
        #     guides(fill=F)+
        #     # geom_vline(xintercept=(DF.tar$SST.M), linetype=2, size=1, col='red')+
        #     # scale_fill_brewer(direction=-1)+
        #     ylab('')+
        #     xlab('')
    })
    # # Plot 2
    # output$bioPlot <- renderPlot({
    #   
    #   # mutate(Site=case_when(Abb %in% input$Sites_Ab ~ "Stations", TRUE ~ "Autres"))
    #   
    #   Quant=as.vector(quantile(StJacu[,input$Var_resp] , probs = c(.25,.5,.75)))
    #   
    #   Sites= StJacu  %>% filter(Abb %in% input$Sites_Ab)
    #   StJacu=StJacu %>% mutate(Site=rep('All',nrow(StJacu)))
    #   Sites=Sites %>% mutate(Site=rep('Sites select',nrow(Sites)))
    #   
    #   nDF=rbind(StJacu,Sites)
    #   
    #   ggplot(data=nDF,aes_string('Site',y=input$Var_resp))+
    #     # geom_jitter(size=1, shape=21)+
    #     geom_violin(aes(fill=Site))+
    #     geom_boxplot(alpha=.8, width=.1)+
    #     # geom_split_violin(alpha=.8)+
    #     # geom_hline(yintercept=Quant,linetype=2, size=.9)+
    #     # # geom_split_violin()
    #     # # geom_violin(aes(fill=substrat3), alpha=.2)+ 
    #     # # # geom_jitter(data=subset(StJacu, Abb %in%   c("IDB","ILC")), col='green')+
    #     # # geom_boxplot(data=subset(StJacu, Abb %in%   c("IDB","ILC")),width=.2)+
    #     # scale_y_log10()+
    #     facet_grid(substrat3~N.couv)+
    #     theme_light()+
    #     xlab('')
    # })
    
    # Plot 3
    output$Estran_plot <- renderPlot({
        
        
        
        
        req(input$sel_spe) # Requiert une valeur de sites
        DF_bio2=DF_bio %>% filter(Spe %in% input$sel_spe)
        
        p1=ggplot(data=DF_bio2,aes(recouv,val.scaled, col=Spe))+
            # geom_jitter()+
            geom_smooth(aes(group=Spe), method='loess')+
            theme_minimal()+
            ylab('Fréquence des abondances (%)')+
            xlab('Classe de recouvrement')
        
        p2=ggplot(data=DF_bio2,aes(substrat3,as.numeric(val.scaled), col=Spe))+
            # geom_jitter()+
            stat_summary( aes(group=Spe, col=Spe),fun.y=mean, geom="line",size=1.5)+
            # scale_y_log10()+
            stat_summary( fun.data="mean_cl_normal",size=2)+
            theme_minimal()+
            ylab('Fréquence des abondances (%)')+
            xlab('Ceinture algues brunes')
        
        ggpubr::ggarrange(p1,p2, ncol=1, common.legend=T)# mutate(Site=case_when(Abb %in% input$Sites_Ab ~ "Stations", TRUE ~ "Autres"))
    })
    
    # plot 4
    output$Rank_species <- renderPlot({
        DF_bio_site= DF_bio %>% filter(Abb %in% input$Site_compo) %>%
            group_by(substrat3, Spe) %>%
            dplyr::summarize(count= n(),
                             M=mean(PA, na.rm=T),
                             SD=sd(PA, na.rm=T))%>%
            group_by(substrat3) %>%
            mutate(rank = dense_rank(-M))
        
        
        ggplot(DF_bio_site,aes(as.factor(rank), M))+
            geom_bar(stat='identity',aes(fill=Spe), col='black',  position = position_dodge(width = 1))+
            # geom_errorbar(aes(ymin=M-SD, ymax=M+SD), width=.2)+
            facet_grid(.~substrat3, scale="free_y")+
            # scale_y_log10()+
            guides(fill=guide_legend(title="Taxons"))+
            scale_fill_brewer(palette="Spectral")+
            ylab("Abondance moyenne(ind/m²)")+theme_light()+
            xlab('rang des taxons')
        
    })
}




# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }

# Run the application 
shinyApp(ui = ui, server = server)
