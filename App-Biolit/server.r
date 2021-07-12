######  server 


server2 <- function(input, output) {
  
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
      summarize(minX = min(x),
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
  # Plot 2
  output$bioPlot <- renderPlot({
    
    # mutate(Site=case_when(Abb %in% input$Sites_Ab ~ "Stations", TRUE ~ "Autres"))
    
    Quant=as.vector(quantile(StJacu[,input$Var_resp] , probs = c(.25,.5,.75)))
    
    Sites= StJacu  %>% filter(Abb %in% input$Sites_Ab)
    StJacu=StJacu %>% mutate(Site=rep('All',nrow(StJacu)))
    Sites=Sites %>% mutate(Site=rep('Sites select',nrow(Sites)))
    
    nDF=rbind(StJacu,Sites)
    
    ggplot(data=nDF,aes_string('Site',y=input$Var_resp))+
      # geom_jitter(size=1, shape=21)+
      geom_violin(aes(fill=Site))+
      geom_boxplot(alpha=.8, width=.1)+
      # geom_split_violin(alpha=.8)+
      # geom_hline(yintercept=Quant,linetype=2, size=.9)+
      # # geom_split_violin()
      # # geom_violin(aes(fill=substrat3), alpha=.2)+ 
      # # # geom_jitter(data=subset(StJacu, Abb %in%   c("IDB","ILC")), col='green')+
      # # geom_boxplot(data=subset(StJacu, Abb %in%   c("IDB","ILC")),width=.2)+
      # scale_y_log10()+
      facet_grid(substrat3~N.couv)+
      theme_light()+
      xlab('')
  })
  
  # Plot 3
  output$Estran_plot <- renderPlot({
    
    df3$substrat3 = with(df3, factor(substrat3, levels =
                                       c('Pel','Fspir','Fvesi','Anodo','Fser')))
    
    DF_bio=melt(df3, id.var=c('Abb','AAAA','substrat3','date','longit', 'lat', 'recouv'), var='Spe')
    # DF_bio=dcast(Abb + longit+lat+ date+ recouv + substrat3 + AAAA ~ Spe, data=df3, value.var='ab')
    DF_bio=DF_bio %>% group_by(Spe) %>% mutate(Val=scale(value))
    
    
    req(input$sel_spe) # Requiert une valeur de sites
    DF_bio2=DF_bio %>% filter(Spe %in% input$sel_spe)
    
    p1=ggplot(data=DF_bio2,aes(recouv,Val, col=Spe))+
      # geom_jitter()+
      geom_smooth(aes(group=Spe), method='loess')+
      theme_minimal()+
      ylab('Fréquence des abondances (%)')+
      xlab('Classe de recouvrement')
    
    p2=ggplot(data=DF_bio2,aes(substrat3,as.numeric(Val), col=Spe))+
      # geom_jitter()+
      stat_summary( aes(group=Spe, col=Spe),fun.y=mean, geom="line",size=1.5)+
      # scale_y_log10()+
      stat_summary( fun.data="mean_cl_normal",size=2)+
      theme_minimal()+
      ylab('Fréquence des abondances (%)')+
      xlab('Ceinture algues brunes')
    
    ggarrange(p1,p2, ncol=1, common.legend=T)# mutate(Site=case_when(Abb %in% input$Sites_Ab ~ "Stations", TRUE ~ "Autres"))
  })
}

shinyApp(ui = ui, server = server2)
