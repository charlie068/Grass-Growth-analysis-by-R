### Initialise ####
## set Folder
setwd("~/Teagasc/August2018")

##Set libraries
if (TRUE){
library(data.table)
library(readxl)
library(ggplot2)
library(plotrix)
library("writexl")
library("Hmisc") 
library("Rmisc")
library("scales")
library("multcompView")
library("officer")
library("magrittr")
library("reshape2")
library("rvg")
library("dplyr")
library("ggpubr")
library("moments")
library("rcompanion")
library("officer")
library("magrittr")
#library("rvg")
}

##Load excel File
growth<-read_excel("GROWTH MEASURMENTS020818.xlsx")
growth<-data.table(growth)
##melt the File and Format it in data.table
growth<-melt(growth, id=c('Variety', 'Date', 'Row', 'Chamber'))
names(growth)[6] <- "Height"
growth<-data.table(growth)

growth<-growth[Variety!="Bar"]

growth<-na.omit(growth)

### Growth Graph ####

#remove outliers
growth_out <- growth[,.SD[Height < (quantile(Height,0.75)+1.5*IQR(Height)) & Height > (quantile(Height,0.25)-1.5*IQR(Height))],by=c("Variety", "Chamber", "Date")]
sub_growth_L<-growth_out[Chamber=='L',]
sub_growth_M<-growth_out[Chamber=='M',]

##get average per time a per variety
growth_average<- as.data.table(growth_out[, .('Mean'=mean(Height, na.rm = TRUE),'Std'=std.error(Height, na.rm=TRUE)), by = list(Variety, Date, Chamber)])

########## end initialisation   ########################


##plot last time point Chmaber M
ggplot(growth_average[Variety!='Bar'&Chamber=="M"&as.character(Date)=="2018-06-15"],aes(Variety,Mean,fill=Variety)) + 
  geom_bar( stat = "summary", fun.y = "mean")+
  geom_errorbar(data=growth_average[Variety!='Bar'&Chamber=="M"&as.character(Date)=="2018-06-15"], mapping=aes(x=Variety, ymin=Mean+Std, ymax=Mean-Std))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Grass Height Average M") +
  xlab("Date") + ylab("Mean(Height)")

##plot last time point Chamber L
ggplot(growth_average[Variety!='Bar'&Chamber=="L"&as.character(Date)=="2018-06-15"],aes(Variety,Mean,fill=Variety)) + 
  geom_bar( stat = "summary", fun.y = "mean")+
  geom_errorbar(data=growth_average[Variety!='Bar'&Chamber=="L"&as.character(Date)=="2018-06-15"], mapping=aes(x=Variety, ymin=Mean+Std, ymax=Mean-Std))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Grass Height Average L") +
  xlab("Date") + ylab("Mean(Height)")



##plot means as bar graph
ggplot(data=growth_average[Variety!='Bar'], aes(x=Date, y=Mean), group=Chamber) + geom_line(aes(linetype=Chamber)) + geom_point() + 
  facet_grid(Chamber ~ Variety) +
  geom_errorbar(data=growth_average[Variety!='Bar'], mapping=aes(x=Date, ymin=Mean+Std, ymax=Mean-Std))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Grass Height Average") +
  xlab("Date") + ylab("Mean(Height)")

#Write excel file with growth average
#write_excel_csv(growth_average, 'growthAverage.csv')

##plot means as bar graph in one graph (two colours)
ggplot(data=growth_average[Variety!='Bar'&Chamber!='R'], aes(x=Date, y=Mean)) + geom_line(size=1,aes(colour=Chamber)) +
  geom_point(shape=1,size=2,aes(colour=Chamber)) + 
  facet_grid(. ~ Variety) +
  geom_errorbar(data=growth_average[Variety!='Bar'&Chamber!='R'], mapping=aes(x=Date, ymin=Mean+Std, ymax=Mean-Std, colour=Chamber))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Grass Height Average") +
  xlab("Date") + ylab("Mean(Height)")


### PLOTS Average per time ####

##plot means as line graph in one graph  "Grass Height Chamber M"
ggtile<-paste("Grass Height Chamber M")
growth_chM<-growth_average[Variety!='Bar'&Chamber=="M"]
ggplot(growth_chM, aes(x=Date, y=Mean)) + geom_line(size=1,aes(colour=Variety)) +
  geom_point(shape=1,size=2,aes(colour=Variety)) + 
  #facet_grid(. ~ Variety) +
  geom_errorbar(growth_chM, mapping=aes(x=Date, ymin=Mean+Std, ymax=Mean-Std, colour=Variety),width=0.2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(ggtile) +
  xlab("Date") + ylab("Mean(Height)")+
  theme_bw(base_size=20) +
  theme(plot.title = element_text(vjust=-0.6,hjust = 0.5, face="bold", size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits=c(0,35)) 

##plot means as bar graph in one graph (two colours)
ggtile<-paste("Grass Height Chamber L")
  growth_chM<-growth_average[Variety!='Bar'&Chamber=="L"]
  ggplot(growth_chM, aes(x=Date, y=Mean)) + geom_line(size=1,aes(colour=Variety)) +
  geom_point(shape=1,size=2,aes(colour=Variety)) + 
  #facet_grid(. ~ Variety) +
  geom_errorbar(growth_chM, mapping=aes(x=Date, ymin=Mean+Std, ymax=Mean-Std, colour=Variety),width=0.2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(ggtile) +
  xlab("Date") + ylab("Mean(Height)")+
  theme_bw(base_size=20) +
  theme(plot.title = element_text(vjust=-0.6,hjust = 0.5, face="bold", size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(limits=c(0,35)) 
  
##plot means as line graph (one colour)
ggplot(data=growth_average[Variety!='Bar'], aes(x=Date, y=Mean)) + geom_line(size=1, aes(linetype=Chamber)) +
  geom_point(shape=1,size=2) + 
  facet_grid(. ~ Variety) +
  geom_errorbar(data=growth_average[Variety!='Bar'], mapping=aes(x=Date, ymin=Mean+Std, ymax=Mean-Std))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Grass Height Average") +
  xlab("Date") + ylab("Mean(Height)")

### BOX plot ####
##plot means as boxplot graph (facet colour)                               fffffffff
subsub_growth<-growth[Variety!='Bar'&Date<="2018-04-09"|Date>="2018-04-11"&Date<="2018-04-18"|Date>="2018-04-26"]
ggplot(data=subsub_growth, aes(Variety, Height, fill=Variety)) + 
  geom_boxplot() +
  facet_grid(Chamber ~ Date) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Grass Height Average") +
  xlab("Date") + ylab("Mean(Height)")

##plot means as boxplot graph (facet colour)                               fffffffff
subsub_growth<-growth[Variety!='Bar'&Date<="2018-04-09"|Date>="2018-04-11"&Date<="2018-04-18"|Date>="2018-04-26"]
ggplot(data=subsub_growth, aes(Variety, Height, fill=Variety)) + 
  geom_boxplot() +
  facet_grid(Chamber ~ Date) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Grass Height Average") +
  xlab("Date") + ylab("Mean(Height)")


##plot means as bar graph smooth (not nice, not enough timepoint)
ggplot(data=growth_average[Variety!='Bar'], aes(x=Date, y=Mean)) + 
  geom_point(shape=1,size=2) + 
  facet_grid(. ~ Variety) +
  stat_smooth(aes(linetype=Chamber),  formula = y ~ s(x, k = 5), method = "gam", se = FALSE)


### STATS 1way anova ####

# for Chamber M #

growth<-data.table(growth)
sub_growth_M<-growth[Date>="2018-05-9"&Date<="2018-05-11"&Chamber=="M"]
boxplot(sub_growth_M$Height~sub_growth_M$Variety)        #graphical summary
aov_growth_M = aov(Height~Variety,sub_growth_M)  #do the analysis of variance
summary(aov_growth_M)                                    #show the summary table
print(model.tables(aov_growth_M,"means"),digits=3)       #report the means and the number of subjects/cell
###tukey 
tuk<- TukeyHSD(aov_growth_M)
tuk


### STAS 1way anova STAS 1way anova
### for Chamber L
growth<-data.table(growth)
sub_growth_L<-growth[as.character(Date)=="2018-05-10"&Chamber=="L"]
aov_growth_L = aov(Height~Variety,sub_growth_L)  #do the analysis of variance
summary(aov_growth_L)                                    #show the summary table
print(model.tables(aov_growth_L,"means"),digits=3)       #report the means and the number of subjects/cell
boxplot(sub_growth_L$Height~sub_growth_L$Variety)        #graphical summary
###tukey 
tuk<- TukeyHSD(aov_growth_L)
tuk


#### Normality Test ####

#if (TRUE){
  #Grass='Gl'
#Data$Turbidity_tuk =   transformTukey(Data$Turbidity,                 plotit=FALSE)
  sub_growth<-growth_out
  sub_growth$Height<-transformTukey(sub_growth$Height,plotit=FALSE)
  sub_growth<-na.omit(sub_growth)
  ListVariety<-unique(sub_growth$Variety)
  list_date<-unique(sub_growth$Date)
  resume<-data.table(var=character(),date=character(),shapiro=numeric())
  plotNormalHistogram(sub_growth$Height)
  for (j in 1:(length(ListVariety))) {
    for (da in 1:length(list_date)){
          Grass<-ListVariety[j] 
          subG<-sub_growth[Variety==Grass&as.character(Date)==as.character(list_date[da])]$Height
          print('Grass :', face="bold-italic")
          print(Grass)
          print(paste(Grass,shapiro.test(subG)[2],sep="~"))
          #print(shapiro.test(subG))
          #print(agostino.test(subG))
          #print(length(subG))
          #print(ggdensity(subG, title = Grass))
          #print(ggqqplot(subG, title = Grass))
         
          resume<-rbind(resume,list(Grass,as.character(list_date[da]),shapiro.test(subG)[2]))
        }
    }
  print(paste(Grass,shapiro.test(subG)[2],sep="~"))
  print(ggdensity(subG, title = Grass))
  print(ggqqplot(subG, title = Grass))
  
  ##BOXCOX transformation #### For test, not used in the end
  #per date and variety

  
  
  library(MASS)
  sub_growth<-growth_out[,Height:=(Height+10)]$Height
  sub_growth<-na.omit(sub_growth)
  Box = boxcox(sub_growth ~ 1,              # Transform Turbidity as a single vector
               lambda = seq(-10,10,0.1)       # Try values -6 to 6 by 0.1
  )
  
  Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
  
  Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
  
  Cox2[1,]                                  # Display the lambda with the greatest
                                            #    log likelihood
  lambda = Cox2[1, "Box.x"]                 # Extract that lambda
  T_box = (sub_growth ^ lambda - 1)/lambda
  lambda
  # Transform the original data
  library(rcompanion)
  plotNormalHistogram(sub_growth)
  plotNormalHistogram(T_box)
  agostino.test(T_box)
  agostino.test(sub_growth)
  sub_growth_tranf<-growth[,Height:=Height+10]
  sub_growth_tranf<-sub_growth_tranf[,Height:= (((Height) ^ lambda - 1)/lambda)]
  
### Friedman test ####
  growth_out2<-growth_out
  growth_out2$Variety<-factor(growth_out2$Variety)
  growth_out2$Date<-factor(growth_out2$Date)
  
  friedman.test(growth_out2$Height,growth_out2$Variety, growth_out$Date)
  friedman.test(Height~Variety|Date, data=growth_out)
  table(growth_out2$Variety,growth_out2$Date)

#####          TWO WAY ANOVA   ###### BETWEEN 2 CHAMBERS    ###############

  
sub_growth1<-growth[(Variety!="Bar")&Chamber!="R", ] ##remove bar and chamber R

list_date<-unique(sub_growth$Date)

###remove outliers
sub_growth_out <- sub_growth1[,.SD[Height < (quantile(Height,0.75)+1.5*IQR(Height)) & Height > (quantile(Height,0.25)-1.5*IQR(Height))],by=c("Variety", "Chamber", "Date")]
##Create table
#date_chosen<-"2018-04-04"
#date_chosen<-"2018-04-17"
#date_chosen<-"2018-04-26"
#date_chosen<-"2018-05-03"
date_chosen<-"2018-05-10"
resume <- data.table(Interaction=character(),
                 value=numeric(), 
                 significance=logical(), 
                 stringsAsFactors=FALSE) 

sub_growth_date<-sub_growth_out[as.character(Date)==date_chosen]   ###Choose date
list_variety<-unique(sub_growth_date$Variety)
lenvar=length(list_variety)   ###number of variety
for (i in 2:lenvar){
  for (j in i:lenvar){
    print(paste(list_variety[i-1],list_variety[j],sep="/"))
  sub_growth_2way<-sub_growth_date[(Variety==list_variety[i-1]|Variety==list_variety[j])]
  aov_growth_2way = aov(Height~Variety*Chamber,sub_growth_2way)  #do the analysis of variance
  resume<-rbind(resume,list((as.character(paste(list_variety[i-1],list_variety[j],sep="_"))),as.numeric(unlist(summary(aov_growth_2way)[[1]][5][1])[3]),(unlist(summary(aov_growth_2way)[[1]][5][1])[3])<0.05))

  ##print(model.tables(aov_growth_2way,"means"),digits=3)         #report the means and the number of subjects/cell
  #boxplot(Height~Chamber*Variety,data=sub_growth_2way)          #graphical summary
  }}
  ##boxplot(Height~Chamber*Variety,data=sub_growth) 
View(resume)
 if (FALSE) {
   
 ggplot(sub_growth_date, aes(x = interaction(Chamber, Variety), y = Height, fill=Variety))+
    geom_bar( stat = "summary", fun.y = "mean")+
    geom_blank() +
    theme_bw() +
    ggtitle(date_chosen)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = 'Variety', y = 'Growth (cm)', size=20) +
    theme(plot.title = element_text(vjust=-0.6, face="bold", size=20))
    #theme(axis.text=element_text(size=16), axis.title=element_text(size=20,face="bold"))
 }   
    ##write pvalues
    write_xlsx(resume, paste(date_chosen,"twoway.csv",sep="_"  ))
    tgc <- summarySE(sub_growth_date, measurevar="Height", groupvars=c("Variety","Chamber"))

    #PLOT
    ggplot(data=tgc, aes(x=Variety, y=Height, fill=Chamber, group=Chamber)) +
      geom_bar(colour="black", stat="identity", position = "dodge2") +
      geom_errorbar(aes(ymax = Height + se, ymin = Height-se, group=Chamber),
                    position = position_dodge(width = 0.9), width = 0.25)+
      geom_blank() +
      theme_bw() +
      ggtitle(date_chosen)+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(x = 'Variety', y = 'Growth (cm)') +
      theme(axis.text=element_text(size=12),
             axis.title=element_text(size=14,face="bold"))+
      #theme(title = element_text(face="bold", size=15)) +
      theme(plot.title = element_text(size=20,face="bold"))+
      scale_fill_brewer(palette="Set1")+
      scale_y_continuous(limits=c(0,40))  #,oob = rescale_none
      
      # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) #+ 
      #theme(legend.position="none")
  
    
   
    #####          TWO WAY ANOVA   ###### BETWEEN 2 DATES and    ###############

    
    sub_growth_out<-growth_out[(Variety!="Bar")&!as.character(Date)=="2018-04-09"&Chamber!="R", ] ##remove bar and chamber R from outliers removed table
    
    
    list_date<-unique(sub_growth_out$Date)
    
    ###remove outliers 
    #sub_growth_out <- sub_growth1[,.SD[Height < (quantile(Height,0.75)+1.5*IQR(Height)) & Height > (quantile(Height,0.25)-1.5*IQR(Height))],by=c("Variety", "Chamber", "Date")]
    
    
    ##Create table
    date<-unique(sub_growth_out$Date)
    ldate<-length(date)
    
     #### RATIO ANALYSIS BETWEEN TWO DATES  
    #choose dates
    doc <- read_pptx()  # create pptx  
    for (a in 1:(ldate-1)){
    for (b in 2:ldate){
        
     
        date1<-a
        date2<-b
        date_chosen1<-as.character(date[date1])
        date_chosen2<-as.character(date[date2])
        
        #define data.table 
        resume <- data.table(Variety1=character(),
                             Variety2=character(),
                             value=numeric(), 
                             significance=numeric(), 
                            stringsAsFactors=FALSE) 
        ###Choose chamber sub_growth_out
        sub_growth_date<-sub_growth_out[(as.character(Date)==date_chosen1|as.character(Date)==date_chosen2)&Chamber=="L"]  
        list_variety<-unique(sub_growth_date$Variety)
        lenvar=length(list_variety)   #number of variety
        #do two way between each variety
        
        ggtitl<-paste(date_chosen1,date_chosen2,sep="-")
        print(ggtitl)
        
        for (i in 1:lenvar){
          for (j in 1:lenvar){
            if (i!=j){
            print(paste(list_variety[i],list_variety[j],sep="/"))
            sub_growth_2way<-sub_growth_date[(Variety==list_variety[i]|Variety==list_variety[j])]
            aov_growth_2way = aov(Height~Variety*Date,sub_growth_2way)  #do the analysis of variance
            resume <-rbind(resume,list(as.character(list_variety[i]),as.character(list_variety[j]),as.numeric(unlist(summary(aov_growth_2way)[[1]][5][1])[3]),(as.numeric(unlist(summary(aov_growth_2way)[[1]][5][1])[3]))))
            }
          }
          
        }
         #View(resume)
        #summary(aov_growth_2way)
        #list((as.character(paste(list_variety[i-1],list_variety[j],sep="_"))),as.numeric(unlist(summary(aov_growth_2way)[[1]][5][1])[3]),(unlist(summary(aov_growth_2way)[[1]][5][1])[3])<0.05)
        ##write pvalues of 2WAY-ANOVA in xl
        write_xlsx(resume, paste(ggtitl,"L_twoway.xlsx",sep="_"  ))
        resume[,value:=NULL]
        resumeC<-cast(resume, Variety1~Variety2)
        write_xlsx(resumeC, paste(ggtitl,"L_twowayC1640.xlsx",sep="_"  ))
        
        #Make Graph
        # Calculate error bars
        tgc <- summarySE(sub_growth_date, measurevar="Height", groupvars=c("Variety","Date"))
        doc<-doc %>% 
          add_slide(layout = "Title and Content", master = "Office Theme") # add slide
        doc<-doc %>% 
          ph_with_text(type = "title", str = ggtitl) # add title
        
        #Make graph
        p<-ggplot(data=tgc, aes(x=Variety, y=Height, fill=as.factor(Date), group=as.factor(Date))) +
          geom_bar(colour="black", stat="identity", position = "dodge2") +
          geom_errorbar(aes(ymax = Height + se, ymin = Height-se, group=as.factor(Date)),
          position = position_dodge(width = 0.9), width = 0.25)+
          ggtitle(ggtitl)+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          labs(x = 'Variety', y = 'Growth (cm)') +
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=14,face="bold"))+
          #theme(title = element_text(face="bold", size=15)) +
          theme(plot.title = element_text(size=20,face="bold"))+
          scale_fill_brewer(palette="Set1")+
          scale_y_continuous(limits=c(0,20))+
          theme_bw(base_size=20) +
          theme(plot.title = element_text(vjust=-0.6,hjust = 0.5, face="bold", size=20))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
        print(p)
        # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) #+ 
        #theme(legend.position="none")
        
          doc <- doc %>% ph_with_vg_at(code= print(p),left=1, top=2, width=6,height=4 )
          #doc <- ph_with_vg_at(doc, code = barplot(1:5, col = 2:6),
                           #    left = 1, top = 2, width = 6, height = 4)
        
    
      }}
    
    
    print(doc, target="twoway_date_variety082018_L.pptx" )
    ### END TWO WAY per date and variety
    
    
    
    
                                             ### STATS 1way anova ####

    
    
    sub_growth_out<-growth_out[(Variety!="Bar")&!as.character(Date)=="2018-04-09"&Chamber!="R", ] ##remove bar and chamber R from outliers removed table
    
    
    list_date<-unique(sub_growth_out$Date)
    
    ###remove outliers  (already remvoed in the beginning)
    #sub_growth_out <- sub_growth1[,.SD[Height < (quantile(Height,0.75)+1.5*IQR(Height)) & Height > (quantile(Height,0.25)-1.5*IQR(Height))],by=c("Variety", "Chamber", "Date")]
    
    
    ##Create table
    date<-unique(sub_growth_out$Date)
    ldate<-length(date)
    
    #####  ANALYSIS BETWEEN TWO DATES  
    #choose dates
  #  doc =pptx() # create pptx     
    
    
    list_variety<-unique(sub_growth_date$Variety)
    lenvar=length(list_variety)   #number of variety

    
    
    for (a in 1:(ldate-1)){
       b <- (a+1)
        
        
        date1<-a
        date2<-b
        date_chosen1<-as.character(date[date1])
        date_chosen2<-as.character(date[date2])
        
        ###Choose Date
        sub_growth_date<-sub_growth_out[(as.character(Date)==date_chosen1|as.character(Date)==date_chosen2)&Chamber=="M"]  
     
        #do two way between each variety
        
        ggtitl<-paste(date_chosen1,date_chosen2,sep="_")
        print(ggtitl)
        
        ### ANOVA ONE WAY per see   ####
        model=lm(sub_growth_date$Height~sub_growth_date$Date)
      
        #ANOVA=aov(model)
        #summary(ANOVA)
        
       
        # Tukey test to study each pair of treatment :
       # TUKEY <- TukeyHSD(x=ANOVA, 'sub_growth_date$Variety', conf.level=0.95)
        #(TUKEY)
        # View(resume)
        ##write pvalues of 2WAY-ANOVA in xl
       
        ggtitl<-paste(date_chosen1,date_chosen2,sep="-")
        print(ggtitl)
        #do ttest between each date for each variet
        
        
        resume <- data.table(Interaction=character(),
                             value=numeric(), 
                             significance=logical(), 
                             stringsAsFactors=FALSE)
      
        
        for (v in 1:lenvar){
          
            print(list_variety[v])
            results_ttest<-t.test(sub_growth_date[Variety==list_variety[v]&as.character(Date)==date_chosen1,Height],sub_growth_date[Variety==list_variety[v]&as.character(Date)==date_chosen2,Height])
            results_ttest[3]
            resume<-rbind(resume,list(as.character(list_variety[v]),as.numeric(results_ttest[3]),(results_ttest[3])<0.05))
        }
      
          
        write_xlsx(resume, paste(ggtitl,"ttest.xlsx",sep="_"  ))
      }  
    
      
                                  #### GROWTH between two dates  ####
    
    ######### STATS 1way anova ##

    
    sub_growth_out<-growth_out[(Variety!="Bar")&!as.character(Date)=="2018-04-09"&Chamber!="R", ] ##remove bar and chamber R from outliers removed table
    
    
    
    ##Create table
    date<-unique(sub_growth_out$Date)
    ldate<-length(date)
    
                                   
    #choose dates
  
    
    
    list_variety<-unique(sub_growth_date$Variety)
    lenvar=length(list_variety)   #number of variety
    
    #declare ppt 
    #doc =pptx() # create pptx    
    #Loop per date
    for (a in 1:(ldate-1)){
      for (b in 2:ldate){
        
        
        date1<-a
        date2<-b
        date_chosen1<-as.character(date[date1])
        date_chosen2<-as.character(date[date2])
        
        ###Choose Date
        sub_growth_date<-sub_growth_out[(as.character(Date)==date_chosen1|as.character(Date)==date_chosen2)&Chamber=="M"]  
        
        #do two way between each variety
        
        ggtitl<-paste(date_chosen1,date_chosen2,sep="_")
        print(ggtitl)
        
   

        resume <- data.table(Interaction=character(),
                             value=numeric(), 
                             stringsAsFactors=FALSE)
        for (v in 1:lenvar){
          
          print(list_variety[v])
          growthratediff<-sub_growth_date[Variety==list_variety[v]&as.character(Date)==date_chosen2,mean(Height)]-sub_growth_date[Variety==list_variety[v]&as.character(Date)==date_chosen1,mean(Height)]
          growthratediff<-growthratediff/(sub_growth_date[Variety==list_variety[v]&as.character(Date)==date_chosen1,mean(Height)])
          growthratediff<-(growthratediff/as.numeric(date[date2]-date[date1]))*100
          resume<-rbind(resume,list(as.character(list_variety[v]),as.numeric(growthratediff)))
        }
    
        ##plot means as bar graph in one graph (two colours)
        ggtile<-paste("Difference of Height Average between two dates")
        growth_diff_average<-resume
        names(growth_diff_average)=c("Variety","Height")
        p<-ggplot(growth_diff_average, aes(x=Variety, y=Height, fill=Variety)) +
          geom_bar(stat="identity") +
          ggtitle(ggtitl)+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          labs(x = 'Variety', y = 'Growth (% per day)') +
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=14,face="bold"))+
          #theme(title = element_text(face="bold", size=15)) +
          theme(plot.title = element_text(size=20,face="bold"))+
          #scale_fill_brewer(palette="Set1")+
          scale_y_continuous(limits=c(0,5))+
          theme_bw(base_size=20) +
          theme(plot.title = element_text(vjust=-0.6,hjust = 0.5, face="bold", size=20))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
        print(p)
        
        
        #doc<-addSlide(doc,"Title and Content") # add slide
        #doc<-addTitle(doc,ggtitl) # add title
        #doc <- addPlot(doc, fun=function() print(p) ,vector.graphic =TRUE )  # add plot
        
        
    }}
    #writeDoc(doc, "growth_diff3005b.pptx" )
    


### END of Graph based on growth diff
    

      
    
        
    