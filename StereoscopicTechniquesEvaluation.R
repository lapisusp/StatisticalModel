###################
#
#
#
###################

rm(list=ls())
#Directory
source('C:\\Users\\User\\Desktop\\StatisticalModel\\permutationtest.r')

#Files: objective, subjective, dictionaries, pvalues and scores (simulator and game)
subdir = 'io/'

# datobjfile = 'objective_data_game.txt'
# dicobjfile = 'objective_dic_game.txt'
# datsubfile = 'subjective_data_game.txt'
# dicsubfile = 'subjective_dic_game.txt'
# scorefile = 'scores_game.txt'
# pvfile = 'pvalues_game.txt'

datobjfile = 'objective_data_simulator.txt'
dicobjfile = 'objective_dic_simulator.txt'
datsubfile = 'subjective_data_simulator.txt'
dicsubfile = 'subjective_dic_simulator.txt'
scorefile = 'scores_simulator.txt'
pvfile = 'pvalues_simulator.txt'


CompTechniques = function(data, dictionaire, siglevel=0.1, outlier.range=NULL, 
                          plot=TRUE, subdir='') {

  techniques = sort(unique(data$Technique))
  qtt = length(techniques)
  
  
  # block design
  datanames = colnames(data)
  if ('Scenario' %in% datanames) { 
    data$chave = paste(data$System, prettyNum(data$Individual, width=3), 
                        data$Scenario, sep='-')
  } else {
    data$chave = paste(data$System, prettyNum(data$Individual, width=3), sep='-')
  }
  
  dictionaire$chave = paste(dictionaire$System, dictionaire$Metric, sep='-')
  
  MatrCompar = data.frame()
  MatrEscore = data.frame()
  
  # Outputs 
  TechniqueScore = rep(0, qtt)   # Technique score
  TechniqueWins = rep(0, qtt)    # Technique wins
  TechniqueLosses = rep(0, qtt)  # Technique losses
  TechniqueTies = rep(0, qtt)    # Technique ties
  TechComp = data.frame()        # Pairwise comparisons

  # Identify metrics used in the current system
  metrics = sort(unique(dictionaire$Metric))
  qtm = length(metrics)
  
  for (idm in 1:qtm) {
    currmet = metrics[idm]
    dadossm = subset(data, data$Metric==currmet)
    dicsm = subset(dictionaire, dictionaire$Metric==currmet)
    
    if (plot==TRUE) {
      plotfile = paste(subdir, 'Boxplot-', currsys, '-', currmet, '.png', sep='')
      png(filename=plotfile, width=700, height=400, pointsize=14)
      boxplot(dadossm$Value~dadossm$Technique, outline=FALSE, xlab = 'Technique', ylab=currmet,
              main=paste(currsys, ': ', currmet, sep=''))
      dev.off()
    }
    
    for (idt1 in 1:(qtt-1)) {
      vt1 = subset(dadossm, dadossm$Technique==techniques[idt1])
      for (idt2 in (idt1+1):qtt) {
        vt2 = subset(dadossm, dadossm$Technique==techniques[idt2])
        
        # Identify configurations in common
        chave = sort(intersect(vt1$chave, vt2$chave))
        idxmatch1 = match(chave, vt1$chave)
        idxmatch2 = match(chave, vt2$chave)
        
        idxindiv = vt1$Individual[idxmatch1]
        value1 = vt1$Value
        value2 = vt2$Value*dicsm$Sign
        
        # Remove outliers of the differences
        if (!is.null(outlier.range)) {
          bbsc = boxplot(value1-value2, outline=TRUE, range = 3, plot=FALSE)
          idxvalsc = setdiff(1:length(value1), bbsc$out)
        } else {
          idxvalsc = 1:length(value1)
        }
        
        idxindiv = idxindiv[idxvalsc]
        value1 = value1[idxvalsc]
        value2 = value2[idxvalsc]
        
        mvalue1 = mean(value1)
        mvalue2 = mean(value2)

        resultsc = BlockPermTest(idxindiv, x=value1, y=value2, 
                                 B=10000, alternative='two.sided', paired=TRUE)
        
        if (resultsc$p.value<=siglevel) {
          if (mvalue1*dicsm$Sign > mvalue2*dicsm$Sign) {
            TechniqueWins[idt1] = TechniqueWins[idt1] + 1 
            TechniqueLosses[idt2] = TechniqueLosses[idt2] + 1 
          } else {
            TechniqueWins[idt2] = TechniqueWins[idt2] + 1 
            TechniqueLosses[idt1] = TechniqueLosses[idt1] + 1 
          }
        } else {
          TechniqueTies[c(idt1,idt2)] = TechniqueTies[c(idt1,idt2)] + 1 
        }
        
        CompRes = data.frame(Metric=currmet, Tec1=techniques[idt1], Tec2=techniques[idt2],
                             DifMean=resultsc$statistic, p.value=resultsc$p.value,
                             stringsAsFactors=FALSE)
        TechComp = rbind(TechComp, CompRes)
      }  # for (idt2 in (idt1+1):qtt)
    }  # for (idt1 in 1:(qtt-1))
  }  # for (idm in 1:qtm)

  # Score based on wins-losses
  NormConst = 10/((qtt-1) * qtm)
  TechniqueScore = (TechniqueWins-TechniqueLosses)*NormConst

  # browser()
  
  return(list(techniques=techniques, wins=TechniqueWins, ties=TechniqueTies, 
              losses=TechniqueLosses, score=TechniqueScore, techcomp=TechComp))
}

#Directory
setwd("C:/Users/User/Desktop/StatisticalModel/")

subdic = read.table(paste(subdir, dicsubfile, sep=''), header=TRUE, sep='\t', stringsAsFactors=FALSE)
subdata = read.table(paste(subdir, datsubfile, sep=''), header=TRUE, sep='\t', stringsAsFactors=FALSE)

SubScores = CompTechniques(data=subdata, dictionaire=subdic, plot=FALSE, subdir=subdir)

objdic = read.table(paste(subdir, dicobjfile, sep=''), header=TRUE, sep='\t', stringsAsFactors=FALSE)
objdata = read.table(paste(subdir, datobjfile, sep=''), header=TRUE, sep='\t', stringsAsFactors=FALSE)

ObjScores = CompTechniques(data=objdata, dictionaire=objdic, plot=FALSE, subdir=subdir)

Scores = data.frame(techiques=ObjScores$techniques, 
                     objwin=ObjScores$wins, objtie=ObjScores$ties, objloss=ObjScores$losses, objscore=ObjScores$score, 
                     subwin=SubScores$wins, subtie=SubScores$ties, subloss=SubScores$losses, subscore=SubScores$score,
                     stringsAsFactors=FALSE)
write.table(Scores, paste(subdir, scorefile, sep=''), col.names=TRUE, row.names=FALSE, sep='\t')

PValues = rbind(ObjScores$techcomp, SubScores$techcomp)
write.table(PValues, paste(subdir, pvfile, sep=''), col.names=TRUE, row.names=FALSE, sep='\t')
