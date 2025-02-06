options(max.print=999999)
options(digits = 6)
options(scipen=500)
library("scmamp")
library(rlist)

ms <- c("macro-F1", "macro-precision", "macro-recall", "micro-F1", "micro-precision", "micro-recall", "F1", "precision",
        "recall", "hamming-loss", "subset-accuracy", "accuracy", "wlp", "mlp", "clp",  "one-error", "average-precision",
        "coverage", "margin-loss", "ranking-loss", "micro-AUC", "macro-AUC", "micro-AUCPR", "macro-AUCPR")

# índice das medidas negativas - medidas que o menor valor é melhor e.g. hamming-loss, ...
negmes <- c(10, 13,14,15,16,18,19,20)

# índice das medidas de ranking
rankmes <- c(15, 16, 17, 18, 19)

# lista de datasets
ds=c("yeast", "birds", "enron",  "EukaryotePseAAC", "foodtruck", "genbase", "HumanGO", 
     "HumanPseAAC",  "medical", "PlantGO", "PlantPseAAC", "slashdot", "ohsumed", "tmc2007_500", "cal500", 
     "langlog", "ng20", "stackex_chess", "reutersk500")

#ds=c("emotions", "flags", "GnegativeGO", "GnegativePseAAC", "GpositiveGO", "GpositivePseAAC", "scene", "VirusGO", "VirusPseAAC", "Yelp")

# lista de classificadores a serem avaliados
cs = c("ECC", "MLTB03", "MLTB05", "MLTB08", "MLTB10", "MLTB15", "MLTB20",
       "SMLTB03", "SMLTB05", "SMLTB08", "SMLTB10", "SMLTB15", "SMLTB20", 
       "XMLTB03", "XMLTB05", "XMLTB08", "XMLTB10", "XMLTB15", "XMLTB20",
       "XSMLTB03", "XSMLTB05", "XSMLTB08", "XSMLTB10", "XSMLTB15", "XSMLTB20"
)

# lista de classificadores a serem avaliados
cs = c("ECC", "MLTB03", "MLTB05", "MLTB08","MLTB10",
       "SMLTB03", "SMLTB05", "SMLTB08", "SMLTB10",
       "XMLTB03", "XMLTB05", "XMLTB08", "XMLTB10",
       "XSMLTB03", "XSMLTB05" , "XSMLTB08","XSMLTB10"
)

cs = c("ECC", 
       "SMLTB03", "SMLTB05", "SMLTB08", "SMLTB10"#,
       #"XSMLTB03", "XSMLTB05" , "XSMLTB08","XSMLTB10"
)


# diretório onde se encontra o csv com os dados
#input_dir <- "/home/mauri/Downloads/leonardo2/logs/"
#input_dir <- "/home/mauri/Downloads/leonardonb/logs/"
input_dir <- "/home/mauri/Downloads/leonardoornb/logs/"
#input_dir <- "/home/mauri/Downloads/leonardoknn1/logs/"
#input_dir <- "/home/mauri/Downloads/leonardoorsvm/logs/"
#input_dir <- "/home/mauri/Downloads/leonardosvm/logs/"
#input_dir <- "/home/mauri/Downloads/leonardoor/logs/" 
setwd(input_dir)

# diretorio onde serão gravados os resultados

#output_dir <- "/home/mauri/Downloads/leonardo2/results/"
#output_dir <- "/home/mauri/Downloads/leonardonb/results/"
output_dir <- "/home/mauri/Downloads/leonardoornb/results/"
#output_dir <- "/home/mauri/Downloads/leonardoknn1/results/"
#output_dir <- "/home/mauri/Downloads/leonardosvm/results/" 
#output_dir <- "/home/mauri/Downloads/leonardoorsvm/results/" 
#output_dir <- "/home/mauri/Downloads/leonardoor/results/"

todoscomtodos <- function(n){
  ret <- list()
  for (i in 1:(n-1)){
    for (j in (i + 1):n){
      ret[length(ret) + 1] <- list(c(i, j))
    }
  }
  ret
}

generate_ranking <- function(data){
  
  retorno = list()
  coluna = ncol(data)
  linha = nrow(data)
  
  rank_first_0 = data.frame()
  rank_last_0 = data.frame()
  rank_average_0 = data.frame()
  rank_random_0 = data.frame()
  rank_min_0 = data.frame()
  rank_max_0 = data.frame()
  
  i = 1
  for(i in 1:linha){
    rf = rank(data[i,], ties.method = "first")     # first occurrence wins
    rl = rank(data[i,], ties.method = "last")      # last occurrence wins
    rav = rank(data[i,], ties.method = "average")   # média
    ran = rank(data[i,], ties.method = "random")    # ordem aleatória
    rma = rank(data[i,], ties.method = "max")       # máximo
    rmi = rank(data[i,], ties.method = "min")       # mínimo
    
    rank_first_0 = rbind(rank_first_0, rf)
    rank_last_0 = rbind(rank_last_0, rl)
    rank_average_0 = rbind(rank_average_0, rav)
    rank_random_0 = rbind(rank_random_0, ran)
    rank_max_0 = rbind(rank_max_0, rma)
    rank_min_0 = rbind(rank_min_0, rmi)
    
  }
  
  colnames(rank_first_0) = colnames(data)
  colnames(rank_last_0) = colnames(data)
  colnames(rank_average_0) = colnames(data)
  colnames(rank_random_0) = colnames(data)
  colnames(rank_max_0) = colnames(data)
  colnames(rank_min_0) = colnames(data)
  
  
  rank_first_1 = data.frame()
  rank_last_1 = data.frame()
  rank_average_1 = data.frame()
  rank_random_1 = data.frame()
  rank_min_1 = data.frame()
  rank_max_1 = data.frame()
  
  i = 1
  for(i in 1:linha){
    rf = (coluna -  rank_first_0[i,]) +1
    rl = (coluna - rank_last_0[i,]) +  1
    rav = (coluna - rank_average_0[i,]) + 1
    ran = (coluna - rank_random_0[i,]) + 1
    rma = (coluna - rank_max_0[i,]) + 1
    rmi = (coluna - rank_min_0[i,]) + 1
    
    rank_first_1 = rbind(rank_first_1, rf)
    rank_last_1 = rbind(rank_last_1, rl)
    rank_average_1 = rbind(rank_average_1, rav)
    rank_random_1 = rbind(rank_random_1, ran)
    rank_max_1 = rbind(rank_max_1, rma)
    rank_min_1 = rbind(rank_min_1, rmi)
  }
  
  colnames(rank_first_1) = colnames(data)
  colnames(rank_last_1) = colnames(data)
  colnames(rank_average_1) = colnames(data)
  colnames(rank_random_1) = colnames(data)
  colnames(rank_max_1) = colnames(data)
  colnames(rank_min_1) = colnames(data)
  
  rank_average_0 = trunc(rank_average_0,0)
  rank_average_1 = trunc(rank_average_1,0)
  
  retorno$rank_first_0 = rank_first_0
  retorno$rank_last_0 = rank_last_0
  retorno$rank_average_0 = rank_average_0
  retorno$rank_random_0 = rank_random_0
  retorno$rank_max_0 = rank_max_0
  retorno$rank_min_0 = rank_min_0
  
  retorno$rank_first_1 = rank_first_1
  retorno$rank_last_1 = rank_last_1
  retorno$rank_average_1 = rank_average_1
  retorno$rank_random_1 = rank_random_1
  retorno$rank_max_1 = rank_max_1
  retorno$rank_min_1 = rank_min_1
  
  return(retorno)
}

`obter_valores_MLTB03` <- function(m, d){
  
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "0.3-MLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_MLTB05` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "0.5-MLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_MLTB08` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "0.8-MLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_MLTB10` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "\"\\-1-MLTB:\"|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_MLTB15` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "1.5-MLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_MLTB20` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "\"\\-2-MLTB:\"|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}


`obter_valores_SMLTB03` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "0.3-SMLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_SMLTB05` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "0.5-SMLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_SMLTB08` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "0.8-SMLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_SMLTB10` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "\"\\-1-SMLTB:\"|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_SMLTB15` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "1.5-SMLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_SMLTB20` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "\"\\-2-SMLTB:\"|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}


`obter_valores_XMLTB03` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "0.3-XMLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_XMLTB05` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "0.5-XMLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_XMLTB08` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "0.8-XMLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_XMLTB10` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "\"\\-1-XMLTB:\"|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_XMLTB15` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "1.5-XMLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_XMLTB20` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "\"\\-2-XMLTB:\"|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_XSMLTB03` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "0.3-XSMLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_XSMLTB05` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "0.5-XSMLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_XSMLTB08` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "0.8-XSMLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_XSMLTB10` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "\"\\-1-XSMLTB:\"|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_XSMLTB15` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "1.5-XSMLTB:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_XSMLTB20` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "\"\\-2-XSMLTB:\"|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}

`obter_valores_ECC` <- function(m, d){
  cmd <- paste("grep ", paste("\": ", m, " \"", sep=""), " log-", d, "* |grep RES|grep ", "0.3-ECC:|cut -d: -f8", sep="")
  print(cmd)
  values <- system(cmd, intern = T)
  as.numeric(values)
}


obter_valores <- function(){
  first <- T
  for (c in cs){
    print(paste(c, sep = " -> "))
    for (m in ms) {
        for (d in ds){
          print(paste(c, m, d, sep = " -> "))
          values <- do.call(paste("obter_valores_", c, sep=""), list(m, d))
          nv <- length(values)
          #TODO: verificar valores diferentes de NV
          rtemp <- data.frame(dataset=rep(d, nv), classifier=rep(c,nv), fold=seq(1:nv), measure=rep(m,nv), value=values)
          if (first){
            r <- rtemp
            first <- F
          } else {
            r <- rbind(r, rtemp)
          }
        }
    }
  }
  r
}




#df = read.csv(paste(input_dir,"arquivo.csv", sep=""))
df = obter_valores()

# caso queira renomear um classificador 
df[df == "ECC"] <- "ECC"
df[df == "SMLTB03"] <- "A03"
df[df == "SMLTB05"] <- "A05"
df[df == "SMLTB08"] <- "A08"
df[df == "SMLTB10"] <- "A10"

cs = c("ECC", "A03", "A05", "A08", "A10")
# define os grupos para serem comparados, por default está considerando todos os classificadores da lista cs como grupo1
clgroups <- list();
dsgroups <- list();
clgroups[[1]] <- cs
dsgroups[[1]] <- ds

# podem ser criados outros grupos para análises com subconjuntos de classicadores e datasets
# para criar uma segunda análise com apenasos F2Hs em todos os datasets
# clgroups[[2]] <- c("F2HG", "F2HL", "EF2H1", "EF2H2G", "EF2H2L", "EF2H3")
# dsgroups[[2]] <- ds
# 
# para criar um segundo grupo de analise com apenas os não F2Hs em 5 datasets
# clgroups[[3]] <- c("ECCJ48", "RFDTBR", "RFPCT")
# dsgroups[[3]] <- c("yeast","birds","Yelp","emotions","enron")

# nome para arquivos - deve conter um nome para cada grupo definido acima
# esses nomes serão usados como sufixo nos arquivos de saída para identificar cada análise
clsnames <- c("all")

# caso use mais de um grupo - para o exemplo anterior com 3 grupos
# clsnames <- c("all", "F2Hs", "nonF2Hs")


# define como serão as comparações aos pares, o default é todos contra todos
wilco <- todoscomtodos(length(cs))

# é possível especificar um subset de comparações:
# comparar o classificador 1 com o 2, 3 e 4 (o numero corresponde à posição do classificador na lista cs)
#wilco <- list(c(1,2), c(1,3),c(1,4))

###########################################
# NÃO ALTERE AS PRÓXIMAS LINHAS
###########################################

# define a saida para o output_dir informado
setwd(output_dir)

# armazena as inconsistencias dataset com valores diferente de 10
nvalueserror <- c();

friedmanps <- c()

for (g in 1: length(clgroups)){
  
  classifiers <- clgroups[[g]]
  ds <- sort(dsgroups[[g]])
  dfw <- data.frame(matrix(ncol = (length(wilco) * 2), nrow = 0))
  
  totalc1 <- rep(0,length(wilco))
  totalc2 <- rep(0,length(wilco))
  totaleq <- rep(0,length(wilco))
  totalc1s <- rep(0,length(wilco))
  totalc2s <- rep(0,length(wilco))
  
  for (m in ms){
    dfn <- as.data.frame(matrix(nrow=length(ds), ncol=length(classifiers)))
    rownames(dfn) <- ds
    colnames(dfn) <- classifiers
    
    
    for (c in cs){
      
      for (d in ds){
        vl <- as.numeric(df[df$measure == m & df$classifier == c & df$dataset == d,"value"])
        if (length(vl) != 5){
          nvalueserror <- c(nvalueserror, paste(m, c, d, length(vl), sep="-"))
        }
        if (length(vl) > 0){
          dfn[d,c] <- mean(vl, na.rm = T)
        }
      }
    }
    
    mcaptions <- paste(m, clsnames[g], sep="-")
    postscript(paste("z", mcaptions,".eps", sep = ""))
    plotCD(dfn, alpha=0.05, cex=2.5)
    dev.off()
    
    plotCD(dfn, alpha=0.05, cex=1)
    
    friedman <- friedmanTest(dfn, )
    friedmanps <- c(friedmanps, friedman$p.value)
    
    
    # wilcoxon aos pares
    wi <- 1
    res <- c();
    for (cc in wilco){
      c1 <- cc[[1]]
      c2 <- cc[[2]]
      
      npos <- length(which(dfn[c1] - dfn[c2] > 0))
      nneg <- length(which(dfn[c1] - dfn[c2] < 0))
      
      if (m %in% ms[negmes]){
        signal <- ifelse(npos == nneg, "t", ifelse(npos > nneg, "<", ">"))
        signalt <- paste(signal, " (", sprintf("%02d",nneg), "x", sprintf("%02d",npos), ")", sep = "")
      } else {
        signal <- ifelse(npos == nneg, "t", ifelse(npos > nneg, ">", "<"))
        signalt <- paste(signal, " (", sprintf("%02d",npos), "x", sprintf("%02d",nneg), ")", sep = "")
      }
      
      
      res <- c(res,signalt)
      names(res)[length(res)] <- paste(classifiers[c1], "x", classifiers[c2])
      w <- wilcox.test(dfn[,c1], dfn[,c2], paired = T, exact = F)
      res <- c(res, w$p.value)
      names(res)[length(res)] <- paste(cs[c1], "x", cs[c2], "p-value")
      
      
      if (signal == ">"){
        totalc1[wi] <- totalc1[wi] + 1
        if (w$p.value < 0.05){ # TODO parametrizar o alfa
          totalc1s[wi] <- totalc1s[wi] + 1
        }
      } else if (signal == "<"){
        totalc2[wi] <- totalc2[wi] + 1
        if (w$p.value < 0.05){ # TODO parametrizar o alfa
          totalc2s[wi] <- totalc2s[wi] + 1
        }
      } else {
        totaleq[wi] <- totaleq[wi] + 1
      }
      wi <- wi + 1
    }
    print(res)
    dfw <- rbind(dfw, res)
    colnames(dfw) <- names(res)
    
    
    # compute rank
    ranking <- generate_ranking(dfn)
    ranking <- ranking$rank_average_1
    
    
    # add average and ranking
    dfn <- rbind(dfn, colMeans(dfn))
    rownames(dfn)[nrow(dfn)] <- "Average"
    
    dfn <- rbind(dfn, colSums(ranking))
    rownames(dfn)[nrow(dfn)] <- "RankSum"
    
    write.csv(dfn, paste("measures-", mcaptions, ".csv", sep=""))
    
  }
  
  rownames(dfw) <- ms
  
  # linhas dos totais
  dfw <- rbind(dfw, rep("",length(wilco)*2))
  
  linec1 <- c();
  linec2 <- c();
  lineeq <- c();
  linec1s <- c();
  linec2s <- c();
  wi <- 1
  for (cc in wilco){
    c1 <- cs[cc[[1]]]
    c2 <- cs[cc[[2]]]
    
    linec1 <- c(linec1, c(c1, totalc1[wi]))
    linec2 <- c(linec2, c(c2, totalc2[wi]))
    lineeq <- c(lineeq, c("Tie", totaleq[wi]))
    linec1s <- c(linec1s, c(c1, totalc1s[wi]))
    linec2s <- c(linec2s, c(c2, totalc2s[wi]))
    wi <- wi + 1
  }
  dfw <- rbind(dfw, linec1)
  dfw <- rbind(dfw, linec2)
  dfw <- rbind(dfw, lineeq)
  dfw <- rbind(dfw, linec1s)
  dfw <- rbind(dfw, linec2s)
  rownames(dfw)[(nrow(dfw)-5):nrow(dfw)] <- c("Summary","wins c1","wins c2","ties","wins c1 sig","win c2 sig")
  
  
  write.csv(dfw, paste(clsnames[g], "wilcoxon.csv", sep=" "))
  names(friedmanps) <- ms
}

nvalueserror

format(friedmanps, scientific = T)


