library("utiml")
library("proxy")

# Função que define o formato que vai ser impresso a data
logger <- function(a, b=NULL, c=NULL, d=NULL, e=NULL, f=NULL){
  pars <- c(a, b, c, d, e, f)
  str = paste(pars, sep = " ")
  cat(format(Sys.time(), "%a %b %d %X %Y"), str, "\n", sep = ": ")
}

# Função que pega os resultados do multilabel
showResults <- function(result_multi, nfold, name, method, pred_tipo){
  for (i in 1:length(result_multi)){
    line <- paste(names(result_multi)[i], ": ", result_multi[i])
    logger(paste("RESULT-", name, "-",nfold, "-", method,"-", pred_tipo, ":", sep=""), line)
  }
}

# Função para criar matriz de similaridade
cria_matriz_sim <- function(dslab){
  matriz_similaridade <- proxy::dist(t(dslab), by_rows = TRUE, method = "jaccard", upper = TRUE, diag = TRUE, convert_similarities = FALSE)
  M <-as.matrix(matriz_similaridade)
  return(M)
}


verifica_best_metas <- function(matriz_sim, k_labels){
  
  values <- c();
  
  for (i in 1:nrow(matriz_sim)){
    for (j in i: nrow(matriz_sim)){
      
      if (matriz_sim[i,j] != 1 && matriz_sim[i,j] != 0 ){
        values <- c(values,matriz_sim[i,j])
        names(values)[length(values)] <- paste(i, "-", j, sep = "")
      }
      
    }
  }
  
  # ordena values
  ord_values <- values[order(values, decreasing = T)]
  
  # pega os indices dos k metalabels mais similares
  metas <- names(ord_values)[1:k_labels]
  return(metas)
}


# Função para verificar distância
verifica_sim <- function(i, j, matriz_sim, best_metas) {
  
  valor_sim <- matriz_sim[i, j]
  
  for (x in 1:length(best_metas)){
    
    lin <- as.numeric(strsplit(best_metas[x], "-")[[1]][1])
    col <- as.numeric(strsplit(best_metas[x], "-")[[1]][2])
    
    if (i == lin && j == col ) {
      return(valor_sim)
    }
    
  }
  
  return(NULL)
}


# Função para criar meta
cria_meta <- function(dslab, dslabte, matriz_sim, best_metas) {
  dsmtr <- NULL
  dsmte <- NULL
  
  for (i in 1:(ncol(dslab) - 1)) {
    for (j in (i + 1):ncol(dslab)) {
      valor_sim <- verifica_sim(i, j, matriz_sim,  best_metas)
      
      if (!is.null(valor_sim) && valor_sim < 1 && valor_sim > 0) {
        mtr <- as.integer(dslab[, i] + dslab[, j] >= 2)
        mte <- as.integer(dslabte[, i] + dslabte[, j] >= 2)
        
        if (is.null(dsmtr)) {
          dsmtr <- data.frame(mtr)
          dsmte <- data.frame(mte)
        } else {
          dsmtr <- cbind(dsmtr, mtr)
          dsmte <- cbind(dsmte, mte)
        }
        names(dsmtr)[ncol(dsmtr)] <- paste(i, "-", j, sep = "")
        names(dsmte)[ncol(dsmte)] <- paste(i, "-", j, sep = "")
      }
    }
  }
  
  ret <- list()
  ret$dsmtr <- dsmtr
  ret$dsmte <- dsmte
  
  return(ret)
}

#nds_train = dataset com os meta-labels, lbl_names = nome dos labels originais, demais parâmetros usa default
ECCx <- function (nds_train, lbl_names, base.algorithm = "C5.0", 
                  m = 10, subsample=0.75, attr.space = 0.5,  replacement = TRUE, seed = 1, cores = 1){  
  
  # ECCx Model class
  eccmodel <- list(rounds = m, call = match.call())
  eccmodel$nrow <- ceiling(nds_train$measures$num.instances * subsample)
  eccmodel$ncol <- ceiling(length(nds_train$attributesIndexes) * attr.space)
  eccmodel$cardinality <- nds_train$measures$cardinality
  
  set.seed(seed)
  idx <- lapply(seq(m), function(iteration) {
    list(
      rows = sample(nds_train$measures$num.instances, eccmodel$nrow, replacement),
      cols = sample(nds_train$attributesIndexes, eccmodel$ncol),
      
      # obtem o nome dos meta-labels
      mtl_names <- subset(rownames(nds_train$labels), !(rownames(nds_train$labels) %in%  lbl_names)),
      
      # cria chain com um random dos mtl seguindo de um random dos lbl
      chain = c(sample(mtl_names), sample(lbl_names))
    )
  })
  
  eccmodel$models <- lapply(seq(m), function(iteration) {
    print(paste("iteration: ", iteration, sep=""))
    ndata <- create_subset(nds_train, idx[[iteration]]$rows, idx[[iteration]]$cols)
    chain <- idx[[iteration]]$chain
    
    ccmodel <- cc(ndata, base.algorithm, chain, cores = cores)
    ccmodel$attrs <- colnames(ndata$dataset[, ndata$attributesIndexes])
    rm(ndata)
    
    ccmodel
  })
  class(eccmodel) <- "ECCmodel"
  
  eccmodel
}

calculaAUCPR <- function(pred_puro, dslabte){
  csvsufix <- paste(as.integer(runif(1, 100000, 999999)), ".csv", sep="")
  write.csv(x = pred_puro, file = paste("pred", csvsufix, sep = ""), row.names = F)
  write.csv(x = dslabte, file = paste("true", csvsufix, sep = ""), row.names = F)
  
  #pyprog <- "/home/mauri/PycharmProjects/auprc/auprc.py"
  #cmd <- paste("/home/mauri/cudalocal/python-3.9.16/bin/python3", pyprog, paste("true", csvsufix, sep = ""), paste("pred", csvsufix, sep = ""), sep=" ")
  
  pyprog <- "/home/mauriferrandin/tools/auprcpython/auprc.py"
  cmd <- paste("python3 ", pyprog, paste("true", csvsufix, sep = ""), paste("pred", csvsufix, sep = ""), sep=" ")

  print(cmd)
  values <- system(cmd, intern = T)
  
  file.remove(paste("true", csvsufix, sep = ""))
  file.remove(paste("pred", csvsufix, sep = ""))
  
  as.numeric(values)
}



train_file<-"/home/mauri/Downloads/mlds/5-fold/birds/birds_train_2"
test_file <-"/home/mauri/Downloads/mlds/5-fold/birds/birds_test_2"
nfactor = 1.5
cl = "MLTB"
cores = 1

if (length(args) > 0){
  args = commandArgs(trailingOnly=TRUE)
  print(args)
  train_file <- args[1]
  test_file <- args[2]
  nfactor <- as.numeric(args[3])
  cl <- args[4]
  cores = as.numeric(args[5])
}


dsname = head(strsplit(tail(strsplit(train_file, "/")[[1]], n=1), "\\_")[[1]], n=1)
fold = as.integer(tail(strsplit(tail(strsplit(train_file, "/")[[1]], n=1), "\\_")[[1]], n=1))
print(paste("file: ", train_file, " nfactor: ", nfactor," method: ", 1, " fold: ", fold, sep=""))


# Carregando os dados de treino e teste
dstrain <- mldr(train_file, force_read_from_file = T)
dstest <- mldr(test_file, force_read_from_file = T)

# Extraindo os rótulos das instâncias de treino e teste
dslab <- dstrain$dataset[,dstrain$labels$index]
dslabte <- dstest$dataset[,dstest$labels$index]


if (cl == "ECC"){
  # ECC_Puro
  
  model <- ecc(dstrain, base.algorithm = "C5.0", cores = cores, seed = 1)
  pred_puro <- predict(model, dstest)
  result_3 <- multilabel_evaluate(dstest, pred_puro, labels = T)
  if(is.na(result_3$multilabel["macro-AUC"])){
    result_3$multilabel["macro-AUC"] <- mean(result_3$labels[,"AUC"], na.rm=TRUE)
  }
  
  
  # calcula as auprc micro, macro
  aucprs <- calculaAUCPR(pred_puro, dslabte)
  names(aucprs) <- c("micro-AUCPR", "macro-AUCPR")
  result_3$multilabel <- c(result_3$multilabel, aucprs)
  
  cat("Predição ECC puro", "\n")
  print(result_3)
  
  showResults(result_3$multilabel, fold, dsname,  nfactor, "ECC")
  
} else {
  k_labels <- as.integer(ncol(dslab) * nfactor)
  
  # Criando a matriz de similaridade 
  matriz_sim <- cria_matriz_sim(dslab)
  
  # Verifica melhores labels
  best_metas <- verifica_best_metas(matriz_sim, k_labels)
  
  #if (method== 1) {
  metas <- cria_meta(dslab, dslabte, matriz_sim, best_metas) 
  #} else {
  #  cat("Escolha inválida.")
  #  return()
  #}
  
  dslab <- cbind(metas$dsmtr, dslab)
  dslabte <- cbind(metas$dsmte, dslabte)
  
  dstrainatt <- dstrain$dataset[,dstrain$attributesIndexes]
  dstraint <- mldr_from_dataframe(cbind(dstrainatt, dslab), labelIndices = (ncol(dstrainatt)+1):(ncol(dstrainatt) + ncol(dslab)))
  
  dstestatt <- dstest$dataset[,dstest$attributesIndexes]
  dstestt <- mldr_from_dataframe(cbind(dstestatt, dslabte), labelIndices = (ncol(dstestatt)+1):(ncol(dstestatt) + ncol(dslab)))

  if (cl == "MLTB"){  
    
    model <- ecc(dstraint, base.algorithm = "C5.0", cores = cores, seed = 1)
    pred <- predict(model, dstestt)
    result_1 <- multilabel_evaluate(dstestt, pred, labels = T)
    if(is.na(result_1$multilabel["macro-AUC"])){
      result_1$multilabel["macro-AUC"] <- mean(result_1$labels[,"AUC"], na.rm=TRUE)
    }
    
    # calcula as auprc micro, macro
    aucprs <- calculaAUCPR(pred, dstestt$dataset[,dstestt$labels$index])
    names(aucprs) <- c("micro-AUCPR", "macro-AUCPR")
    result_1$multilabel <- c(result_1$multilabel, aucprs)
    
    cat("Predição com mtlb", "\n")
    print(result_1)
    
    showResults(result_1$multilabel, fold, dsname,  nfactor, "MLTB")
    
    pred <- pred[,rownames(dstrain$labels)]
    result_2 <- multilabel_evaluate(dstest, pred, labels = T)
    if(is.na(result_2$multilabel["macro-AUC"])){
      result_2$multilabel["macro-AUC"] <- mean(result_2$labels[,"AUC"], na.rm=TRUE)
    }
    
    # calcula as auprc micro, macro
    aucprs <- calculaAUCPR(pred, dstest$dataset[,dstest$labels$index])
    names(aucprs) <- c("micro-AUCPR", "macro-AUCPR")
    result_2$multilabel <- c(result_2$multilabel, aucprs)
    
    cat("Predição com mtlb retirado","\n")
    print(result_2)
    
    showResults(result_2$multilabel, fold, dsname,  nfactor, "SMLTB")
  } else if (cl == "XMLTB"){  
    model <- ECCx(dstraint, lbl_names = rownames(dstrain$labels),  base.algorithm = "C5.0", cores = cores, seed = 1)
    pred <- predict(model, dstestt)
    result_4 <- multilabel_evaluate(dstestt, pred, labels = T)
    if(is.na(result_4$multilabel["macro-AUC"])){
      result_4$multilabel["macro-AUC"] <- mean(result_4$labels[,"AUC"], na.rm=TRUE)
    }
    
    # calcula as auprc micro, macro
    aucprs <- calculaAUCPR(pred, dstestt$dataset[,dstestt$labels$index])
    names(aucprs) <- c("micro-AUCPR", "macro-AUCPR")
    result_4$multilabel <- c(result_4$multilabel, aucprs)
    
    cat("Predição com mtlb", "\n")
    print(result_4)
    showResults(result_4$multilabel, fold, dsname,  nfactor, "XMLTB")
    
    pred <- pred[,rownames(dstrain$labels)]
    result_5 <- multilabel_evaluate(dstest, pred, labels = T)
    
    if(is.na(result_5$multilabel["macro-AUC"])){
      result_5$multilabel["macro-AUC"] <- mean(result_5$labels[,"AUC"], na.rm=TRUE)
    }

    # calcula as auprc micro, macro
    aucprs <- calculaAUCPR(pred, dstest$dataset[,dstest$labels$index])
    names(aucprs) <- c("micro-AUCPR", "macro-AUCPR")
    result_5$multilabel <- c(result_5$multilabel, aucprs)
    
    cat("Predição com mtlb retirado","\n")
    print(result_5)
    
    showResults(result_5$multilabel, fold, dsname,  nfactor, "XSMLTB")
    
  } else{
    print("Valor inválido para cl")
  }
}

