palavras <- c("WILLIAM","CAIO","GABRIEL")

tamanho_grid <- 34

gerarGrid <- function(){
  linha_default <- replicate(tamanho_grid,as.character(rep("_", tamanho_grid)))
  assign("grid", data.frame(linha_default), envir = .GlobalEnv)
  colnames(grid) <<- rep("*", tamanho_grid)
  for (i in 1:tamanho_grid) {
    grid[,i] <<- as.character(grid[,i])
  }
}

montarPalavras <- function(){
  tryCatch({
    direcoes <- c("leftright","updown","diagonalup","diagonaldown")
    for(palavra in palavras){
      tamanho_palavra <- nchar(palavra)
      colocada <- FALSE
      
      while(!colocada){
        orientacao <- sample(direcoes,1)
        switch (orientacao,
                "leftright" = {
                  passo_x <- 1;
                  passo_y <- 0;
                },
                "updown"={
                  passo_x <- 0;
                  passo_y <- 1;
                },
                "diagonalup"={
                  passo_x <- 1;
                  passo_y <- -1;
                },
                "diagonaldown"={
                  passo_x <- 1;
                  passo_y <- 1;
                }
        )
        #escolhendo posicao inicial da palavra
        x_alocado <- sample(1:tamanho_grid, 1)
        y_alocado <- sample(1:tamanho_grid, 1)
        
        x_len <- x_alocado + tamanho_palavra*passo_x
        y_len <- y_alocado + tamanho_palavra*passo_y
        
        #caso saiamos do tamanho do grid, escolha novamente outra posicao
        if(x_len < 0 || x_len >= tamanho_grid) next
        if(y_len < 0 || y_len >= tamanho_grid) next
        
        falhou = FALSE;
        
        for (i in 1:tamanho_palavra) {
          caractere <- substr(palavra, i, i)
          novo_x <- x_alocado + i*passo_x
          novo_y <- x_alocado + i*passo_y
          if(grid[novo_x,novo_y] == "_"){
            next
          } else {
            falhou = TRUE;
            break
          }
        }
        
        if(falhou){
          next
        } else {
          for (i in 1:tamanho_palavra) {
            caractere <- substr(palavra, i, i)
            novo_x <- x_alocado + i*passo_x
            novo_y <- x_alocado + i*passo_y
            grid[novo_x,novo_y] <<- caractere
            colocada = TRUE
          }
        }
      }
    }
  },
  error = function(cond){gerarGrid();montarPalavras()}
  )
}

preencherGrid <- function(){
  alfabeto <- c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z')
  #Fonte das probabilidades: WIKIPEDIA
  probs <- c(0.1463,0.0104,0.0388,0.0499,0.1257,0.0102,0.0130,0.0128,0.0618,0.0040,0.0002,0.0278,0.0474,0.0505,0.1073,0.0252,0.0120,0.0653,0.0781,0.0434,0.0463,0.0167,0.0001,0.0021,0.0001,0.0047)
  for (x in 1:tamanho_grid) {
    for (y in 1:tamanho_grid) {
      if(grid[x,y] == "_"){
        grid[x,y] <<- sample(alfabeto, prob=probs, 1)
      }
    }
  }
}

gerarGrid();montarPalavras();preencherGrid();ptm <- proc.time()
# ------------------------------------------------------
grid
while(1){tryCatch({if(readline(prompt="Aperte ENTER para finalizar o cronometro.") != NULL){}},error = function(cond){print((proc.time()-ptm))});break}
