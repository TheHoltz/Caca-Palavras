palavras <- c("BLOCOS","FATORES")

tamanho_grid <- 20

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
  for (x in 1:tamanho_grid) {
    for (y in 1:tamanho_grid) {
      if(grid[x,y] == "_"){
        grid[x,y] <<- sample(alfabeto, 1)
      }
    }
  }
}

gerarGrid()
montarPalavras()
preencherGrid()
