
flujodecaudal=function(caudal1,caudal2,caudal3){
  
  caudal1bajo  = c(230, 230, 320, 350);
  caudal1medio = c(320, 350, 440, 530);
  caudal1alto  = c(440, 530, 640, 680);
  
  caudal2bajo  = c(230, 230, 320, 350);
  caudal2medio = c(320, 350, 440, 530);
  caudal2alto  = c(440, 530, 640, 680);
  
  caudal3bajo  = c(230, 230, 320, 350);
  caudal3medio = c(320, 350, 440, 530);
  caudal3alto  = c(440, 530, 640, 680);
  
  Aumentacaudal = 1000;
  Mantienecaudal = 700;
  Disminuyecaudal = 300;
  
  
  mu_bajo  = calcula_Mu(caudal1bajo,caudal1);
  mu_medio = calcula_Mu(caudal1medio,caudal1);
  mu_alto  = calcula_Mu(caudal1alto,caudal1);
  
  mu_bajo2  = calcula_Mu(caudal2bajo,caudal2);
  mu_medio2 = calcula_Mu(caudal2medio,caudal2);
  mu_alto2  = calcula_Mu(caudal2alto,caudal2);
  
  mu_bajo3  = calcula_Mu(caudal3bajo,caudal3);
  mu_medio3 = calcula_Mu(caudal3medio,caudal3);
  mu_alto3  = calcula_Mu(caudal3alto,caudal3);
  
  
  basereglas = matrix(c(Y(caudal1bajo,caudal2bajo,caudal3bajo),Disminuyecaudal,
                        Y(caudal1bajo,caudal2bajo,caudal3medio),Disminuyecaudal,
                        Y(caudal1bajo,caudal2bajo,caudal3alto),Mantienecaudal,
                        
                        Y(caudal1bajo,caudal2medio,caudal3bajo),Disminuyecaudal,
                        Y(caudal1bajo,caudal2medio,caudal3medio),Mantienecaudal,
                        Y(caudal1bajo,caudal2medio,caudal3alto),Aumentacaudal,
                        
                        Y(caudal1bajo,caudal2alto,caudal3bajo),Disminuyecaudal,
                        Y(caudal1bajo,caudal2alto,caudal3medio),Mantienecaudal,
                        Y(caudal1bajo,caudal2alto,caudal3alto),Aumentacaudal,
                        
                        Y(caudal1medio,caudal2bajo,caudal3bajo),Disminuyecaudal,
                        Y(caudal1medio,caudal2bajo,caudal3medio),Mantienecaudal,
                        Y(caudal1medio,caudal2bajo,caudal3alto),Aumentacaudal,
                        
                        Y(caudal1medio,caudal2medio,caudal3bajo),Mantienecaudal,
                        Y(caudal1medio,caudal2medio,caudal3medio),Mantienecaudal,
                        Y(caudal1medio,caudal2medio,caudal3alto),Aumentacaudal,
                        
                        Y(caudal1medio,caudal2alto,caudal3bajo),Disminuyecaudal,
                        Y(caudal1medio,caudal2alto,caudal3medio),Mantienecaudal,
                        Y(caudal1medio,caudal2alto,caudal3alto),Aumentacaudal,
                        
                        Y(caudal1alto,caudal2bajo,caudal3bajo),Disminuyecaudal,
                        Y(caudal1alto,caudal2bajo,caudal3medio),Mantienecaudal,
                        Y(caudal1alto,caudal2bajo,caudal3alto),Aumentacaudal,
                        
                        Y(caudal1alto,caudal2medio,caudal3bajo),Disminuyecaudal,
                        Y(caudal1alto,caudal2medio,caudal3medio),Mantienecaudal,
                        Y(caudal1alto,caudal2medio,caudal3alto),Aumentacaudal,
                        
                        Y(caudal1alto,caudal2alto,caudal3bajo),Mantienecaudal,
                        Y(caudal1alto,caudal2alto,caudal3medio),Aumentacaudal,
                        Y(caudal1alto,caudal2alto,caudal3alto),Aumentacaudal),nrow=9,ncol=2,byrow=TRUE)
                      
  
  antecedentes = basereglas[,1];
  consecuentes = basereglas[,2];
  
  salida = sum(antecedentes*consecuentes)/sum(antecedentes);
  print(paste0('Caudal 1=',caudal1,' y caudal 2=',caudal2,'y caudal 3=',caudal3,' -> Seleccionar=',salida))
}

Y = function(valor1,valor2,valor3){
  salida = min(valor1,valor2,valor3);
}

calcula_Mu = function(difuso,valor){
  a = difuso[1];
  b = difuso[2];
  c = difuso[3];
  d = difuso[4];
  
  if      (valor<a){
    salida=0;
  }
  else{
    if  (valor<b){
      salida = (valor-a)/(b-a);
    }
    else{ 
      if  (valor<c){
        salida = 1;
      }
      else{
        if  (valor<d){
          salida = (d-valor)/(d-c);
        }
        else{
          salida = 0;
        }
      }
    }
  }
}
