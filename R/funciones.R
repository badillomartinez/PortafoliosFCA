

#####FUNCIÓN QUE DESCARGA DATOS DE LOS ACTIVOS############


#' @export
datosInstrumento<-function(simbolo){

  instrumento<-getSymbols(simbolo, src='yahoo', auto.assign=FALSE)

  seriePrecios<-na.omit(instrumento[,paste(simbolo, '.Adjusted', sep='')])

  return(seriePrecios)

}

###############FUNCIÓN QUE ENTREGA UNA TABLA DE PRECIOS DADO UN VECTOR DE TICKERS############

#' @export
listaprecios<-function(listavalores){
  precios<-data.frame(Date=character())

  for (i in 1:length(listavalores)){
    valor<-datosInstrumento(listavalores[i]) ###Pprecios ajustados

    if (i==1){
      precios<-valor
      names(precios)<-c(listavalores[1])
    }
    else{
      precios<-merge(precios, valor, join='inner')
      names(precios)<-c(listavalores[1:i])
    }
  }
  return(precios)
}

#################FUNCION QUE ENTREGA LOS RENDIMIENTOS DE UN ACTIVO DADA LA SERIE DE PRECIOS###############

#' @export
rendimientos<-function(precios){
  dfRendimientos<-precios/lag(precios, k=1) -1
  return(dfRendimientos)
}

############FUNCIóN QUE ENTREGA UNA TABLA DE RENDIMIENTOS DADA UNA TABLA DE RENDIMIENTOS##################

#' @export
rendimientosprecios<-function(precios){
  tabrendimientos<-data.frame(Date=character()) ###DataFrame vacío

  for (i in 1:length(names(precios))){
    if (i==1){
      tabrendimientos<-rendimientos(precios[,i])

    }
    else{
      tabrendimientos<-cbind(tabrendimientos,rendimientos(precios[,i]))

    }
  }
  longitud=length(tabrendimientos[,1])
  return(tabrendimientos[c(2:longitud), ]) #### quitamos la primer fila donde no es posible calcular rendimientos
}

##############Distribución de rendimientos


gaussiana<-function(x,m,s) (2*pi*s^2)^(-.5)*exp(-.5*(x-m)^2/s^2)

#' @export
histograma<-function(columnarendimientos){
  m<-mean(columnarendimientos)
  s<-sd(columnarendimientos)
  a<-round(jb.norm.test(columnarendimientos)$p.value,digits = 4)

  normalidad='Se distribuye normal'

  if (a <0.05){
    normalidad='No se distribuye normal'
  }
  etiqvar=''

  var=round(quantile(columnarendimientos, 0.05),digits = 6)
  etiqvar=paste('\nValor en riesgo al 95%:', var)


  h<-ggplot(columnarendimientos, aes(x =columnarendimientos)) +
    geom_histogram(aes(y =..density..),
                   colour = "black",
                   fill = "cyan2",
                   bins = length(columnarendimientos)^(1/2)) +
    stat_function(fun=gaussiana, args=list(m=m, s=s), colour="coral3")+
    labs(title=paste("Distribución de precios ", names(columnarendimientos[0])), subtitle =  paste("Valores diarios \nP-value Jarque Bera:", a, normalidad, etiqvar))+
    xlab("Rendimiento diario")
  return(h)
}
###########ESTADISTICAS INDIVIDUALES##########

#' @export
rendimientohistorico<-function(vectorPrecios){
  inicio<-as.numeric(first(vectorPrecios))
  fin<-as.numeric(last(vectorPrecios))
  return(fin/inicio -1)
}

#' @export
rendimientoAnualizado<-function(vectorPrecios){
  diasTrans<-as.numeric(end(vectorPrecios)-start(vectorPrecios))
  rendimientoSimple<-rendimientohistorico(vectorPrecios)
  rendAnu<-(1+rendimientoSimple)^(365/diasTrans)-1
  return(rendAnu)
}

#' @export
parametros_CAPM<-function(rendActivo, rendMercado){
  regresion<-lm(rendActivo~rendMercado)
  parametros<-regresion$coefficients
  names(parametros)<-c('alfa','beta')
  return(parametros)
}


#' @export
rendimientoEsperado<-function(rendActivo, tiempo=1){
  promedio=mean(rendActivo)*tiempo
  return(promedio)
}


#' @export
volatilidad<-function(rendActivo, tiempo=1){
  return(sd(rendActivo)*tiempo^(1/2))
}
##########FUNCIONES INDICADORES DESEMPEÑO##########


#' @export
SharpeAbs<-function(vectorRend, tiempo=1){
  rendimientodiario<-mean(vectorRend)
  rendTemporal=rendimientoEsperado(vectorRend, tiempo) #####No se vale solo multiplicar por tiempo pues no se usan logrendimientos ni tasas continuas
  volatilidadAct<-volatilidad(vectorRend, tiempo)
  return(rendTemporal/volatilidadAct)
}


#' @export
SharpeExceso<-function(vectorRend, tiempo=1, r){
  SharpeA<-SharpeAbs(vectorRend, tiempo)
  volatilidadAct<-volatilidad(vectorRend, tiempo)
  return(SharpeA-((r/tiempo)/volatilidadAct))
}


#' @export
M2<-function(vectorRend, rmercado,volMercado, tiempo=1){
  rendimientoPortafolio<-rendimientoEsperado(vectorRend, tiempo)
  SharpePortafolio<-SharpeExceso(vectorRend,r=rmercado, tiempo=tiempo)
  volatilidadAct<-volatilidad(vectorRend, tiempo)
  return(rendimientoPortafolio+SharpePortafolio*(volatilidadAct-volMercado))
}


#' @export
M2exceso<-function(vectorRend, rmercado,volMercado, tiempo=1){
  M2P<-M2(vectorRend, rmercado,volMercado, tiempo)
  return((1+M2P)/(1+rmercado)-1)
}


#' @export
treynor<-function(rendActivo, rendMercado, tiempo=1){
  anualizadoPortafolio<-rendimientoEsperado(rendActivo, tiempo)
  anualizadoMercado<-rendimientoEsperado(rendMercado,tiempo)
  beta<-parametros_CAPM(rendActivo, rendMercado)['beta']
  traynor<-as.numeric((anualizadoPortafolio-anualizadoMercado)/beta)
  return(traynor)
}

#' @export
treynorAjustado<-function(rendActivo, rendMercado, tiempo=1){
  treynorPort<-treynor(rendActivo, rendMercado, tiempo)
  volmercado<-volatilidad(rendMercado, tiempo)
  return(treynorPort*(1/volmercado))
}

#' @export
Jensen<-function(rendActivo, rendMercado, tiempo=1)
{
  coeficientes<-parametros_CAPM(rendActivo, rendMercado)
  volmercado<-volatilidad(rendMercado, tiempo)
  return(coeficientes['alfa']/(coeficientes['beta']))
}

#' @export
maxCaida<-function(precioActivo){
  max_dif=0

  for (i in 2:length(precioActivo)){
    valor<-as.numeric(precioActivo[i])
    fecha<-index(precioActivo)[i]
    maxRelativo<-as.numeric(max(precioActivo[paste('/',index(precioActivo)[i], sep='')]))
    caida=valor/maxRelativo-1
    if (caida<max_dif){
      max_dif<-caida
    }
  }
  return(max_dif)
}


#' @export
sterling<-function(rendActivo, rendMercado, precioActivo, tiempo=1){
  rendimientosPortafolio<-rendimientoEsperado(rendActivo, tiempo)
  rendimientoMercado<-rendimientoEsperado(rendMercado, tiempo)
  dropdownMax<-maxCaida(precioActivo)
  razSterling<-(rendimientosPortafolio-rendimientoMercado)/dropdownMax
  return(razSterling)
}


#' @export
semidesviacionNeg<-function(rendActivo, objetivo=0, tiempo=1){
  copia=rendActivo-objetivo/tiempo
  copia[copia[,1]>0]=0
  sdNeg=volatilidad(copia, tiempo)
  return(sdNeg)
}


#' @export
semidesviacionPos<-function(rendActivo, objetivo=0, tiempo=1){
  copia=rendActivo-objetivo/tiempo
  copia[copia[,1]<0]=0
  sdNeg=volatilidad(copia, tiempo)
  return(sdNeg)
}


#' @export
probabilidadCaida<-function(rendActivo, objetivo=0){
  copia<-rendActivo[rendActivo[,1]<objetivo]
  proba<-length(copia)/length(rendActivo)
  return(proba)
}


#' @export
probabilidadSubida<-function(rendActivo, objetivo=0){
  copia<-rendActivo[rendActivo[,1]>objetivo]
  proba<-length(copia)/length(rendActivo)
  return(proba)
}
########funciones para comparativos multiples


#' @export
preciosbase100<-function(tablaprecios){
  tabla100<-tablaprecios[,1]*100/as.numeric(first(tablaprecios[,1]))
  for (i in 2 : length(names(tablaprecios))){
    precio100<-tablaprecios[,i]*100/as.numeric(first(tablaprecios[,i]))
    tabla100<-merge(tabla100,precio100, join='inner')
  }

  return(tabla100)
}

#' @export
graficabase100<-function(tablaprecios){
  base100<-preciosbase100(tablaprecios)
  base100Largo<-melt(fortify.zoo(base100), id.vars='Index')
  grafica<-ggplot(base100Largo)+geom_line(aes(x=Index,y=value, color=variable))
  return(grafica)
}

#' @export
preciosPortafolio<-function(tablaprecios, vectorPesos, nombre){
  for (i in 1:ncol(tablaprecios)){
    tablaprecios[,i]=tablaprecios[,i]*vectorPesos[i]
  }
  columnaPort=xts(rowSums(tablaprecios), index(tablaprecios))
  names(columnaPort)=c(nombre)
  return(columnaPort)
}

#' @export
graficaCorrelacion<-function(tablarendimientos){

  nombres<-names(tablarendimientos)
  correlacion<-data.frame(cor(tablarendimientos))

  correlacion<-rownames_to_column(correlacion, var='variable1')

  correlacion<-melt(correlacion, id.vars=c('variable1'))

  names(correlacion)<-c('variable1','variable2','r')

  correlacion$variable1<-factor(correlacion$variable1, levels = nombres)
  correlacion$variable2<-factor(correlacion$variable2, levels=rev(nombres))

  grafica<-ggplot(data=correlacion, aes(x=variable1, y=variable2, fill=r, label=round(r,3)))+geom_tile() +
    labs(x = NULL, y = NULL, fill = "Coeficiente de Correlación", title="Correlación para los rendimientos")+
    scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1))+
    geom_text()+
    theme_classic()
  return(grafica)
}


#' @export
graficaRR<-function(tablarendimientos){
  simbolo=names(tablarendimientos)
  riesgorendimiento=data.frame(simbolo)

  rend<-NULL
  vol<-NULL
  for (i in riesgorendimiento[,1]){
    rend<-c(rend,rendimientoEsperado(tablarendimientos[,i]))
    vol<-c(vol,volatilidad(tablarendimientos[,i]))
  }

  riesgorendimiento[,'Rendimiento'] <- rend
  riesgorendimiento[,'Riesgo'] <- vol

  grafica=ggplot(riesgorendimiento, aes(y=Rendimiento, x=Riesgo, color=simbolo, label=simbolo))+geom_point()+geom_text(hjust=-0.002, vjust=0)
  return(grafica)
}


######Funciones Portafolios

pesosSimulacion<-function(rendimientos, tamanio){
  pesos<-data.frame(Date=character()) ###DataFrame vacío

  for (i in 1:ncol(rendimientos)){
    if (i==1){
      pesos<-as.data.frame(runif(tamanio))
      names(pesos)<-c(names(rendimientos)[1])
    }
    else if(i<ncol(rendimientos)){
      columnapesos<-as.data.frame(rowSums(pesos))
      pesos[,names(rendimientos)[i]]<-apply(columnapesos, 1,completaAleatorios)

    }
    else{
      pesos[,names(rendimientos)[i]]<-1-rowSums(pesos)
    }
  }

  return(pesos)
}

#' @export
pesosSimulacionvtascorto<-function(rendimientos, tamanio){
  pesos<-data.frame(Date=character()) ###DataFrame vacío

  for (i in 1:ncol(rendimientos)){
    if (i==1){
      pesos<-as.data.frame(runif(tamanio,min=-1, max=1))
      names(pesos)<-c(names(rendimientos)[1])
    }
    else if(i<ncol(rendimientos)){

      pesos[,names(rendimientos)[i]]<-as.data.frame(runif(tamanio,min=-2, max=2))

    }
    else{
      pesos[,names(rendimientos)[i]]<-1-rowSums(pesos)
    }
  }

  return(pesos)
}



completaAleatorios<-function(suma){
  aleatorio<-runif(1,max=1-suma)
  return(aleatorio)
}

#' @export
rendimientosPesos<-function(vectorPesos, vectorRendimientos){
  return(as.numeric(vectorPesos)%*%as.matrix(vectorRendimientos))
}

#' @export
riesgoPortafolio<-function(vectorPesos, matrizSigma){
  varianza=as.numeric(vectorPesos)%*%matrizSigma%*%as.matrix(vectorPesos)
  return(varianza)
}

#' @export
simulacionPortafolios<-function(rendimientos, tamanio, vectorRendimientos, matrizSigma){
  pesos<-pesosSimulacion(rendimientos, tamanio)
  rendimientoSimulaciones<-apply(pesos, MARGIN = 1, rendimientosPesos, vectorRendimientos=vectorRendimientos)
  riesgoSimulaciones<-apply(pesos,MARGIN=1,riesgoPortafolio, matrizSigma=matrizSigma)

  pesos['rendEsperado']=rendimientoSimulaciones
  pesos['varianza']=riesgoSimulaciones
  pesos['volatilidad']=sqrt(pesos['varianza'])
  return(pesos)
}

#' @export
simulacionPortafoliosvtasCorto<-function(rendimientos, tamanio, vectorRendimientos, matrizSigma){
  pesos<-pesosSimulacionvtascorto(rendimientos, tamanio)
  rendimientoSimulaciones<-apply(pesos, MARGIN = 1, rendimientosPesos, vectorRendimientos=vectorRendimientos)
  riesgoSimulaciones<-apply(pesos,MARGIN=1,riesgoPortafolio, matrizSigma=matrizSigma)

  pesos['rendEsperado']=rendimientoSimulaciones
  pesos['varianza']=riesgoSimulaciones
  pesos['volatilidad']=sqrt(pesos['varianza'])
  return(pesos)
}

#' @export
MVP<-function(matrizSigma, vectorRendimientos){
  tamanio<-length(vectorRendimientos)
  unos<-rep(1, tamanio)
  matrizAumentada<-rbind(matrizSigma, unos)
  unos<-c(unos,0)
  matrizAumentada<-cbind(matrizAumentada, unos)
  portMVP<-solve(matrizAumentada, c(rep(0,tamanio),1))
  return(portMVP[1:tamanio])
}

#' @export
minVarRend<-function(rend, matrizSigma, vectorRendimientos){
  tamanio<-length(vectorRendimientos)
  unos<-rep(1, tamanio)
  matrizAumentada<-rbind(matrizSigma, unos)
  unos<-c(unos,0)
  matrizAumentada<-cbind(matrizAumentada, unos)
  esperanzas<-c(vectorRendimientos,0)
  matrizAumentada<-cbind(matrizAumentada, esperanzas)
  esperanzas<-c(esperanzas,0)
  matrizAumentada<-rbind(matrizAumentada, esperanzas)
  resultado<-c(rep(0,tamanio),1,rend)
  pesosRend<-solve(matrizAumentada, resultado)[0:tamanio]
  return(pesosRend)
}

#' @export
minVarianza<-function(matrizSigma, rendObj, minimos=rep(0,tamanio), maximos=rep(1,tamanio)){
  valorRestriccion<-c(1,rendObj)
  funcionRestriccion<-function(x){
    return(restricciones%*%x-valorRestriccion)
  }
  w<-slsqp(rep(0,tamanio), fn=riesgoPortafolio, lower=minimos, upper=maximos, heq = funcionRestriccion,matrizSigma=matrizSigma)
  return(w$par)
}

#' @export
minVarianza<-function(matrizSigma, vectRendimientos, rendObj, n,minimos=rep(0,n), maximos=rep(1,n)){
  restricciones<-rbind(c(rep(1,n)), vectRendimientos)
  valorRestriccion<-c(1,rendObj)
  funcionRestriccion<-function(x){
    return(restricciones%*%x-valorRestriccion)
  }
  w<-slsqp(rep(0,n), fn=riesgoPortafolio, lower=minimos, upper=maximos, heq = funcionRestriccion,matrizSigma=matrizSigma)
  return(w$par)
}
