#' Localiza el mínimo de una función en un intervalo determinado
#' 
#' @param funx Expresión analítica de la ecuación que queremos minimizar, dependiente de X
#' @param left Límite inferior del intervalo de búsqueda
#' @param right Límite superior del intervalo de búsqueda
#' @param uncert Nivel de incertidumbre admitida
#' @param eps Ruido a introducir en la dicotomía
#' @param details Indica si se desea ver el detalle de la ejecución o no
#' @return El detalle de la ejecución del algoritmo como data.frame
#' @export
opt_dicotomica = function(funx,left,right,uncert = 0.1,eps = 0.01,details = T) {
  if (uncert <= 0)
    stop("uncert should be greater than 0")
  if (eps <= 0)
    stop("epsilon should be greater than 0")
  
  n = 0
  an = left
  bn = right
  mres = NULL
  names = c("n","an","bn","lambdan","mun","fan","fbn","flambdan","fmun","Amplitud","an1","bn1")
  
  repeat {
    lambda.n = ((an + bn) / 2) - eps
    mu.n = ((an + bn) / 2) + eps
    f.an = funx(an)
    f.bn = funx(bn)
    f.lambda.n = funx(lambda.n)
    f.mu.n = funx(mu.n)
    
    if (f.lambda.n < f.mu.n) {
      an1 = an
      bn1 = mu.n
    } else {
      an1 = lambda.n
      bn1 = bn
    }
    
    vres = c(n + 1,an,bn,lambda.n,mu.n,f.an,f.bn,f.lambda.n,f.mu.n,bn - an,an1,bn1)
    
    if (details)
      if (is.null(mres))
        mres = vres
      else
        mres = rbind(mres, vres)
    
    
    if ((bn - an) <= uncert)
      break
    
    n = n + 1
    an = an1
    bn = bn1
  }
  
  if (details) {
    colnames(mres) = names
    rownames(mres) = NULL
    dfres = as.data.frame(mres)
  } else {
    names(vres) = names
    dfres = as.data.frame(vres)
  }
  
  return(dfres)
}



#' Localiza el mínimo de una función en un intervalo utilizando Golden Ratio
#' 
#' @param funx Expresión analítica de la ecuación que queremos minimizar, dependiente de X
#' @param left Límite inferior del intervalo de búsqueda
#' @param right Límite superior del intervalo de búsqueda
#' @param uncert Nivel de incertidumbre admitida
#' @param details Indica si se desea ver el detalle de la ejecución o no
#' @return El detalle de la ejecución del algoritmo como data.frame
#' @export
opt_goldenratio = function(funx,left,right,uncert=0.1,detail=T) {
  if (uncert <= 0)
    stop("longinc should be greater than 0")
  
  n = nval = 0
  rho = ((3 - sqrt(5)) / 2)
  a0 = left
  b0 = right
  a1 = a0 + rho * (b0 - a0)
  b1 = a0 + (1 - rho) * (b0 - a0)
  f.a1 = funx(a1)
  f.b1 = funx(b1)
  mres = NULL
  names = c("n","a0","b0","a1","b1","fa1","fb1","Amplitud")
  
  repeat {
    f.a1 = funx(a1)
    f.b1 = funx(b1)
    
    if (f.a1 > f.b1) {
      a2 = a1
      b2 = b0
      a3 = b1
      b3 = a2 + (1 - rho) * (b2 - a2)
    } else {
      a2 = a0
      b2 = b1
      a3 = a2 + rho * (b2 - a2)
      b3 = a1
    }
    
    vres = c(n+1,a0,b0,a1,b1,f.a1,f.b1,b0-a0)  
    if (detail)
      if (is.null(mres))
        mres = vres
      else
        mres = rbind(mres, vres)
    
    if ((b0 - a0) <= uncert)
      break

    n = n + 1
    nval = nval + 2
    a0 = a2
    b0 = b2
    a1 = a3
    b1 = b3
  }
  
  if (detail) {
    colnames(mres) = names
    rownames(mres) = NULL
    dfres = as.data.frame(mres)
  } else {
    names(vres) = names
    dfres = as.data.frame(vres)
  }
  
  return(dfres)
}



#' Localiza el mínimo de una función en un intervalo determinado utilizando bisección
#' 
#' @param funx Expresión analítica de la ecuación que queremos minimizar, dependiente de X
#' @param left Límite inferior del intervalo de búsqueda
#' @param right Límite superior del intervalo de búsqueda
#' @param uncert Nivel de incertidumbre admitida
#' @param details Indica si se desea ver el detalle de la ejecución o no
#' @return El detalle de la ejecución del algoritmo como data.frame
#' @export
opt_biseccion = function(funx,left,right,uncert=0.1,details=T) {
  if (uncert <= 0)
    stop("longinc should be greater than 0")
  

  n = 0
  an = left
  bn = right
  mres = NULL
  names = c("n","a","lambda","b","derflambada","Amplitud")
  
  repeat {
    lambda.n = (an + bn) / 2
    fd.lambdan = funx(lambda.n)
    
    if (fd.lambdan == 0) {
      vres = c(n + 1, an, lambda.n, bn, fd.lambdan, bn - an)
      if(is.null(mres))
        details = F
      break
    } else if (fd.lambdan > 0) {
      an1 = an
      bn1 = lambda.n
    } else {
      an1 = lambda.n
      bn1 = bn
    }
    
    vres = c(n + 1, an, lambda.n, bn, fd.lambdan, bn - an)
    if (details) 
      if (is.null(mres)) 
        mres = vres
       else 
        mres = rbind(mres, vres)

    if ((bn - an) <= uncert) 
      break
    
    n = n + 1
    an = an1
    bn = bn1
  }
  
  if (details) {
    mres
    colnames(mres) = names
    rownames(mres) = NULL
    dfres = as.data.frame(mres)
  } else {
    mres
    names(vres) = names
    dfres = as.data.frame(vres)
  }
  
  return(dfres)
}



#' Localiza el mínimo de una función dado un punto inicial con el método de las 
#' 
#' @param funx Expresión analítica de la ecuación que queremos minimizar de N dimensiones
#' @param vx.ini Coordenadas iniciales para empezar la búsqueda de N dimensiones
#' @param eps Intervalo de cambio para la parada
#' @param max.iter Número máximo de iteraciones a realizar
#' @param min Valor mínimo de coordenada por donde moverse
#' @param max Valor máximo de coordenada por donde moverse
#' @return El detalle de la ejecución del algoritmo como data.frame
#' @export
opt_ciclicas = function(funx,vx.ini,eps=0.01,max.iter=1000,detail=T,min=-1000,max=1000) {
  if (eps <= 0)
    stop("epsilon should be greater than 0")
  
  mres = NULL
  k = 1
  vyi = vx.ini
  vxki = vx.ini
  ndimension = length(vx.ini)
  m.vectoresD = diag(x=1,nrow=ndimension,ncol=ndimension)
  names = c("k","j",paste0("vyi",1:ndimension),"lambda",paste0("vyimas1",1:ndimension))
  
  repeat {
    j = 1

    for (j in 1:ndimension) {
      vDj = m.vectoresD[j,]
      ffunxj = function(lambda) funx(vyi+lambda*vDj)
      sol.uni = optimize(ffunxj,c(min,max),maximum = F)
      vyi1 = vyi + sol.uni$minimum * vDj

      vres = c(k,j,vyi,sol.uni$minimum,vyi1)
      if (detail) 
        if (is.null(mres)) 
          mres = vres
         else 
          mres = rbind(mres,vres) 
      
      vyi = vyi1
    }
    
    vxki1 = vyi1 
    norma.l2 = sqrt(sum((vxki1-vxki)^2))  
    if ((norma.l2<eps) | (k>=max.iter)) {
      break
    } else {
      k = k+1
      vyi = vxki1
    }
  } 
  
  if (detail) {
    colnames(mres) = names
    rownames(mres) = NULL
    dfres = as.data.frame(mres)
  } else {
    names(vres) = names
    dfres = as.data.frame(vres)
  }

  return(dfres)
}



#' Localiza el mínimo de una función de N dimensiones a partir de un punto inicial usando el método de pasos descendentes
#' 
#' @param funx Expresión analítica de la ecuación que queremos minimizar, de N dimensiones
#' @param gradfunx Gradiente de la función a minimizar
#' @param vx.ini Límite superior del intervalo de búsqueda
#' @param eps Intervalo de cambio para la parada
#' @param max.iter Número máximo de iteraciones para encontrar la solución
#' @param details Indica si se desea ver el detalle de la ejecución o no
#' @param min Cota inferior de búsqueda
#' @param max Cota superior de búsqueda
#' @return El detalle de la ejecución del algoritmo como data.frame
#' @export
opt_descendentes = function(funx,gradfunx,vx.ini,eps=0.01,max.iter=1000,details=T,min=0.00001,max=1000) {
  if (eps <= 0)
    stop("epsilon should be greater than 0")
  
  mres = NULL
  k = 1
  vxki = vx.ini
  ndimension = length(vx.ini)
  gradfxi = gradfunx(vxki)
  names = c("k",paste0("vxki",1:ndimension),"fxi",paste0("vd",1:ndimension),"lambda",paste0("vxkimas1",1:ndimension),"NormaGrad","Fin")
  
  repeat {
    vD = gradfxi
    fxi = funx(vxki)
    ffunxj = function(lambda) funx(vxki-lambda*vD)
    sol.uni = optimize(ffunxj,c(min,max),maximum = F)
    vxki1 = vxki - sol.uni$minimum * vD
    gradfxi1 = gradfunx(vxki1)
    norma.l2 = sqrt(sum((gradfxi1)^2))  
    vres = c(k,vxki,fxi,-gradfxi,sol.uni$minimum,vxki1,norma.l2,norma.l2<eps)
    
    if (details) 
      if (is.null(mres)) 
        mres = vres
       else 
        mres = rbind(mres,vres) 
    
    if ((norma.l2<eps) | (k>=max.iter)) {
      break
    } else {
      k = k+1
      vxki = vxki1      
      gradfxi = gradfunx(vxki)
    }
  }
  
  if (details) {
    colnames(mres) = names
    rownames(mres) = NULL
    dfres = as.data.frame(mres)
  } else {
    names(vres) = names
    dfres = as.data.frame(vres)
  }

  return(dfres)
}





#' Localiza el mínimo de una función en un intervalo determinado
#' 
#' @param vx.ini Vector de puntos iniciales para la búsqueda, de N dimensiones
#' @param funx Expresión analítica de la ecuación que queremos minimizarde N dimensiones
#' @param lrest.igualdad Función restrictiva de igualdad
#' @param phi.igualdad 
#' @param mu1 
#' @param beta 
#' @param eps Nivel de incertidumbre admitida
#' @param max.iter Número máximo de iteraciones a realizar
#' @param details Indica si se desea ver el detalle de la ejecución o no
#' @return El detalle de la ejecución del algoritmo como data.frame
#' @export
opt_penalizaciones = function(vx.ini,funx,lrest.igualdad,
                                     phi.igualdad=function(x) abs(x)^2,mu1 = 0.1,beta=10, 
                                     eps=0.01,max.iter = 1000,details=T) {
  if (eps <= 0)
    stop("epsilon should be greater than 0")
  
  mres = NULL
  k = 1
  vxki = vx.ini
  ndimension = length(vx.ini)
  names = c("k","muk",paste0("vxkmas1",1:ndimension),"fvxkmas1","fpvxkmas1","h2vxkmas1","mukporh2vxkmas1")
  
  f.alfa = function(vx, muk) {
    pri = funx(vx)
    pri = pri + muk * phi.igualdad(lrest.igualdad[[1]](vx))
    return(pri)
  }
  
  muk = mu1

  repeat {
    fmu.temp = function(vxki)
      f.alfa(vxki, muk)
    
    solk =  optim(vxki, fmu.temp)
    vxki1 = solk$par
    fpvxki1 = solk$value
    fvxki1 = funx(vxki1)
    alfavki1 = lrest.igualdad[[1]](vxki1) ^ 2
    muk.alfavki1 = muk * alfavki1
    vres = c(k,muk,vxki1,fvxki1,fpvxki1,alfavki1,muk.alfavki1)
    
    if (details) 
      if (is.null(mres)) 
        mres = vres
       else 
        mres = rbind(mres, vres)

    if ((muk.alfavki1 < eps) | (k >= max.iter)) {
      break
    } else {
      k = k + 1
      vxki = vxki1
      muk = muk * beta
    }
  }
  
  if (details) {
    colnames(mres) = names
    rownames(mres) = NULL
    dfres = as.data.frame(mres)
  } else {
    names(vres) = names
    dfres = as.data.frame(vres)
  }
  
  return(dfres)
}







## ------------------------------------------------------------------------
#' Minimiza una función bidimensional. La matriz hessiana de la función debe ser invertible o dará un error
#' Se imprime la iteración final y la solución
#' Se utiliza el algoritmo de Newton multidimensional sin restricciones para obtener estos datos.
#' @param funx Expresión analítica de la ecuación que queremos minimizar, dependiente de 'x' y de 'y'
#' @param vx.ini Vector de dos dimensiones que representa el punto inicial
#' @param epsilon Valor para la condición de parada
#' @param nummax.iter número máximo de iteraciones del algoritmo
#' @param report.calculos Si es True, muestra todas las iteraciones
#' @author Andrés Doncel Ramírez
#' @return El detalle de la ejecución del algoritmo como data.frame
optmul_nores_newton = function(funx, vx.ini,
                               epsilon=0.01,nummax.iter =1000,
                               report.calculos=T) {
  mres = NULL
  k = 1
  vk = vx.ini
  vk2 = vx.ini
  ndimension = length(vx.ini)
  fx = function(x,y) eval(funx)
  
  ## Gradientes y Hessianos
  
  D1fun = deriv(funx,c("x","y"), hessian = T, func=T)
  D1grad = function(x,y) attr(D1fun(x,y),"gradient")
  
  D1hess = function(x,y) 
    matrix(as.vector(attr(D1fun(x,y),"hessian")),nrow=ndimension,ncol=ndimension)
  # Paso 3
  repeat {
    ev=fx(vk[1],vk[2])
    Grad = D1grad(vk[1],vk[2])
    invHess = solve(D1hess(vk[1],vk[2]))
    
    vk2=vk- as.vector(invHess%*%t(Grad))
    
    (dif = sqrt(sum((vk2-vk)^2)))
    (norma.l2 = sqrt(sum((Grad)^2)))
    vres = c(k, vk, vk2,ev, dif,norma.l2, (dif<epsilon)||norma.l2<epsilon)
    
    if (report.calculos) {
      if (is.null(mres)) {
        mres = vres
      } else {
        mres = rbind(mres,vres) 
      }
    }
    
    if ((dif<epsilon)||(norma.l2<epsilon) || (k>=nummax.iter)) {
      break
    } else {
      k = k+1
    }
    vk=vk2
  } # final repeat
  
  
  print("Óptimo encontrado en la iteración k")
  print(k)
  if (report.calculos) {
    colnames(mres) = c("k",paste0("vk",1:ndimension),
                       paste0("vk2",1:ndimension),"Eval f","Dif. iter", "NormaGrad","Fin")
    
    rownames(mres) = NULL
    dfres = as.data.frame(mres)
    print.data.frame(dfres[k,2:3])
  } else {
    names(vres)= c("k",paste0("vk",1:ndimension),
                   paste0("vk2",1:ndimension),"Eval f","Dif. iter","NormaGrad","Fin")
    dfres = vres
    print(dfres)
  }
  
  
  return(dfres)
  
}