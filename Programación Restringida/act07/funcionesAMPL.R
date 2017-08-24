## FUNCIONES PARA USAR EN RSTUDIO-R: AMPL, GLPK, ETC.


# colocar en el fichero Rmd fuente como un chunk
# mimacbookpro15=TRUE # mimacbookpro15, macmini
# if (mimacbookpro15) {
#   ampl.dir = "/Users/calvo/Documents/maqvirvm_Win8164bits/doctrabajo/apliterminal/ampl/amplide.macosx64/"  
#   glpsol.dir = "/Users/calvo/Documents/maqvirvm_Win8164bits/doctrabajo/apliterminal/GLPK/"
# } else {
#   ampl.dir = "/Applications/apliterminal/ampl/amplide-demo/ampl/"  
#   glpsol.dir = "/Applications/apliterminal/GLPK/"
# }
#source("funcionesAMPL.R")

# glpsol.dir = "c:/optsis/glpk/" 
# ampl.dir = "c:/optsis/ampl/"

## funciones que contiene

#func_resuelve_modelo_glpk = function(fichero,...) {
  
# func_glpk_modelo = function(v_mod, 
#                             fic.mod="fglpk_modelo.mod", 
#                             glpsol.install.dir = glpsol.dir, # acabado en /
#                             glpsol.install.exe = "glpsol.exe",
#                             dir.actual = getwd(), 
#                             muestra.salida.ext = FALSE ) {
  
#execute <- function(programme, subject.spec = "", intern = FALSE, wait = FALSE){ 

#command <- function(comando, intern = TRUE, wait = FALSE){ 

# func_ampl_moddatrun = function(v_mod, 
#                                v_dat, 
#                                v_run, 
#                                fic.mod="fampl_modelo.mod", 
#                                fic.dat="fampl_datos.dat", 
#                                fic.run="fampl_run.run", 
#                                ampl.install.dir = ampl.dir, # acabado en / 
#                                ampl.install.exe = "ampl.exe", 
#                                dir.actual = getwd() ) {


# func_ampl_moddatrun_mac = function(v_mod,
#                                    v_dat,
#                                    v_run,
#                                    fic.mod="fampl_modelo.mod",
#                                    fic.dat="fampl_datos.dat",
#                                    fic.run="fampl_run.run",
#                                    ampl.install.dir = ampl.dir,  # acabado en /
#                                    ampl.install.exe = "./ampl",
#                                    dir.actual = getwd()
# ) {
  

library(Rglpk)

## FUNCIÓN PARA RESOLVER EL MODELO GLPK 
func_resuelve_modelo_glpk = function(fichero,...) {
  proy_glpk = Rglpk_read_file(file = fichero,type = "MathProg",verbose = F) 
  
  salida = Rglpk_solve_LP(proy_glpk$objective, 
                          proy_glpk$constraints[[1]], 
                          proy_glpk$constraints[[2]], 
                          proy_glpk$constraints[[3]], 
                          bounds = proy_glpk$bounds, 
                          types = proy_glpk$types, 
                          max = proy_glpk$maximum,...) 
  salida 
}

func_glpk_modelo = function(v_mod, 
                            fic.mod="fglpk_modelo.mod", 
                            glpsol.install.dir = glpsol.dir, # acabado en /
                            glpsol.install.exe = "glpsol.exe",
                            dir.actual = getwd(), 
                            muestra.salida.ext = FALSE ) {
  cat(v_mod,file=fic.mod)
  comando = paste0(glpsol.install.dir,glpsol.install.exe," ", 
                   " --glp ",
                   " -m ",fic.mod, 
                   " -o ","salida.txt" )
  # en windows: c:/optsis/glpk/glpsol.exe --glp -m ejglpkb.mod -o ej.txt 
  # en mac: comando = "glpsol --glp -m asignacion.mod -o asignacion_salida.txt" 
  #system(comando)
  cat(system(comando, intern = TRUE), sep = '\n')
  if (muestra.salida.ext) { readLines("salida.txt") }
}

execute <- function(programme, subject.spec = "", intern = FALSE, wait = FALSE){ if(!identical(subject.spec, "")){
  subject.spec <- paste0(" ", subject.spec)
} #put space before the subject if it exists 
  
  system(paste0("cmd.exe /c ", programme, subject.spec), intern = intern, wait = wait) 
}


command <- function(comando, intern = TRUE, wait = FALSE){ 
  system(paste("cmd.exe /c", comando), intern = T, wait = wait) 
}

func_ampl_moddatrun = function(v_mod, 
                               v_dat, 
                               v_run, 
                               fic.mod="fampl_modelo.mod", 
                               fic.dat="fampl_datos.dat", 
                               fic.run="fampl_run.run", 
                               ampl.install.dir = ampl.dir, # acabado en / 
                               ampl.install.exe = "ampl.exe", 
                               dir.actual = getwd() ) {
  cat(v_mod,file=fic.mod) 
  cat(v_dat,file=fic.dat) 
  cat(v_run,file=fic.run)
  # fic.ampl.mod = paste0("'",dir.actual,"/",fic.mod,"'") 
  # fic.ampl.dat = paste0("'",dir.actual,"/",ficherodat,"'") 
  # fic.ampl.run = paste0("'",dir.actual,"/",ficherorun,"'")
  
  tmp1=file.copy(fic.mod,ampl.install.dir,overwrite = T)
  tmp1=file.copy(fic.dat,ampl.install.dir,overwrite = T)
  tmp1=file.copy(fic.run,ampl.install.dir,overwrite = T)
  
  comando = paste("cd ",ampl.install.dir," && ", ampl.install.exe, fic.run, sep= " ")
  #setwd(ampl.install.dir)
  #cat(system(comando, intern = TRUE), sep = '\n')
  #command(comando)
  cat(command(comando),sep = "\n")
  
  #cat(system(comando, intern = TRUE), sep = '\n')
  #setwd(dir.actual)
  tmp1=file.remove(paste0(ampl.install.dir,fic.mod))
  tmp1=file.remove(paste0(ampl.install.dir,fic.dat))
  tmp1=file.remove(paste0(ampl.install.dir,fic.run))
  
}



func_ampl_moddatrun_mac = function(v_mod,
                               v_dat,
                               v_run,
                               fic.mod="fampl_modelo.mod",
                               fic.dat="fampl_datos.dat",
                               fic.run="fampl_run.run",
                               ampl.install.dir = ampl.dir,  # acabado en /
                               ampl.install.exe = "./ampl",
                               dir.actual = getwd()
) {
  
  cat(v_mod,file=fic.mod)
  cat(v_dat,file=fic.dat)
  cat(v_run,file=fic.run)
  
  
  # fic.ampl.mod = paste0("'",dir.actual,"/",fic.mod,"'")
  # fic.ampl.dat = paste0("'",dir.actual,"/",ficherodat,"'")
  # fic.ampl.run = paste0("'",dir.actual,"/",ficherorun,"'")
  
  tmp1=file.copy(fic.mod,ampl.install.dir,overwrite = T)
  tmp1=file.copy(fic.dat,ampl.install.dir,overwrite = T)
  tmp1=file.copy(fic.run,ampl.install.dir,overwrite = T)
  
  comando = paste("cd ",ampl.install.dir," ; ",
                  ampl.install.exe,
                  fic.run,
                  sep= " ")
  #cat(system(comando, intern = TRUE), sep = '\n')
  salida = system(comando, intern = TRUE)
  salida = chartr(""," ",salida)  # sub no va bien
  cat(salida, sep = '\n')
  ##
  
  tmp1=file.remove(paste0(ampl.install.dir,fic.mod))
  tmp1=file.remove(paste0(ampl.install.dir,fic.dat))
  tmp1=file.remove(paste0(ampl.install.dir,fic.run))
  
}

# func_ampl_moddatrun(v_mod = modelo,
#                     v_dat = data,
#                     v_run = run)

func_ampl_copia_character_a_fichero = function(vchar,fic,directorio="",copia_actual=FALSE) {
  
  if (directorio=="") {
    cat(vchar,file=fic)
  } else {
    cat(vchar,file=paste0(directorio,fic))
    if (copia_actual) {
      cat(vchar,file=fic)
    }
  } 
  #tmp1=file.copy(fic,directorio,overwrite = T)
  
}

func_ampl_copia_de_directorio = function(fic,directorio,dir.destino = getwd()) {
  tmp1 = file.copy(paste0(directorio,fic),dir.destino,overwrite = T)
}

func_ampl_leer_fichero_OUT = function(fic) {
  read.table(fic,skip = 1,header=TRUE)
  
}

func_ampl_character_limpia = function(salida) {
  chartr(""," ",salida)  # sub no va bien
}
