
param nuTrabajadores;
param nuTareas;
set TAREAS := 1..nuTareas;
set TRABAJADORES := 1..nuTrabajadores;
param coste {TRABAJADORES};
param cualificacion {TAREAS,TRABAJADORES} binary;
var Quien {TAREAS,TRABAJADORES} binary;

minimize z: 
   sum {i in TAREAS,j in TRABAJADORES} coste[j]*Quien[i,j];
s.t. r1 {j in TRABAJADORES}:
   sum {i in TAREAS} cualificacion[i,j]*Quien[i,j] <= 1;
s.t. r2 {i in TAREAS}:
   sum {j in TRABAJADORES} cualificacion[i,j]*Quien[i,j] = 1;

