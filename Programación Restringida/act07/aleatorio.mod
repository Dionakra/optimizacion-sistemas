
param nvar;
param nres = 15;
set VARIABLES:= 1..nvar;
set RESTRICCIONES := 1..nres;

param c {VARIABLES} := trunc(Uniform(-10,10));

param A {RESTRICCIONES,VARIABLES} := trunc(Uniform(-20,20));
param b {RESTRICCIONES} := trunc(Uniform(10,100));

var x {VARIABLES} >=0, <= 10;

maximize z: 
   sum {i in VARIABLES} c[i]*x[i];

s.t. rest {j in RESTRICCIONES}:
   sum {i in VARIABLES} A[j,i]*x[i] <= b[j];
