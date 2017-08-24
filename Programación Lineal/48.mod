/* ejercicio8.mod */ 
/* Ejercicio 8 */ 
var x1 binary;
var x2 binary;
var x3 binary;
var x4 binary;
var x5 binary;
var x6 binary;


minimize z: x1 + x2 + x3 + x4 + x5 + x6;

s.t. r1: x1-x4 >= 1;
s.t. r2: x1 + x3 + x4 + x6 >=1;
s.t. r3: x1 + x2 + x5 >= 1;
s.t. r4: x2 >= 1;
s.t. r5: x2 + x3 + x5 + x6 >= 1;
s.t. r6: x3 + x4 >= 1;
s.t. r7: x1 + x3 + x4 + x5 >= 5;