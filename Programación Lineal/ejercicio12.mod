/* ejercicio12.mod */ 
/* Ejercicio 12 */ 

var l>=2;
var c>=2;
var i>=2;

maximize z: i;

s.t. r1: l + c + i = 10;
s.t. r2: c - (l + i) >= 0;
