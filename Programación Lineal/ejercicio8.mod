/* ejercicio8.mod */ 
/* Ejercicio 8 */ 
var x1>=0;
var x2>=0;
var x3>=0;
var x4>=0;
var y1>=0;
var y2>=0;
var y3>=0;
var y4>=0;

minimize z: (x2-x1) + (y4-y2) + (x4-x3) + (y3-y1);

s.t. r1: y1-y2 = 0;
s.t. r2: y3-y4 = 0;
s.t. r3: y3-y1 >= 0;
s.t. r4: y4-y2 >= 0;
s.t. r5: x1-x3 = 0;
s.t. r6: x2-x4 = 0;
s.t. r7: x2-x1 >= 0;
s.t. r8: x4-x3 >= 0;
s.t. r9: 2*x4-y4 = 4;
s.t. r10: x1 <= 1;
s.t. r11: y1 <= 2;
s.t. r12: x4 >= 2;
s.t. r13: y4 >= 3;