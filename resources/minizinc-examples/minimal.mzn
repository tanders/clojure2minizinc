var -3..3: a;
var -3..3: b;
constraint a != b;
solve satisfy;
output ["{", " :a ", show(a), " :b ", show(b), "}\n"];
