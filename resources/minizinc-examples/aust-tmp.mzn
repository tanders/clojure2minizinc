int: nc = 3;

var 1..nc: wa; var 1..nc: nt; var 1..nc: sa; var 1..nc: q;
var 1..nc: nsw; var 1..nc: v; var 1..nc: t;

constraint wa != nt; 
constraint wa != sa; 
constraint nt != sa; 
constraint nt != q; 
constraint sa != q; 
constraint sa != nsw; 
constraint sa != v; 
constraint q != nsw; 
constraint nsw != v; 
solve satisfy;


output ["wa=", show(wa), "	 nt=", show(nt),
"	 sa=", show(sa), "\n", "q=", show(q),
"	 nsw=", show(nsw), "	 v=", show(v), "
",
"t=", show(t), "
"]; 