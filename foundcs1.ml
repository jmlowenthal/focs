fun interleave(x, []) = [[x]]
	| interleave(x, h::t) = (x::h::t) :: map(fn y => h::y)(interleave(x, t));
	
fun concat([]) = []
	| concat(h::t) = h @ concat(t);
	
fun permutation([]) = [[]]
	| permutation(h::[]) = [[h]]
	| permutation(h::t) = concat(map(fn y => interleave(h, y))(permutation(t)));
	
fun power(b, 0) = 1
	| power(b, n) = 
		if n mod 2 = 0 then power(b*b, n div 2)
		else b * power(b, n - 1);
		
fun ipower_(b, 0, c) = c
	| ipower_(b, e, c) = ipower_(b, e - 1, c * b);
fun ipower(b, e) = ipower_(b, e, 1);

(* 1 *)
fun listsum([]) = 0
	| listsum(h::t) = h + listsum(t);
	
(* 2 *)
fun ilistsum_([], s) = s
	| ilistsum_(h::t, s) = ilistsum_(t, s + h);
fun ilistsum([]) = 0
	| ilistsum(h::t) = ilistsum_(h::t, 0);

(* 3 *)
exception EmptyList;
fun lastelement([]) = raise EmptyList
	| lastelement(h::[]) = h
	| lastelement(h::t) = lastelement(t);
	
(* 4 *)
fun evenelements_([], x) = []
	| evenelements_(h::t, x) =
		if x then h :: evenelements_(t, not(x))
		else evenelements_(t, not(x));
fun evenelements([]) = []
	| evenelements(h::t) = evenelements_(h::t, false);