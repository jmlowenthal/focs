datatype 'a tree = Lf
	| Br of 'a * 'a tree * 'a tree

(* Exercise 1 *)
fun extend(v, Lf) = Br(v, Lf, Lf)
	| extend(v, Br(w, l, r)) = Br(v, extend(w, r), l);
	
fun arrayoflist([]) = Lf
	| arrayoflist(h::t) = extend(h, arrayoflist(t));
	
(* Exercise 2 *)
exception Empty;
fun shrink(Lf) = raise Empty
	| shrink(Br(v, Lf, Lf)) = Lf
	| shrink(Br(v, Br(w, r, l), Lf)) = Br(w, r, l)
	| shrink(Br(v, Lf, Br(w, r, l))) = Br(w, r, l)
	| shrink(Br(v, Br(w, ll, lr), r)) = Br(w, r, shrink(Br(w, ll, lr)));
	
fun listofarray(Lf) = []
	| listofarray(Br(v, l, r)) = v :: listofarray(shrink(Br(v, l, r)));
	
(* Exercise 3 *)
fun getSubsOfEvens_(Lf, _) = []
	| getSubsOfEvens_(Br(v, l, r), x) =
		if v mod 2 = 0 then x :: getSubsOfEvens_(shrink(Br(v, l, r)), x + 1)
		else getSubsOfEvens_(shrink(Br(v, l, r)), x + 1);
		
fun getSubsOfEvens(t) = getSubsOfEvens_(t, 1);