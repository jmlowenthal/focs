datatype 'a tree = Lf
	| Br of 'a * 'a tree * 'a tree
	
exception Empty;
fun shrink(Lf) = raise Empty
	| shrink(Br(v, Lf, Lf)) = Lf
	| shrink(Br(v, Br(w, r, l), Lf)) = Br(w, r, l)
	| shrink(Br(v, Lf, Br(w, r, l))) = Br(w, r, l)
	| shrink(Br(v, Br(w, ll, lr), r)) = Br(w, r, shrink(Br(w, ll, lr)));
	
(* Exercise 1 *)
fun insert(a : string, Lf) = Br(a, Lf, Lf)
	| insert(a, Br(b, l, r)) =
		if a = b then Br(b, l, r)
		else if a < b then Br(b, insert(a, l), r)
		else Br(b, l, insert(a, r));
		
fun member(a : string, Lf) = false
	| member(a, Br(b, l, r)) =
		if (a = b) then true
		else if (a < b) then member(a, l)
		else member(a, r);
		
(* Exercise 2 *)
fun union(Lf, t) = t
	| union(t, Lf) = t
	| union(t, Br(v : string, l, r)) = union(insert(v, t), union(l, r));
	
(* Exercise 3 *)
fun inter(t, Lf) = Lf
	| inter(Lf, t) = Lf
	| inter(t, Br(v : string, l, r)) =
		if (member(v, t)) then union(insert(v, inter(t, l)), inter(t, r))
		else union(inter(t, l), inter(t, r));
		
(* Exercise 4 *)
fun remove(v : string, Lf) = Lf
	| remove(v, Br(w, l, r)) = 
		if v = w then shrink(Br(v, remove(v, l), remove(v, r)))
		else Br(w, remove(v, l), remove(v, r));