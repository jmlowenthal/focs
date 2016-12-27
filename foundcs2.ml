(*  *)
type 'a queue = 'a list * 'a list;

exception Empty;

fun normalise(([], y) : 'a queue) = (rev(y), []) : 'a queue
	| normalise ((a, b)) = (a, b) : 'a queue;

fun size((x, y) : 'a queue) = List.length(x) + List.length(y);

fun push(x, (a, b) : 'a queue) = (a, x::b) : 'a queue;

fun pop(([], []) : 'a queue) = raise Empty
	| pop(([], b) : 'a queue) = pop(normalise(([], b)))
	| pop((h::t, b) : 'a queue) = normalise((t, b));
	
fun top(([], []) : 'a queue) = raise Empty
	| top(([], b) : 'a queue) = top(normalise(([], b)))
	| top((h::t, b) : 'a queue) = h;
	
(* Exercise 4 *)
fun min([]) = raise Empty
	| min(h::[]) = h
	| min(h::t) = 
		let val n = min(t) in
			if (h < n) then h
			else n
		end;
			
fun remove(x, []) = []
	| remove(x, h::t) = if (h = x) then t else h::remove(x, t);

fun selsort([]) = []
	| selsort(l : int list) =
		let val m = min(l) in
			m :: selsort(remove(m, l))
		end;
		
(* Exercise 7 *)
datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

fun sum(Lf) = 0
	| sum(Br(v, l, r)) = v + sum(l) + sum(r);