datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

exception Empty;

(* Exercise 1 *)
fun leftmost(Lf) = raise Empty
	| leftmost(Br(v, Lf, _)) = v
	| leftmost(Br(v, l, _)) = leftmost(l);

fun popleftmost(Lf) = Lf
	| popleftmost(Br(v, Lf, r)) = r
	| popleftmost(Br(v, l, _)) = popleftmost(l);
	
fun pop(Lf) = Lf
	| pop(Br(v, Lf, Lf)) = Lf
	| pop(Br(v, st, Lf)) = st
	| pop(Br(v, Lf, st)) = st
	| pop(Br(v, l, r)) = Br(leftmost(r), l, popleftmost(r));
	
fun remove(x, Lf) = Lf
	| remove(x, Br(v, l, r)) =
		if x < v then Br(v, remove(x, l), r)
		else if x > v then Br(v, l, remove(x, r))
		else pop(Br(v, l, r));
		
(* Exercise 2 *)
fun shrink(Lf) = raise Empty
	| shrink(Br(v, Lf, Lf)) = Lf
	| shrink(Br(v, Br(w, r, l), Lf)) = Br(w, r, l)
	| shrink(Br(v, Lf, Br(w, r, l))) = Br(w, r, l)
	| shrink(Br(v, Br(w, ll, lr), r)) = Br(w, r, shrink(Br(w, ll, lr)));
	
(* Exercise 4 *)
fun map2 f [] = []
	| map2 f ([]::tl) = [] :: (map2 f tl)
	| map2 f ((h::[])::tl) = (f(h)::[]) :: (map2 f tl)
	| map2 f ((h::t)::tl) =
		let val (v::_) = (map2 f [t]) in
			(f(h)::v) :: (map2 f tl)
		end;
		
(* Exercise 5 *)
datatype 'a ttree = Twig of 'a
	| Br of 'a * 'a ttree * 'a ttree;
	
(* Exercise 5.1 *)
exception Backtrack;
fun find_path_ p (Twig(a)) sum =
		if p(sum + a) then a :: []
		else raise Backtrack
	| find_path_ p (Br(a, l, r)) sum =
		a :: (find_path_ p l (sum + a))
		handle Backtrack => a :: (find_path_ p r (sum + a));
		
fun find_path p t = find_path_ p t 0;

(* Exercise 5.2 *)
fun sum([]) = 0
	| sum(h::t) = h + sum(t);
	
fun filter p [] = []
	| filter p (h::t) =
		if p(h) then h :: (filter p t)
		else filter p t;

fun all_paths_(Twig(a)) = (a::[])::[]
	| all_paths_(Br(a, l, r)) = map (fn l => a::l) ((all_paths_ l) @ (all_paths_ r));
	
fun all_paths p t = filter(fn l => p(sum(l)))(all_paths_(t));

(* Exercise 5.3 *)
datatype 'a stream = Nil | Item of 'a * ('a -> 'a stream);
datatype route = Left | Right;

fun next Nil = Nil
	| next (Item(a, f)) = f(a);	
		
fun nextPath (Twig(v)) _ true = raise Backtrack
	| nextPath (Twig(v)) _ false = []
	| nextPath (Br(v, l, r)) _ false = Left :: (nextPath l [] false)
	| nextPath (Br(v, l, r)) (d::ds) true =
		if d = Left then (
			Left :: (nextPath l ds true)
				handle Backtrack => Right :: (nextPath r [] false)
		)
		else Right :: (nextPath r ds true)
	| nextPath (Br(v, l, r)) [] true = nextPath (Br(v, l, r)) [] false;

fun follow _ (Twig(a)) = a::[]
	| follow [] (Br(v, _, _)) = v::[]
	| follow (h::tl) (Br(v, l, r)) =
		v :: (if h = Left then follow tl l else follow tl r);

fun all_pathsq_ p t (v, r) =
	(
		let val route = nextPath t r true
			val values = follow route t
		in
			if p(sum(values)) then Item((values, route), all_pathsq_ p t)
			else all_pathsq_ p t ([], route)
		end
	) handle Backtrack => Nil;

fun all_pathsq p t = all_pathsq_ p t ([], []);

(* Exercise 7 *)
datatype 'a lazytree = Lf | Br of 'a * (unit -> 'a lazytree) * (unit -> 'a lazytree);
datatype 'a lazylist = Nil | Cons of 'a * (unit -> 'a lazylist);
datatype 'a queue = Q of 'a list * 'a list;

fun next Nil = Nil
	| next (Cons(v, f)) = f();

fun norm(Q([], t)) = Q(rev t, [])
	| norm q = q;

fun qnull(Q([], [])) = true
	| qnull _ = false;
	
fun push(Q(h, t), x) = norm(Q(h, x::t));

fun pop(Q(x::h, t)) = norm(Q(h, t));

fun top(Q(x::h, t)) = x;

fun nextBreadth (Q([], [])) = Nil
	| nextBreadth (Q([], t)) = nextBreadth(norm(Q([], t)))
	| nextBreadth q =
		case top(q) of Lf => nextBreadth(pop(q))
			| Br(a, l, r) => Cons(a, fn() => nextBreadth(push(push(pop(q), l()), r())));
	
fun breadthList t = nextBreadth(Q([t], []));

fun incTree(k) = Br(k, (fn() => incTree(2*k)), (fn() => incTree(2*k + 1)));