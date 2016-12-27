(* Exercise 1 *)
datatype 'a fan = Wave of 'a * ('a fan) list;

fun flip (Wave(v, l)) = Wave(v, map flip (rev l));

fun paint f (Wave(v, l)) = Wave(f v, map (paint f) l);

fun same_shape (Wave(a, [])) (Wave(b, [])) = true
	| same_shape (Wave(av, ah::al)) (Wave(bv, bh::bl)) =
		(same_shape ah bh) andalso (same_shape (Wave(av, al)) (Wave(bv, bl)))
	| same_shape _ _ = false;
	
fun foldr f ([], e) = e
 | foldr f (x::xs, e) = f(x, foldr f (xs,e));
fun paper (Wave(x,fs), q) = foldr paper (fs, q+1);

(* Exercise 2 *)

(* coefficent : int * variable powers list : int list *)
type Term = int * int list;
type Expression = Term list;

fun listeq [] [] = true
	| listeq (x::xs) (y::ys) = (x = y) andalso (listeq xs ys)
	| listeq _ _ = false;

fun add ((a, ns) : Term) ((b, ms) : Term) : Expression =
	if listeq ns ms then [(a + b, ns)]
	else [(a, ns), (b, ms)];
	
(* Exercise 3 *)
datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;
datatype 'a queue = Q of 'a list * 'a list;

fun norm (Q([], t)) = Q(rev t, [])
	| norm q = q;
	
fun qnull (Q([], [])) = true
	| qnull _ = false;

fun enq (Q(h, t)) x = norm (Q(h, x::t));
fun deq (Q(x::h, t)) = norm (Q(h, t));
fun qhd (Q(x::h, t)) = x;

fun breadth_ q =
	if qnull q then []
	else
		let val hd = qhd q
			val tl = deq q in
			(
				let val Br(v, l, r) = hd in
					v :: breadth_ (enq (enq tl l) r)
				end
				handle MatchException => breadth_ tl
			)
		end;
	
fun breadth t = breadth_ (Q([t], []));

(* Exercise 5 *)
fun power x n =
	let val r = ref x
		val i = ref n
	in
		(
			while !i > 1 do (r := !r * x; i := !i - 1);
			!r
		)
	end;
	
(* Exercise 6 *)
fun identity n =
	Array.tabulate(n,
		fn i => Array.tabulate(n,
			fn j => if i = j then 1 else 0
		)
	);
	
fun transpose arr =
	Array.tabulate(
		Array.length(Array.sub(arr, 0)),
		fn m => Array.tabulate(
			Array.length(arr),
			fn n => Array.sub(Array.sub(arr, n), m)
		)
	)