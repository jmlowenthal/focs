(* Exercise 1 *)
fun nfold f 0 = (fn x => x)
	| nfold f n = (fn x => f(nfold(f)(n - 1)(x)));
	
fun sum a b = nfold(fn x => x + 1)(b)(a);
fun product a b = nfold(fn x => x + a)(b)(0);
fun power a b =  nfold(fn x => x * a)(b)(1);

(* Exercise 2 *)
datatype 'a stream = Cons of 'a * (unit -> 'a stream);
fun from k = Cons(k, fn() => from(k + 1));

fun nth(Cons(k, f), 1) = k
	| nth(Cons(k, f), n) = nth(f(), n - 1);
	
(* Exercise 3 *)
fun square(a) = Cons(a * a, fn() => square(a + 1));
val squares = square(1);

(* Exercise 4 *)
fun map2 f (Cons(x, xf)) (Cons(y, yf)) = Cons((f x y), (fn() => map2 f (xf()) (yf())));

fun get(Cons(k, f), 0) = []
	| get(Cons(k, f), n) = k :: get(f(), n - 1);