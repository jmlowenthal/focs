exception EmptyList;
exception OutOfRange;

fun last([]) = raise EmptyList
	| last(x::[]) = x
	| last(x::xs) = last(xs);
	
(* Space: O(n), time: O(n) *)
fun butlast([]) = raise EmptyList
	| butlast(x::[]) = []
	| butlast(x::xs) = x::butlast(xs);
	
fun nth([], n) = raise OutOfRange
	| nth(x::xs, 0) = x
	| nth(x::xs, n) = nth(xs, n - 1);