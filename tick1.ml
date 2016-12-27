fun evalquad(a : real, b : real, c : real, x : real) = a * x * x + b * x + c : real;

(* Recursive factorial *)
fun facr(n : int) =
	if (n < 1) then 1
	else n * facr(n - 1);

(* "Iterative" factorial *)
fun faci_(n : int, result : int) =
	if (n < 1) then result
	else faci_(n - 1, result * n);

fun faci(n : int) = faci_(n, 1);

(* Sum of 1/(2^(n-1)) *)
fun sumt_(x : real, n : int) =
	if (n < 1) then 0.0
	else x + sumt_(x / 2.0, n - 1) : real;

fun sumt(n : int) = sumt_(1.0, n);