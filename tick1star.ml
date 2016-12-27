(* Recursive approximation of e *)
fun eapprox_(n : int, x : real, t : int) =
	if (n < 1) then 0.0
	else x + eapprox_(n - 1, x / real(t), t + 1);

fun eapprox(n : int) = eapprox_(n, 1.0, 1);

(* Recursive approximation of e^z *)
fun exp_(z : real, n : int, x : real, t : int) = 
	if (n < 1) then 0.0
	else x + exp_(z, n - 1, x * z / real(t), t + 1);
	
fun exp(z: real, n : int) = exp_(z, n, 1.0, 1);

(* Greatest common divisor *)
fun gcd(a : int, b : int) = 
	if (a < 1 orelse b < 1) then 0
	else if (a = b) then a
	else if (a mod 2 = 0 andalso b mod 2 = 0) then 2 * gcd(a div 2, b div 2)
	else if (a mod 2 = 1 andalso b mod 2 = 0) then gcd(a, b div 2)
	else if (a mod 2 = 0 andalso b mod 2 = 1) then gcd(a div 2, b)
	else gcd(b, abs((a - b) div 2));