fun choose_(0, _) = [[]]
	| choose_(_, []) = [[]]
	| choose_(k, h::t) = map(fn y => h :: y)(choose_(k - 1, t)) @ choose_(k, t);
	
fun filter([], f) = []
	| filter(h::t, f) = if f(h) then h::filter(t, f) else filter(t, f);
	
fun choose(n, t) = filter(choose_(n, t), fn y => length y = n);