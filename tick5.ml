type colour = int * int * int
type xy = int * int
datatype image = Image of xy * colour array array;

fun image ((w, h) : xy) (c : colour) =
	Image((w, h), Array.tabulate(h, fn i => Array.array(w, c)));
	
fun size (Image(dim, c)) = dim;

fun drawPixel (Image((w, h), img)) (c : colour) ((x, y) : xy) =
	if (0 <= x andalso x < w andalso 0 <= y andalso y < h) then
		Array.update(Array.sub(img, y), x, c)
	else ();

fun format4 i = StringCvt.padLeft #" " 4 (Int.toString i);
	
fun toPPM (Image((w, h), img)) filename =
	let val oc = TextIO.openOut filename
	in
		TextIO.output(oc, "P3\n" ^ Int.toString w ^ " " ^ Int.toString h ^ "\n255\n");
		TextIO.output(oc, Array.foldl (fn (row, prev) =>
			(
				Array.foldl (fn ((r, g, b), last) =>
					last ^ format4 r ^ format4 g ^ format4 b
				) prev row
			) ^ "\n"
		) "" img);
		TextIO.closeOut oc
	end;
	
fun drawHoriz _ _ _ 0 = ()
	| drawHoriz (source as Image((w, h), img)) c (x, y) l = (
		Array.update(Array.sub(img, y), x, c);
		drawHoriz source c (x + 1, y) (l - 1)
	);
	
fun drawVert _ _ _ 0 = ()
	| drawVert (source as Image((w, h), img)) c (x, y) l = (
		Array.update(Array.sub(img, y), x, c);
		drawVert source c (x, y + 1) (l - 1)
	);
	
fun drawDiag _ _ _ 0 = ()
	| drawDiag (source as Image((w, h), img)) c (x, y) l = (
		Array.update(Array.sub(img, y), x, c);
		drawDiag source c (x + 1, y + 1) (l - 1)
	);
	
fun drawLine (source as Image((w, h), img)) c (x0, y0) (x1, y1) =
		let val dx = Int.abs(x1 - x0)
			val dy = Int.abs(y1 - y0)
			val sx = if x0 < x1 then 1 else 0 - 1
			val sy = if y0 < y1 then 1 else 0 - 1
			fun line x y err = (
				Array.update(Array.sub(img, y), x, c);
				if (x = x1 andalso y = y1) then ()
				else
					let val e2 = 2 * err
					in
						if (e2 > 0 - dy) then
							if e2 < dx then
								line (x + sx) (y + sy) (err - dy + dx)
							else line (x + sx) y (err - dy)
						else
							if e2 < dx then
								line x (y + sy) (err + dx)
							else line x y err
					end;
				()
			)
		in
			line x0 y0 (dx - dy)
		end;