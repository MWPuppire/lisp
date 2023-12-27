; numeric functions
(def! // (fn* (num den) (trunc (/ num den))))
(def! <= (fn* (x y) (not (< y x))))
(def! > (fn* (x y) (< y x)))
(def! >= (fn* (x y) (not (< x y))))
(def! sign (fn* (num) (if (< num 0) -1 1)))
(def! round (fn* (num) (
	trunc (+
		num
		(* 0.5 (sign num))
	)
)))
(def! floor (fn* (num) (
	if (= num (trunc num))
		num
		(round (- num 0.5))
)))
(def! ceil (fn* (num) (
	if (= num (trunc num))
		num
		(round (+ num 0.5))
)))

; boolean functions
(def! bool (fn* (bool) (if bool true false)))
(def! not (fn* (bool) (if bool false true)))
(def! and (fn* (&args) (
	bool (foldr (fn* (acc x) (
		if acc (bool x) false
	)) true args)
)))
(def! or (fn* (&args) (
	bool (foldr (fn* (acc x) (
		if acc true (bool x)
	)) false args)
)))

; type checking functions
(def! list? (fn* (x) (= (typeof x) "list")))
(def! atom? (fn* (x) (= (typeof x) "atom")))
(def! macro? (fn* (x) (= (typeof x) "macro")))
(def! symbol? (fn* (x) (= (typeof x) "symbol")))
(def! vector? (fn* (x) (= (typeof x) "vector")))
(def! keyword? (fn* (x) (= (typeof x) "keyword")))
(def! fn? (fn* (x) (= (typeof x) "fn")))
(def! map? (fn* (x) (= (typeof x) "map")))
(def! macro? (fn* (x) (= (typeof x) "string")))
(def! macro? (fn* (x) (= (typeof x) "number")))
(def! sequential? (fn* (x) (
	or (list? x) (vector? x)
)))
(def! nil? (fn* (x) (= x nil)))
(def! true? (fn* (x) (= x true)))
(def! false? (fn* (x) (= x false)))

; atomic functions
(def! swap! (fn* (atom fn &args) (
	reset! atom (apply fn @atom args)
)))

; list functions
(def! list (fn* (&args) args))
(def! empty? (fn* (ls) (
	; can't use `or`, since `or` uses `reduce` which uses `empty?`
	if (= ls ())
		true
		(if (= ls "")
			true
			(if (= ls [])
				true
				(nil? ls)
			)
		)
)))
(def! count (fn* (ls) (
	if (empty? ls)
		0
		(+ 1 (count (rest ls)))
)))
(def! cons (fn* (item ls) (apply list item ls)))
(def! concat (fn* (&lists) (
	foldr (fn* (acc list) (
		foldr (fn* (acc item) (cons item acc)) acc (rev list)
	)) () (rev lists)
)))
(def! nth (fn* (ls n) (
	if (empty? ls)
		(throw (str "index " n " out of range"))
		(if (= n 0)
			(first ls)
			(nth (rest ls) (- n 1))
		)
)))
(def! head first)
(def! tail rest)
(def! map (fn* (transformer ls) (
	if (empty? ls)
		()
		(cons (transformer (first ls)) (map transformer (rest ls)))
)))
(def! vec (fn* (ls) (apply vector ls)))
(def! seq (fn* (ls) (
	if (empty? ls)
		nil
		(cons (first ls) (let* (tmp (seq (rest ls)))
			(if (nil? tmp) () tmp)
		))
)))
(def! conj (fn* (ls &args) (
	if (list? ls)
		(concat args ls)
		(vec (concat ls args))
)))
(def! join (fn* (strs sep) (
	if (empty? strs)
		""
		(str (reduce (fn* (acc x) (
			str acc sep x
		)) strs))
)))
(def! reduce (fn* (reducer ls) (
	let* (acc (first ls) ls (rest ls)) (
		if (empty? ls)
			acc
			(reduce reducer (
				cons (reducer acc (first ls)) (rest ls)
			))
	)
)))
(def! last (fn* (ls) (reduce (fn* (_ item) item) ls)))
(def! foldr (fn* (reducer acc ls) (
	if (empty? ls)
		acc
		(foldr reducer (reducer acc (first ls)) (rest ls))
)))
(def! rev (fn* (ls) (
	foldr (fn* (acc next) (
		cons next acc
	)) () ls
)))
(def! flatten (fn* (ls) (
	foldr (fn* (acc ls) (
		concat acc (
			if (sequential? ls)
				ls
				(list ls)
		)
	)) () ls
)))

; map functions
(def! hashmap (fn* (&pairs) (apply assoc {} (flatten pairs))))
(def! keys (fn* (mp) (map (fn* (pair) (first pair)) (pairs mp))))
(def! vals (fn* (mp) (map (fn* (pair) (first (rest pair))) (pairs mp))))

; string functions
(def! pr-str (fn* (&strs) (
	if (empty? strs)
		""
		(str (reduce (fn* (acc x) (str acc " " x)) strs))
)))
