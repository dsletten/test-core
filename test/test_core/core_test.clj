(ns test-core.core-test
  (:require [clojure.math :as math]
            [clojure.test :refer :all]))

;; (defn to-array
;; (defn cast


(deftest test-vector
  (is (= [] (vector)))
  (is (= [1] (vector 1)))
  (is (= [1 2 3 4 5] (vector 1 2 3 4 5))))

(deftest test-vec
  (are [expected coll]
    (let [actual (vec coll)]
      (is (and (vector? actual)
               (= expected actual))))
    
    [] '()
    [1] '(1)
    [1 2 3] '(1 2 3)

    [] '[]
    [1] '[1]
    [1 2 3] '[1 2 3]

    [] {}
    [[:a 1]] {:a 1}
    [[:a 1] [:b 2]] {:a 1 :b 2}

    [] ""
    [\a] "a"
    [\a \s \d \f] "asdf"

    [] #{}
    [1] #{1}
    [1 3 2] #{1 2 3})) ; !!

(deftest test-hash-map
  (is (= {} (hash-map)))
  (is (= {:e 5 :g 7 :c 3 :h 8 :b 2 :d 4 :f 6 :i 9 :a 1}
         (hash-map :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9)))
  (is (= {:e 5 :g 7 :c 3 :h 8 :b 2 :d 4 :f 6 :i 9 :a 10}
         (hash-map :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :a 10)))
  (is (not= [:a :b :c :d :e :f :g :h :i]
            (keys (hash-map :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9)))) )

(deftest test-sorted-map
  (is (= {} (sorted-map)))
  (is (= {:e 5 :g 7 :c 3 :h 8 :b 2 :d 4 :f 6 :i 9 :a 1}
         (sorted-map :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9)))
  (is (= {:e 5 :g 7 :c 3 :h 8 :b 2 :d 4 :f 6 :i 9 :a 10}
         (sorted-map :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :a 10)))
  (is (= [:a :b :c :d :e :f :g :h :i]
         (keys (sorted-map :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9)))) )

(deftest test-hash-set
  (is (= #{} (hash-set)))
  (is (= #{:e :g :c :h :b :d :f :i :a} (hash-set :a :b :c :d :e :f :g :h :i)))
  (is (= #{:e :g :c :h :b :d :f :i :a} (hash-set :a :b :c :d :e :f :g :h :i :a)))
  (is (not= [:a :b :c :d :e :f :g :h :i]
            (vec (hash-set :a :b :c :d :e :f :g :h :i)))) )

(deftest test-sorted-set
  (is (= #{} (sorted-set)))
  (is (= #{:e :g :c :h :b :d :f :i :a} (sorted-set :a :b :c :d :e :f :g :h :i)))
  (is (= [:a :b :c :d :e :f :g :h :i]
         (vec (sorted-set :a :b :c :d :e :f :g :h :i)))) )

;https://clojure.org/guides/comparators
;;    Comparison function is baked into map, used for later lookup:
;;    (get (into (sorted-map-by (fn [a b] (println a b) (compare a b))) {"a" 1 "b" 2}) "c")
;; static public PersistentTreeMap create(Comparator comp, ISeq items){
;; 	IPersistentMap ret = new PersistentTreeMap(comp);
(deftest test-sorted-map-by
  (is (= [:a :b :c :d :e :f :g :h :i]
         (keys (sorted-map-by compare :f 6 :e 5 :g 7 :h 8 :a 1 :b 2 :i 9 :c 3 :d 4))))
  (is (= [:i :h :g :f :e :d :c :b :a]
         (keys (sorted-map-by #(compare %2 %1) :f 6 :e 5 :g 7 :h 8 :a 1 :b 2 :i 9 :c 3 :d 4))))
  (is (= [:a :b :c :d :e :f :g :h :i]
         (keys (into (sorted-map-by compare) {:e 5 :g 7 :c 3 :h 8 :b 2 :d 4 :f 6 :i 9 :a 1}))))
  (is (= [:a :b :c :d :e :f :g :h :i]
         (keys (into (sorted-map) {:e 5 :g 7 :c 3 :h 8 :b 2 :d 4 :f 6 :i 9 :a 1}))))
  (is (= [:i :h :g :f :e :d :c :b :a]
         (keys (into (sorted-map-by #(compare %2 %1)) {:e 5 :g 7 :c 3 :h 8 :b 2 :d 4 :f 6 :i 9 :a 1})))) )

(deftest test-sorted-set-by
  (is (= [:a :b :c :d :e :f :g :h :i]
         (vec (sorted-set-by compare :g :f :c :a :h :i :e :d :b))))
  (is (= [:i :h :g :f :e :d :c :b :a]
         (vec (sorted-set-by #(compare %2 %1) :g :f :c :a :h :i :e :d :b))))
  (is (= [:a :b :c :d :e :f :g :h :i]
         (vec (into (sorted-set-by compare) #{:g :f :c :a :h :i :e :d :b}))))
  (is (= [:a :b :c :d :e :f :g :h :i]
         (vec (into (sorted-set) #{:g :f :c :a :h :i :e :d :b}))))
  (is (= [:i :h :g :f :e :d :c :b :a]
         (vec (into (sorted-set-by #(compare %2 %1)) #{:g :f :c :a :h :i :e :d :b})))) )

(deftest test-nil?
  (is (nil? nil))
  (is (not (nil? false)))
  (is (not (nil? 0)))
  (is (not (nil? '())))
  (is (not (nil? [])))
  (is (not (nil? "")))
  (is (not (nil? {})))
  (is (not (nil? #{}))))
  
(deftest test-false?
  (is (false? false))
  (is (false? 'false)) ; !!!
  (is (not (false? nil)))
  (is (not (false? :false)))
  (is (not (false? "false")))
  (is (not (false? 0)))
  (is (not (false? '())))
  (is (not (false? [])))
  (is (not (false? "")))
  (is (not (false? {})))
  (is (not (false? #{}))))

(deftest test-true?
  (is (true? true))
  (is (true? 'true)) ; !!!
  (is (not (true? :true)))
  (is (not (true? "true")))
  (is (not (true? 1))))

(deftest test-boolean?
  (is (boolean? true))
  (is (boolean? 'true)) ; !!!
  (is (boolean? false))
  (is (boolean? 'false)) ; !!!
  (is (boolean? (Boolean. "true")))
  (is (boolean? (Boolean. 'true))) ; !!!
  (is (boolean? (Boolean. true)))
  (is (boolean? (Boolean. "false")))
  (is (boolean? (Boolean. 'false))) ; !!!
  (is (boolean? (Boolean. false)))
  (is (not (boolean? :true)))
  (is (not (boolean? "true")))
  (is (not (boolean? 1)))
  (is (not (boolean? nil)))
  (is (not (boolean? :false)))
  (is (not (boolean? "false")))
  (is (not (boolean? 0)))
  (is (not (boolean? '())))
  (is (not (boolean? [])))
  (is (not (boolean? "")))
  (is (not (boolean? {})))
  (is (not (boolean? #{}))))

(deftest test-some?
  (is (not (some? nil)))
  (is (some? false))
  (is (some? 0))
  (is (some? '()))
  (is (some? []))
  (is (some? ""))
  (is (some? {}))
  (is (some? #{})))

(deftest test-any? ; !!!
  (is (true? (any? nil)))
  (is (true? (any? false)))
  (is (true? (any? 0)))
  (is (true? (any? '())))
  (is (true? (any? [])))
  (is (true? (any? "")))
  (is (true? (any? {})))
  (is (true? (any? #{}))))

(deftest test-str
  (is (= "" (str)))
  (is (= "123" (str 1 2 3)))
  (are [expected x]
    (is (= expected (str x)))

    "" nil
    "a" \a
    "x" 'x
    ":y" :y
    "3.141592653589793" Math/PI
    "[]" []
    "[7 8 9]" [7 8 9]
    "{}" {}
    "{:a 1, :b 2}" {:a 1 :b 2}))

(deftest test-symbol?
  (is (symbol? 'pung))
  (is (not (symbol? "pung")))
  (is (not (symbol? :pung)))
  (is (symbol? (symbol "pung")))
  (is (symbol? (symbol :pung)))
  (is (not (symbol? 'true))) ; !!!
  (is (symbol? 'True))
  (is (symbol? 'TRUE))
  (is (not (symbol? :true)))
  (is (not (symbol? 'false))) ; !!!
  (is (symbol? 'False))
  (is (symbol? 'FALSE))
  (is (not (symbol? :false)))
  (is (symbol? (gensym))))

(deftest test-keyword?
  (is (keyword? :pung))
  (is (not (keyword? 'pung)))
  (is (not (keyword? "pung")))
  (is (keyword? (keyword 'pung)))
  (is (keyword? (keyword "pung"))))

(deftest test-symbol
  (let [s 'pung]
    (is (identical? s (symbol s))))
  (let [s "pung"]
    (is (= s (name (symbol s)))) )
  (let [k :pung]
    (is (= (name k) (name (symbol k)))) ))

(deftest test-keyword
  (let [k :pung]
    (is (identical? k (keyword k))))
  (let [s "pung"]
    (is (= s (name (keyword s)))) )
  (let [s 'pung]
    (is (= (name s) (name (keyword s)))) ))

;find-keyword

(deftest test-list*
  (is (nil? (list* nil)))
  (is (nil? (list* '())))
  (is (nil? (list* [])))
  (is (= '(a) (list* 'a '())))
  (is (= '(a b) (list* 'a 'b '())))
  (is (= '(a b c d e) (list* 'a 'b 'c 'd 'e '()))) )

(deftest test-apply
  (is (== (+ 1 2 3 4 5) (apply + '(1 2 3 4 5))))
  (is (== (+ 1 2 3 4 5) (apply + 1 '(2 3 4 5))))
  (is (== (+ 1 2 3 4 5) (apply + 1 2 '(3 4 5))))
  (is (== (+ 1 2 3 4 5) (apply + 1 2 3 '(4 5))))
  (is (== (+ 1 2 3 4 5) (apply + 1 2 3 4 '(5))))
  (is (== (+ 1 2 3 4 5) (apply + 1 2 3 4 5 '()))) )

(deftest test-meta-data
  (is (nil? (meta [1 2 3])))
  (is (= {:foo :bar} (meta (with-meta [1 2 3] {:foo :bar}))))
  (is (= {:a 1} (meta (with-meta (with-meta (with-meta #{7 8} {:c 3}) {:b 2}) {:a 1}))))
  (is (= {:a 1 :b 2 :c 3} (meta (vary-meta (vary-meta (with-meta #{7 8} {:c 3})
                                                      merge
                                                      {:b 2})
                                           merge
                                           {:a 1})))) )


;; (defmacro lazy-seq
;; (defn ^:static ^clojure.lang.ChunkBuffer chunk-buffer ^clojure.lang.ChunkBuffer [capacity(defn ^:static chunk-append [^clojure.lang.ChunkBuffer b x]
;; (defn ^:static ^clojure.lang.IChunk chunk [^clojure.lang.ChunkBuffer b]
;; (defn ^:static  ^clojure.lang.IChunk chunk-first ^clojure.lang.IChunk [^clojure.lang.IChu(defn ^:static ^clojure.lang.ISeq chunk-rest ^clojure.lang.ISeq [^clojure.lang.IChunkedSe(defn ^:static ^clojure.lang.ISeq chunk-next ^clojure.lang.ISeq [^clojure.lang.IChunkedSe(defn ^:static chunk-cons [chunk rest]
;; (defn ^:static chunked-seq? [s]

(deftest test-concat
  (is (empty? (concat)))
  (is (empty? (concat [] [] [])))
  (is (= '(:a) (concat [:a])))
  (is (= '(:a :b) (concat [:a] [:b])))
  (is (= '(:a :b) (concat [:a] [] [:b])))
  (is (= '(:a :b) (concat [:a] [:b] [])))
  (is (= '(:a :b :c :d) (concat [:a] [:b] [:c] [:d])))
  (is (= '(:a :b :c :d) (concat [] [:a] [] [:b] [] [:c] [] [:d] [])))
  (is (= '((a b) (c d e) (f)) (concat '((a b)) '((c d e)) '((f)))) )
  (is (= "Is this not pung?" (apply str (concat "Is " "this " "not " "pung?")))) )

;; (defmacro delay
;; (defn delay?
;; (defn force

(deftest test-if-not
  (is (if-not (even? 9) true false))
  (is (if-not (odd? 9) false true))
  (are [x]
    (is (= (if-not (< x 10) :big :small)
           (if (not (< x 10)) :big :small)
           (if (>= x 10) :big :small)
           (if (<= 10 x) :big :small)))

    3
    10
    100))

(deftest test-identical?
  (is (identical? 2 2))
  (is (not (identical? 2000 2000)))
  (is (identical? 1/1 1/1))
  (is (identical? 1/1 1))
  (is (identical? 4/4 1))
  (is (not (identical? 1/2 1/2)))
  (is (not (identical? 2.0 2.0)))
  (is (not (identical? Math/PI Math/PI)))
  (let [x 2.0]
    (is (not (identical? x x)))) ; !
  (is (identical? "asdf" "asdf"))
  (is (not (identical? 'x 'x))) ; !!!!!!!!!!!!
  (is (identical? :x :x))
  (is (identical? \P \P))
  (is (not (identical? \北 \北)))
  (is (identical? '() '()))
  (is (not (identical? '(a b c) '(a b c))))
  (let [l (list nil nil)]
    (is (identical? l l)))
  (is (not (identical? '(()) '(()))) )
  (is (identical? [] []))
  (is (not (identical? [1 2] [1 2])))
  (is (identical? {} {}))
  (is (not (identical? {:a 1} {:a 1})))
  (is (identical? #{} #{}))
  (is (not (identical? #{:x :y} #{:x :y}))))

(deftest test-=
  ;; Don't know what's going on here...
  ;; test-core.core> (= 1) => true
  ;;(defn =
  ;; ([x] true)
  ;; ([x y] (clojure.lang.Util/equiv x y))
  ;; ([x y & more]
  ;;  (if (clojure.lang.Util/equiv x y)
  ;;    (if (next more)
  ;;      (recur y (first more) (next more))
  ;;      (clojure.lang.Util/equiv y (first more)))
  ;;    false)))

;;  (is (= 1)) ;    error: java.lang.Exception: = expects more than one argument

  (is (= 2 2))
  (is (= 2000 2000))
  (is (= 1/1 1/1))
  (is (= 1/1 1))
  (is (= 4/4 1))
  (is (= 1/2 1/2))
  (is (= 2.0 2.0))
  (is (= Math/PI Math/PI))
  (let [x 2.0]
    (is (= x x)))
  (is (= 1 3/3 1N (int 1) (short 1) (byte 1)))
  (is (not (= 1 1.0)))
  (is (not (= 1M 1N)))
  (is (= "asdf" "asdf"))
  (is (= 'x 'x))
  (is (= :x :x))
  (is (= \P \P))
  (is (= \北 \北))
  (is (= '() '()))
  (is (= '(a b c) '(a b c)))
  (let [l (list nil nil)]
    (is (= l l)))
  (is (= '(()) '(())))
  (is (= [] []))
  (is (= [1 2] [1 2]))
  (is (= {} {}))
  (is (= {:a 1} {:a 1}))
  (is (= #{} #{}))
  (is (= #{:x :y} #{:x :y})))

;;;
;;;    Different semantics from Common Lisp:
;;;    (= 1 2 1) => NIL
;;;    (/= 1 2 1) => NIL
;;;    
(deftest test-not=
  (is (not= 1 2))
  (is (not= 1 1.0))
  (is (not= 1 2 1)))

(deftest test-compare
  (is (zero? (compare 2 2)))
  (is (neg? (compare 2 5)))
  (is (pos? (compare 2 1)))
  (is (zero? (compare 2.0 2.0)))
  (is (neg? (compare 2.0 5.0)))
  (is (pos? (compare 2.0 0.0)))
  (is (zero? (compare 1/3 1/3)))
  (is (neg? (compare 1/4 1/3)))
  (is (pos? (compare 3/5 1/5)))
  (is (zero? (compare \a \a)))
  (is (neg? (compare \a \c)))
  (is (pos? (compare \f \b)))
  (is (pos? (compare \a \A)))
  (is (zero? (compare "" "")))
  (is (neg? (compare "" "pung")))
  (is (pos? (compare "pung" "")))
  (is (neg? (compare "cat" "caterwaul")))
  (is (zero? (compare [1 2] [1 2])))
  (is (neg? (compare [1 2] [1 3])))
  (is (pos? (compare [2 2] [1 2]))))

(deftest test-and
  (is (and))
  (is (and true))
  (is (not (and false)))
  (is (and true true))
  (is (not (and true false)))
  (is (not (and false true)))
  (is (not (and false false)))
  (is (== 4 (and 1 2 3 4))))

(deftest test-or
  (is (not (or)))
  (is (or true))
  (is (not (or false)))
  (is (or true true))
  (is (or true false))
  (is (or false true))
  (is (not (or false false)))
  (is (== 1 (or 1 2 3 4))))

(deftest test-zero?
  (is (zero? 0))
  (is (zero? -0))
  (is (zero? 0.0))
  (is (zero? -0.0))
  (is (zero? 0/5))
  (is (zero? (+))))

(deftest test-count
  (are [n ctor]
    (is (== n (count (apply ctor (range n)))) )

    5 list
    6 hash-set
    1 vector
    9 str
    10 (fn [& xs] (into {} (map vector xs xs)))) )

(deftest test-int
  (is (== 3 (int Math/PI)))
  (is (== 3 (int 10/3)))
  (is (= java.lang.Integer (class (int Math/PI))))
  (is (== (quot 10 3) (int 10/3)))
  (is (not= (class (int 10/3)) (class (quot 10 3))))
  (is 97 (int \a)) ; CHAR-CODE
  (is 83 (int \S)))

(deftest test-nth
  (is (= :c (nth [:a :b :c] 2)))
  (is (= :foo (nth [:a :b :c] 9 :foo)))
  (is (thrown? IndexOutOfBoundsException (nth [:a :b :c] 9)))
  (is (= \g (nth "pung" 3)))
  (is (= :pung (nth '(:foo :bar :baz :pung) 3)))
  (is (nil? (nth nil 0)))
  (is (nil? (nth nil 1)))
  (is (nil? (nth nil 100))))

(deftest test-<
  (is (< 2))
  (is (< 2 3))
  (is (< 8/9 1.0 5/4 2N 8M))
  (is (< 1 2 3 4 5))
  (is (not (< 1 2 2 3 3 4))))
  
(deftest test-inc'
  (is (== 1 (inc' 0)))
  (is (= 1 (inc' 0)))
  (is (== 1.0 (inc' 0.0)))
  (is (= 1.0 (inc' 0.0)))
  (is (== 4/3 (inc' 1/3)))
  (is (= 4/3 (inc' 1/3)))
  (is (== 2N (inc' 1N)))
  (is (= 2N (inc' 1N)))
  (is (== 3M (inc' 2M)))
  (is (= 3M (inc' 2M)))
  (is (== 9223372036854775808N (inc' Long/MAX_VALUE)))
  (is (== 9223372036854775808N (inc' (bigint Long/MAX_VALUE))))
  (is (== 9223372036854775808N (inc' (biginteger Long/MAX_VALUE)))) )

(deftest test-inc
  (is (== 1 (inc 0)))
  (is (= 1 (inc 0)))
  (is (== 1.0 (inc 0.0)))
  (is (= 1.0 (inc 0.0)))
  (is (== 4/3 (inc 1/3)))
  (is (= 4/3 (inc 1/3)))
  (is (== 2N (inc 1N)))
  (is (= 2N (inc 1N)))
  (is (== 3M (inc 2M)))
  (is (= 3M (inc 2M)))
  (is (thrown? ArithmeticException (inc Long/MAX_VALUE)))
  (is (== 9223372036854775808N (inc (bigint Long/MAX_VALUE))))
  (is (== 9223372036854775808N (inc (biginteger Long/MAX_VALUE)))) )

; reduce1

(deftest test-reverse
  (is (= '(c b a) (reverse '(a b c))))
  (is (= clojure.lang.PersistentList (class (reverse '(a b c)))) )
  (is (= '(3 2 1) (reverse [1 2 3])))
  (is (= clojure.lang.PersistentList (class (reverse [1 2 3]))))
  (is (= '([:b 2] [:a 1]) (reverse {:a 1 :b 2})))
  (is (= clojure.lang.PersistentList (class (reverse {:a 1 :b 2}))))
  (is (= '(\g \n \u \p) (reverse "pung")))
  (is (= clojure.lang.PersistentList (class (reverse "pung")))) )

; >1?, >0?

(deftest test-+'
  (is (zero? (+')))
  (is (== 1 (+' 1)))
  (is (== 3 (+' 1 2)))
  (is (== 55 (+' 1 2 3 4 5 6 7 8 9 10)))
  (is (== (inc' Long/MAX_VALUE) (+' Long/MAX_VALUE 1)))
  (is (== ##Inf (+' Double/MAX_VALUE Double/MAX_VALUE))))

(deftest test-+
  (is (zero? (+)))
  (is (== 1 (+ 1)))
  (is (== 3 (+ 1 2)))
  (is (== 55 (+ 1 2 3 4 5 6 7 8 9 10)))
  (is (thrown? ArithmeticException (+ Long/MAX_VALUE 1)))
  (is (== ##Inf (+ Double/MAX_VALUE Double/MAX_VALUE))))

(deftest test-*'
  (is (== 1 (*')))
  (is (== 7 (*' 7)))
  (is (== 12 (*' 3 4)))
  (is (== 1024 (apply *' (take 10 (repeat 2)))) )
  (let [big (long (math/ceil (math/sqrt Long/MAX_VALUE)))]
    (is (== 9223372037000250000N (*' big big))))
  (is (== ##Inf (*' Double/MAX_VALUE Double/MAX_VALUE))))

(deftest test-*
  (is (== 1 (*)))
  (is (== 7 (* 7)))
  (is (== 12 (* 3 4)))
  (is (== 1024 (apply * (take 10 (repeat 2)))) )
  (let [big (long (math/ceil (math/sqrt Long/MAX_VALUE)))]
    (is (thrown? ArithmeticException (* big big))))
  (is (== ##Inf (* Double/MAX_VALUE Double/MAX_VALUE))))

;;; !!!
;(deftest test-/
(deftest test-division
  (is (== 1/2 (/ 2)))
  (is (== 0.5 (/ 2.0)))
  (is (== 2 (/ 8 4)))
  (is (== 9/7 (/ 9 7)))
  (is (== (/ (/ 7 9)) (/ 9 7)))
  (is (== 3/2 (/ 3 2)))
  (is (== 1.5 (/ 3 2.0)))
  (is (== 1 (/ 120 2 3 4 5)))
  (is (== (/ (/ (/ (/ 120 2) 3) 4) 5) (/ 120 2 3 4 5)))
  (is (not= (/ 120 (/ 2 (/ 3 (/ 4 5)))) (/ 120 2 3 4 5)))
  (is (thrown? ArithmeticException (/ 0)))
  (is (thrown? ArithmeticException (/ 1 0)))
  (is (thrown? ArithmeticException (/ 0.0)))
  (is (Double/isInfinite (/ 1 0.0)))
  (is (Double/isNaN (/ (math/sqrt -1)))) )

(deftest test--'
  (is (thrown? clojure.lang.ArityException (-')))
  (is (== -1 (-' 1)))
  (is (== -1 (-' 1 2)))
  (is (== (-' (-' 2 1)) (-' 1 2)))
  (is (== -53 (-' 1 2 3 4 5 6 7 8 9 10)))
  (is (== (-' (-' (-' 1 2) 3) 4) (-' 1 2 3 4)))
  (is (not= (-' 1 (-' 2 (-' 3 4))) (-' 1 2 3 4)))
  (is (== (dec' Long/MIN_VALUE) (-' Long/MIN_VALUE 1)))
  (is (== ##-Inf (-' 0 Double/MAX_VALUE Double/MAX_VALUE))))

(deftest test--
  (is (thrown? clojure.lang.ArityException (-)))
  (is (== -1 (- 1)))
  (is (== -1 (- 1 2)))
  (is (== (- (- 2 1)) (- 1 2)))
  (is (== -53 (- 1 2 3 4 5 6 7 8 9 10)))
  (is (== (- (- (- 1 2) 3) 4) (- 1 2 3 4)))
  (is (not= (- 1 (- 2 (- 3 4))) (- 1 2 3 4)))
  (is (thrown? ArithmeticException (- Long/MIN_VALUE 1)))
  (is (== ##-Inf (- 0 Double/MAX_VALUE Double/MAX_VALUE))))


(deftest test-ratios
  (is (== 7/2 14/4))
  (is (== -7/2 (/ -14 4)))
  (is (== -7/2 (/ 14 -4)))
  (is (== 1 4/4))
  #_(let [r 9/15]
    (is (== 1 (gcd (numerator r) (denominator r))))) ; Relatively prime
  (is (== 7 (numerator 14/4)))
  (is (== 2 (denominator 14/4)))
  (is (thrown? ClassCastException (numerator 8))))
