(ns test-core.core-test
  (:require [clojure.math :as math]
            [clojure.pprint :refer [cl-format]]
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

;; The other comparison operators are below?!
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

; reduce1 -> reduce

(deftest test-reverse
  (is (= '(c b a) (reverse '(a b c))))
  (is (= clojure.lang.PersistentList (class (reverse '(a b c)))) )
  (is (= '(3 2 1) (reverse [1 2 3])))
  (is (= clojure.lang.PersistentList (class (reverse [1 2 3]))))
  (is (= '([:b 2] [:a 1]) (reverse {:a 1 :b 2})))
  (is (= clojure.lang.PersistentList (class (reverse {:a 1 :b 2}))))
  (is (= '(\g \n \u \p) (reverse "pung")))
  (is (= clojure.lang.PersistentList (class (reverse "pung")))) )

; nary-inline
; *unchecked-math*
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

(deftest test-<=
  (is (<= 2))
  (is (<= 2 3))
  (is (<= 8/9 1.0 5/4 2N 8M))
  (is (<= 1 2 3 4 5))
  (is (<= 1 1 2 2 3 3 4 4))
  (is (not (<= 1 1 2 2 Math/PI 3 4 4))))

(deftest test->
  (is (> 2))
  (is (> 3 2))
  (is (apply > (reverse '(8/9 1.0 5/4 2N 8M))))
  (is (apply > (reverse '(1 2 3 4 5))))
  (is (not (apply > (reverse '(1 1 2 2 3 3 4 4)))) ))

(deftest test->=
  (is (>= 2))
  (is (>= 3 2))
  (is (apply >= (reverse '(8/9 1.0 5/4 2N 8M))))
  (is (apply >= (reverse '(1 2 3 4 5))))
  (is (apply >= (reverse '(1 1 2 2 3 3 4 4))))
  (is (not (apply <= (reverse '(1 1 2 2 Math/PI 3 4 4)))) ))

(deftest test-==
  (is (== 2))
  (is (== 2 2))
  (is (not (== 2 3)))
  (is (not (= 2 2.0)))
  (is (== 2 2.0))
  ;; (is (not (= 2 2N)))
  ;; (is (== 2 2N))
  (is (not (= 2 2M)))
  (is (== 2 2M))
  (is (not (= 2.0 2M)))
  (is (== 2.0 2M))
  (is (= (= 0 -0) (== 0 -0)))
  (is (= (= 0.0 -0.0) (== 0.0 -0.0)))
  (is (= (= ##Inf ##Inf) (== ##Inf ##Inf)))
  (is (= (= ##-Inf ##-Inf) (== ##-Inf ##-Inf)))
  (is (= (= ##NaN ##NaN) (== ##NaN ##NaN)))
  (is (== 2 2.0 2M 2N 4/2)))

(deftest test-max
  (is (== 2 (max 2)))
  (is (== 3 (max 2 3)))
  (is (== 5 (apply max (shuffle [5 4 3 2 1]))))
  (is (= 2.0 (max 2 2.0)))
  (is (= 2.0 (max 2 4/2 2M 2N 2.0)))) ; Latter of equal values favored

(deftest test-min
  (is (== 2 (min 2)))
  (is (== 2 (min 2 3)))
  (is (== 1 (apply min (shuffle [5 4 3 2 1]))))
  (is (= 2.0 (min 2 2.0)))
  (is (= 2.0 (min 2 4/2 2M 2N 2.0)))) ; Latter of equal values favored

(deftest test-abs
  (is (= 0.0 (abs 0.0) (abs -0.0)))
  (is (= 3 (abs 3) (abs -3)))
  (is (= 3.0 (abs 3.0) (abs -3.0)))
  (is (= 3N (abs 3N) (abs -3N)))
  (is (= 3M (abs 3M) (abs -3M)))
  (is (neg? (abs Long/MIN_VALUE))) ; ?!?!
  (is (every? (every-pred pos? infinite?) (map abs [##Inf ##-Inf])))
  (is (NaN? (abs ##NaN))))

(deftest test-dec'
  (is (== 0 (dec' 1)))
  (is (= 0 (dec' 1)))
  (is (== 0.0 (dec' 1.0)))
  (is (= 0.0 (dec' 1.0)))
  (is (== 1/3 (dec' 4/3)))
  (is (= 1/3 (dec' 4/3)))
  (is (== 1N (dec' 2N)))
  (is (= 1N (dec' 2N)))
  (is (== 2M (dec' 3M)))
  (is (= 2M (dec' 3M)))
  (is (== -9223372036854775809N (dec' Long/MIN_VALUE)))
  (is (== -9223372036854775809N (dec' (bigint Long/MIN_VALUE))))
  (is (== -9223372036854775809N (dec' (biginteger Long/MIN_VALUE)))) )

(deftest test-dec
  (is (== 0 (dec 1)))
  (is (= 0 (dec 1)))
  (is (== 0.0 (dec 1.0)))
  (is (= 0.0 (dec 1.0)))
  (is (== 1/3 (dec 4/3)))
  (is (= 1/3 (dec 4/3)))
  (is (== 1N (dec 2N)))
  (is (= 1N (dec 2N)))
  (is (== 2M (dec 3M)))
  (is (= 2M (dec 3M)))
  (is (thrown? ArithmeticException (dec Long/MIN_VALUE)))
  (is (== -9223372036854775809N (dec (bigint Long/MIN_VALUE))))
  (is (== -9223372036854775809N (dec (biginteger Long/MIN_VALUE)))) )

;; unchecked-inc-int
;; unchecked-inc
;; unchecked-dec-int
;; unchecked-dec
;; unchecked-negate-int
;; unchecked-negate
;; unchecked-add-int
;; unchecked-add
;; unchecked-subtract-int
;; unchecked-subtract
;; unchecked-multiply-int
;; unchecked-multiply
;; unchecked-divide-int
;; unchecked-remainder-int

(deftest test-pos?
  (is (pos? 1))
  (is (pos? 1.0))
  (is (pos? 1/100))
  (is (pos? 1M))
  (is (pos? 1N))
  (is (pos? Double/MIN_VALUE))
  (is (pos? ##Inf))
  (is (not (pos? 0)))
  (is (not (pos? 0.0)))
  (is (not (pos? 0M)))
  (is (not (pos? 0N)))
  (is (not (pos? -1)))
  (is (not (pos? -1.0)))
  (is (not (pos? -1/100)))
  (is (not (pos? -1M)))
  (is (not (pos? (- Double/MIN_VALUE))))
  (is (not (pos? ##-Inf)))
  (is (not (pos? ##NaN))))

;;;
;;;    p       r
;;;    - = q + -
;;;    d       d
;;;    
;;;    p = qd + r <=> r = p - qd
;;;    
;;;    d = (quot p q) => r = (rem p q)
;;;    d = (math/floor (/ p q)) => r = (mod p q)   !!!
;;;
;;;    truncate
;;;    
(deftest test-quot
  (is (= (quot 34 10) (int (/ 34 10))))
  (is (== (quot 34 10) (int (/ 34 10))))
  (is (= (quot -34 10) (int (/ -34 10))))
  (is (== (quot -34 10) (int (/ -34 10))))

  (is (= (quot 34 10) (long (/ 34 10))))
  (is (== (quot 34 10) (long (/ 34 10))))
  (is (= (quot -34 10) (long (/ -34 10))))
  (is (== (quot -34 10) (long (/ -34 10))))

  (is (not= (quot 34 10) (math/ceil (/ 34 10))))
  (is (not (== (quot 34 10) (math/ceil (/ 34 10)))) )
  (is (not= (quot 34 10) (math/floor (/ 34 10))))
  (is (== (quot 34 10) (long (/ 34 10))))

  (is (not= (quot -34 10) (math/ceil (/ -34 10))))
  (is (== (quot -34 10) (math/ceil (/ -34 10))))
  (is (not= (quot -34 10) (math/floor (/ -34 10))))
  (is (not (== (quot -34 10) (math/floor (/ -34 10)))) )

  (is (= java.lang.Long (class (quot 34 10))))
  (is (= java.lang.Double (class (quot 34 10.0))))
  (is (not= (quot 34 10.0) (long (/ 34 10.0))))
  (is (== (quot 34 10.0) (long (/ 34 10.0)))) )

;;;
;;;    Same sign as first arg (or 0)
;;;    
(deftest test-rem
  (is (== 1 (rem 10 3)))
  (is (== 1 (rem 10 -3)))
  (is (== -1 (rem -10 3)))
  (is (== -1 (rem -10 -3))))

(deftest test-rationalize
  (is (== 5 (rationalize 5)))
  (is (== 1/10 (rationalize 1/10)))
  (is (not (== 1/10 (rationalize (float 0.1)))) ) ;!!
  (is (== 1/10 (rationalize 0.1)))
  (is (== 2/10 (rationalize 0.2)))
  (is (== 3/10 (rationalize 0.3)))
  (is (== 4/10 (rationalize 0.4)))
  (is (== 5/10 (rationalize 0.5)))
  (is (== 6/10 (rationalize 0.6))))

;;;
;;;    https://en.wikipedia.org/wiki/Bitwise_operation
;;;    
(deftest test-bit-not
  (is (== -1 (bit-not 0)))
  (is (== -1 (bit-not (byte 0))))
  (is (== -1 (bit-not (short 0))))
  (is (== -1 (bit-not (int 0))))
  (is (thrown? IllegalArgumentException (bit-not 0N)))
  (is (== 0 (bit-not -1)))
  (let [n (rand-int 10000)]
    (is (and (== -1 (+ n (bit-not n)))
             (== n (bit-not (bit-not n)))) ))
  (is (== 2r1010101011010 (bit-and 2r1111111111111 (bit-not 2r0101010100101)))) )

(deftest test-bit-and
  (is (== 2r0000 (bit-and 2r0000 2r1001)))
  (is (== 2r1001 (bit-and 2r1001 2r1001)))
  (is (== 2r1001 (bit-and 2r1111 2r1001))) ; Identity
  (let [b1 2r101010
        b2 2r110110]
    (is (== (bit-and b1 b2) (bit-and b2 b1)))) ; Commutative
  (let [b1 2r101010
        b2 2r110110
        b3 2r010110]
    (is (== (bit-and b1 b2 b3)
            (bit-and b1 (bit-and b2 b3))
            (bit-and (bit-and b1 b2) b3)))) ; Associative
  (is (== 2r0000 (bit-and 2r1110 2r1101 2r1011 2r0111)))
  (is (thrown? IllegalArgumentException (bit-and 1N 0N)))
  (let [n (rand-int 10000)]
    (is (and (== 0 (bit-and 0 n))
             (== n (bit-and -1 n)))) ))

(deftest test-bit-or
  (is (== 2r1001 (bit-or 2r0000 2r1001))) ; Identity
  (is (== 2r1001 (bit-or 2r1001 2r1001)))
  (is (== 2r1111 (bit-or 2r1111 2r1001)))
  (let [b1 2r101010
        b2 2r110110]
    (is (== (bit-or b1 b2) (bit-or b2 b1)))) ; Commutative
  (let [b1 2r101010
        b2 2r110110
        b3 2r010110]
    (is (== (bit-or b1 b2 b3)
            (bit-or b1 (bit-or b2 b3))
            (bit-or (bit-or b1 b2) b3)))) ; Associative
  (is (== 2r1111 (bit-or 2r0001 2r0010 2r0100 2r1000)))
  (is (thrown? IllegalArgumentException (bit-or 1N 0N)))
  (let [n (rand-int 10000)]
    (is (and (== n (bit-or 0 n))
             (== -1 (bit-or -1 n)))) ))

(deftest test-bit-xor
  (is (== 2r1001 (bit-or 2r0000 2r1001))) ; Identity
  (is (== 2r1001 (bit-or 2r1001 2r1001)))
  (is (== 2r1111 (bit-or 2r1111 2r1001)))
  (is (thrown? IllegalArgumentException (bit-or 1N 0N)))
  (let [n (rand-int 10000)]
    (is (and (== n (bit-or 0 n))
             (== -1 (bit-or -1 n)))) ))

;;;
;;;    ~/clojure/clojure/src/jvm/clojure/lang/Numbers.java
;;;    static public long andNot(long x, long y){
;;;        return x & ~y;
;;;    }
;;;    
(deftest test-bit-and-not
  (is (== 2r0 (bit-and-not 2r1 2r1)))
  (is (== 2r1 (bit-and-not 2r1 2r0)))
  (is (== 2r0 (bit-and-not 2r0 2r1)))
  (is (== 2r0 (bit-and-not 2r1 2r1))) )

(deftest test-bit-clear
  (is (== 2r1110 (bit-clear 2r1111 0)))
  (is (== 2r1101 (bit-clear 2r1111 1)))
  (is (== 2r1011 (bit-clear 2r1111 2)))
  (is (== 2r0111 (bit-clear 2r1111 3)))
  (is (== 2r0000 (bit-clear 2r0000 0)))
  (is (== 2r0000 (bit-clear 2r0000 1)))
  (is (== 2r0000 (bit-clear 2r0000 2)))
  (is (== 2r0000 (bit-clear 2r0000 3)))
  (is (== 2r0000 (bit-clear (bit-clear (bit-clear (bit-clear 2r1111 0) 1) 2) 3))))

(deftest test-bit-set
  (is (== 2r1111 (bit-set 2r1111 0)))
  (is (== 2r1111 (bit-set 2r1111 1)))
  (is (== 2r1111 (bit-set 2r1111 2)))
  (is (== 2r1111 (bit-set 2r1111 3)))
  (is (== 2r0001 (bit-set 2r0000 0)))
  (is (== 2r0010 (bit-set 2r0000 1)))
  (is (== 2r0100 (bit-set 2r0000 2)))
  (is (== 2r1000 (bit-set 2r0000 3)))
  (is (== 2r1111 (bit-set (bit-set (bit-set (bit-set 2r0000 0) 1) 2) 3))))

(deftest test-bit-flip
  (is (== 2r1110 (bit-flip 2r1111 0)))
  (is (== 2r1101 (bit-flip 2r1111 1)))
  (is (== 2r1011 (bit-flip 2r1111 2)))
  (is (== 2r0111 (bit-flip 2r1111 3)))
  (is (== 2r0001 (bit-flip 2r0000 0)))
  (is (== 2r0010 (bit-flip 2r0000 1)))
  (is (== 2r0100 (bit-flip 2r0000 2)))
  (is (== 2r1000 (bit-flip 2r0000 3)))
  (is (== 2r0101 (bit-flip (bit-flip (bit-flip (bit-flip 2r1010 0) 1) 2) 3))))
  
(deftest test-bit-test
  (is (every? #(bit-test 2r1111 %) (range 4)))
  (is (not-any? #(bit-test 2r0000 %) (range 10))))

;; https://clojuredocs.org/clojure.core/bit-test#example-5d401face4b0ca44402ef78b
;; ;; bit-wise powerset (set of all subsets)

;; (defn powerset [coll]
;;   (let [cnt (count coll)
;;         bits (Math/pow 2 cnt)]
;;     (for [i (range bits)]
;;       (for [j (range i)
;;             :while (< j cnt)
;;             :when (bit-test i j)]
;;          (nth coll j)))))

;; (powerset [1 2 3])
;; ;; (() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3))

;;;
;;;    Clojure
;;;    
;; (bit-shift-left 2r1 62) => 4611686018427387904
;; (bit-shift-left 2r1 63) => -9223372036854775808
;; (bit-shift-left 2r1 64) => 1   <-- ????????
;; (bit-shift-left 2r1 65) => 2
;;;
;;;    Same as Java
;;;
;; 1L << 62 => 4611686018427387904
;; 1L << 63 => -9223372036854775808
;; 1L << 64 => 1
;; 1L << 65 => 2

;;;
;;;    Different from Java!
;;;
;; 1 << 30 => 1073741824
;; 1 << 31 => -2147483648
;; 1 << 33 => 2
;; 1 << 32 => 1
;; (bit-shift-left (int 1) 30) => 1073741824
;; (bit-shift-left (int 1) 31) => 2147483648
;; (bit-shift-left (int 1) 32) => 4294967296
;; (bit-shift-left (int 1) 33) => 8589934592

;;;
;;;    Common Lisp
;;;    
;; (ash 1 62) => 4611686018427387904
;; (ash 1 63) => 9223372036854775808
;; (ash 1 64) => 18446744073709551616
;; (ash 1 65) => 36893488147419103232
;;    见 Lisp pensoj 250310
(deftest test-bit-shift-left
  (is (every? #(== (bit-shift-left 2r1 %) (math/pow 2 %)) (range 63)))
  (is (loop [i 0
             b 1]
        (cond (== i 63) true
              (== (bit-shift-left 2r1 i) b) (recur (inc i) (*' 2 b))
              :else false)))
  (is (< (bit-shift-left 2r1 61) (bit-shift-left 2r1 62)))
  (is (> (bit-shift-left 2r1 62) (bit-shift-left 2r1 63))) ; !
  (is (neg? (bit-shift-left 2r1 63)))
  (is (== 1 (bit-shift-left 1 64)))
  (is (== Long/MAX_VALUE (bit-shift-left Long/MAX_VALUE 64)))
  (is (== Long/MIN_VALUE (bit-shift-left Long/MIN_VALUE 64)))
  (is (thrown? IllegalArgumentException (bit-shift-left 1N 63))))

(deftest test-bit-shift-right
  (is (neg? (bit-shift-right Long/MIN_VALUE 1)))
  (is (== Long/MAX_VALUE (bit-shift-right Long/MAX_VALUE 64)))
  (is (== Long/MIN_VALUE (bit-shift-right Long/MIN_VALUE 64)))
  (is (== -1 (bit-shift-right Long/MIN_VALUE 63))))

(deftest test-unsigned-bit-shift-right
  (is (pos? (unsigned-bit-shift-right Long/MIN_VALUE 1)))
  (is (== Long/MAX_VALUE (unsigned-bit-shift-right Long/MAX_VALUE 64)))
  (is (== Long/MIN_VALUE (unsigned-bit-shift-right Long/MIN_VALUE 64)))
  (is (== 1 (unsigned-bit-shift-right Long/MIN_VALUE 63))))





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
