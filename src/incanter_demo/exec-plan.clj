
;; How to use Incanter

(ns demo 
  (:use [incanter.core]
        [incanter.stats]
        [incanter.io]
        [incanter.charts]
        [incanter.datasets]
        [clojure.string]))

;; read-dataset
;; we will see how a data set is loaded.
;; We're going to load "Alice in Wonderland" 
;; "Through the LookingGlass" 
;;	complete work of emily dickinson and
;; "Moby Dick"(defn create-dataset 
(defn create-dataset 
  [file-name] 
  (let [dset (col-names (read-dataset file-name) [:word])]
    (transform-col dset :word lower-case)))

(def alice (create-dataset "data/alice.txt"))
(def through (create-dataset "data/through.txt"))
(def emily (create-dataset "data/emily.txt"))
(def moby (create-dataset "data/moby.txt"))

;; group aggreation data with rollup 
(defn word-count 
  [dset]
  ($order [:freq] :desc
    ($rollup :count :freq :word dset)))

(def alice-word-count (word-count alice))
;; filtering and subsetting the data using
;; head $ sel $where and $fn
;; example (with-data alice-word-count ($where {:freq  {:lte 10}}))
($ (range 10 20) :all alice-word-count)

(view (sel alice-word-count :filter #(< (nth % 1) 100) ))

;; histogram
;; summarize data
(with-data alice-word-count
  (view
    (histogram :freq :nbins 18 :data 
      ($where ($fn [freq] (and (<= freq 20) (> freq 1)))))))

;; another roll up to get the spectrum of words
(view ($rollup :count :fof :freq alice-word-count))

(defn fof
  [wc-data-set]   
    ($order [:freq] :asc 
      ($rollup :count :fof :freq wc-data-set)))

(def alice-fof (fof alice-word-count))

(view alice-fof)

;; get number of rows, columns , and dimensions
(dim alice-fof)

;; convert to matrix and transpose (trans)
(trans (to-matrix alice-fof))


;; log-log scatter plot using set-axis :x and :y
(view
  (doto
    (scatter-plot :freq :fof :data alice-fof)
    (set-axis :x (log-axis))
    (set-axis :y (log-axis))))

;; add a column to a dataset, nicer histogram
(def alice-word-count-2
  (col-names 
    (conj-cols
      alice-word-count
      ($map count [:word] alice-word-count))
    [:word :freq :len]))

(view alice-word-count-2)
(view
(histogram :len :data alice-word-count-2))

(summary alice-word-count-2)
;; quantiles and outliers detection
;; just copy 
(quantile ($ :len alice-word-count-2))

(defn is-outlier 
  [x quantiles]
   (let  [[_ q1 _ q3 _ ] quantiles
         iqr (- q3 q1)]  iqr)
  (or (< x (- q1 (* 1.5 iqr))) (> x (+ q3 (* 1.5 iqr)))))) 

(is-outlier 3 (quantile ($ :len alice-word-count-2)))


;; text summary
(defn get-freq-of-freq
    [dset freq] 
    (let [rec (with-data dset ($where {:freq {:eq freq}}))
          freq-of-freq ($ :fof rec)]
       (if (number? freq-of-freq) freq-of-freq 0)))
(defn text-summary 
  [title dset]
  ( let [dset-word-count (word-count dset)
         dset-fof ($order [:freq] :asc (fof dset-word-count))
         tokens (nrow dset)
         types  (nrow dset-word-count)
         hapax (get-freq-of-freq dset-fof 1)] 
  {:title title 
   :tokens tokens
   :types  types
   :hapax hapax
   :dis  (get-freq-of-freq dset-fof 2)
   :tris (get-freq-of-freq dset-fof 3)
   :ten  (get-freq-of-freq dset-fof 10)
   :twenty (get-freq-of-freq dset-fof 20)
   :ttr    (/ (double types) tokens )
   :growth  (/ (double hapax) tokens)
  }))

(def lit-stats
  (to-dataset 
    [(text-summary "alice" alice)
     (text-summary "through" through)
     (text-summary "moby" moby)
     (text-summary "emily" emily)]))

(view lit-stats)


;; growth time series 
(defn cut
      "divides a range r into chunck-count chunks. The function returns 
       the vector of the chunk number"
      [r chunk-count] 
         (let [chunk-size (quot (length r) chunk-count)]  
           (map #(inc (quot % chunk-size)) r)))

(defn chunked
  [dset chunk-count]
     (let [chunks (cut (range 0 (nrow dset)) chunk-count)
           chunked-dset (conj-cols dset chunks)]
       (col-names chunked-dset [:word :chunk])))

(def chunked-alice
  (chunked alice 40))

(defn cumulative-chunk
  [dset chunk-number]
  (with-data dset ($where {:chunk {:$lte chunk-number}})))
(defn cumulative-chunk
  [dset chunk-number]
  (with-data dset ($where {:chunk {:$lte chunk-number}})))

(def alice-growth      
  (to-dataset (map #(text-summary %1 (cumulative-chunk chunked-alice %1))  (range 1 42))))

(view alice-growth)

(view
  (doto
    (scatter-plot  :tokens :hapax :legend true :y-label "freq" :data alice-growth)
    (add-points :tokens :dis :data alice-growth)
    (add-points :tokens :tris :data alice-growth)))

(view
  (doto
    (scatter-plot  :hapax :twenty :legend true :y-label "freq" :data alice-growth)))


(view
(let [m 
      (to-matrix ($ [:tokens 
                  :types
                  :hapax 
                  :dis 
                  :tris 
                  :ten 
                  :twenty
                  :growth
                  :ttr] alice-growth))]
  (correlation m)))

;; linear regression model
;; linear-model functions, its properties, and stuffs
;; tracing lines using scatter plot following by add lines x (fitted lm)
(def lm1 (linear-model ($ :ten alice-growth) ($ :hapax alice-growth)))
(def lm2 (linear-model ($ :twenty alice-growth) ($ :hapax alice-growth)))

(doto 
  (scatter-plot :hapax :twenty :data alice-growth)
  (add-lines ($ :hapax alice-growth) (:fitted lm2))
  (add-points :hapax :ten :data alice-growth)
  (add-lines ($ :hapax alice-growth) (:fitted lm1))
  
  view)



