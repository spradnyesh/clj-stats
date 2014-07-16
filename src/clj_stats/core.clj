(ns clj-stats.core)

(defn stats-max [& args]
  (apply max args))

(defn stats-min [& args]
  (apply min args))

(defn mean [& args]
  (/ (apply + args)
     (count args)))

(defn trimmed-mean [trim & args]
  "remove 'trim'% data from extremes, and then take mean of remaining values.
this is needed to make 'mean' more robust (resistant to outliers)"
  (let [trim-count (int (* (/ trim 100) (count args)))
        data (drop-last trim-count (drop trim-count (sort args)))]
    (when-not (empty? data)
      (apply mean data))))

(defn- quartiles [numer denom & args]
  (let [center-index (* numer (/ (dec (count args)) denom))]
    (if (integer? center-index)
      (nth args center-index)
      (let [prev (nth args (Math/floor center-index))
            next (nth args (Math/ceil center-index))]
        (* numer (/ (+ prev next) denom))))))
(defn median [& args]
  (apply quartiles 1 2 args))
(defn first-quartile [& args]
  (apply quartiles 1 4 args))
(defn third-quartile [& args]
  (apply quartiles 3 4 args))

(defn stats-range [& args]
  (- (apply max args) (apply min args)))

(defn inter-quartile-range [& args] ; also called IQR
  (- (apply third-quartile args) (apply first-quartile args)))

(defn variance [& args]
  (let [mean (apply mean args)]
    (/ (reduce + (map (fn [x]
                        (Math/pow (- x mean) 2))
                      args))
       (dec (count args)))))

(defn standard-deviation [& args]
  (Math/pow (apply variance args) 0.5))

;;;; empirical rule
;; -1SD -- 1SD => 68% of all values
;; -2SD -- 2SD => 95% of all values
;; -3SD -- 3SD => 99.7% of all values
;; where SD => standard-deviation
