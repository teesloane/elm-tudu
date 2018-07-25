(ns tudu.util)

(defn time-now
  []
  (quot (System/currentTimeMillis) 1000))

(defn time-now-ms
  []
  (System/currentTimeMillis))

(defn int->bool
  "FIXME I'm sure there is a way to cast this but I don't have the internet rn."
  [i]
  (if (= i 1) true false))
