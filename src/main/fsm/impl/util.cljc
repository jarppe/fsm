(ns fsm.impl.util
  #?(:cljs (:require [goog.string.format]
                     [goog.string :as gs])))


;;
;; Cross-compatibility utils:
;;


(defn sprintf [fmt & args]
  #?(:clj (apply format fmt args)
     :cljs (apply gs/format fmt args)))
