(ns node
  (:require [goog.object]
            [match.core]
            [match.async]
            [fsm.core]))


(js/console.log "Starting...")


(defn hello []
  "Hello, world!")


(comment
  (js/console.log (hello))
  ;
  )