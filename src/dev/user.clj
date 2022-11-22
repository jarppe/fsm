(ns user
  (:require [shadow.cljs.devtools.api :as shadow]))


(defn repl [build-id]
  (shadow/repl build-id))


#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn repl-web [] (repl :web))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn repl-api [] (repl :node))


(defn run-unit-tests []
  (shadow/compile :test))


(comment

  (shadow/compile :node)
  (shadow/compile :test)

  :cljs/quit
  (repl :web)
  (repl :node)

  (shadow/active-builds)
  ;; => #{:functions :web}

  (shadow/repl :web)
  (js/console.log "Hello!")
  :cljs/quit

  (shadow/repl :functions)
  (js/console.log "Hello")
  :cljs/quit

  ;
  )

