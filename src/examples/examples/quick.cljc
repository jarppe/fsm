(ns examples.quick
  (:require [fsm.core :as fsm]))


(def fsm {:state  :flip
          :states {:flip {:on ["ping" {:to :flop}]}
                   :flop {:on ["ping" {:to :flip}]}}})

(-> fsm :state)
;=> :flip

(-> (fsm/apply-signal fsm "ping")
    :state)
;=> :flop

(-> (reduce fsm/apply-signal
            fsm
            ["ping" "ping" "ping"])
    :state)
;=> :flop

(def fsm {:state  :init
          :states {:init {:on ["hello" {:to :end
                                        :fx [[update :fx conj [:side-effect "hello"]]]}]}
                   :end  {:enter [[update :fx conj [:the-end]]]}}})

(-> (fsm/apply-signal fsm "hello")
    :fx)
;; => [[:side-effect "hello"] [:the-end]]


(defn require-role
  "Ensures that the user has a required role"
  [fsm required-role]
  (let [user (-> fsm :data :user)]
    (contains? (:roles user) required-role)))

(defn must-wear-safety-goggles
  "Ensures that the user has a safety-goggles"
  [fsm]
  (-> fsm :data :user :safety-goggles (true?)))

(def fsm {:state  :stopped
          :states {:stopped {:on ["start" {:to     :running
                                           :guards [[must-wear-safety-goggles]]}]}
                   :running {:on ["stop" {:to     :stopped
                                          :guards [[must-wear-safety-goggles]
                                                   [require-role :admin]]}]}}})
(def user {:roles          #{:admin}
           :safety-goggles false})

(defn on-click [button-name]
  (-> (assoc fsm :data {:user user})
      (fsm/apply-signal! button-name)))

(on-click "start")
; Execution error (ExceptionInfo) at (<cljs repl>:1).
; fsm: error: no transition for signal start

(-> (assoc fsm :data {:user user})
    (fsm/dry-run "start"))
;; => 
;; {:state       :stopped
;;  :signal      "start"
;;  :transitions ({:to       :running
;;                 :allowed? false
;;                 :guards   ([[#object[examples$quick$must_wear_safety_goggles]] false])
;;                 :fx       nil})}

