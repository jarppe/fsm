(ns fsm.core-test
  (:require [clojure.test :as test :refer [deftest is testing]]
            [match.core :refer [matches?]]
            [fsm.core :as fsm]))

(comment

  ; FSM shape:
  {:id             'optional-fsm-id
   :state          'current-state
   :states         {:state-id {:guards ['guard...]
                               :on     ['signal-matchder {:to     'next-state
                                                          :guards ['guard...]
                                                          :fx     ['fx...]}
                                        ...]
                               :fx     ['fx...]
                               :enter  ['enter-fx...]
                               :stay   ['stay-fx...]
                               :leave  ['leave-fx...]}}
   :super          {:guards ['guard...]
                    :fx     ['fx...]
                    :on     ...}
   :default        {:guards ['guard...]
                    :fx     ['fx...]
                    :on     ...}
   :apply-fx       'fn-to-handle-handler-calls
   :signal-matcher 'fn-to-handle-signal-matching}

  ; The default 'fn-to-handle-handler-calls accepts handlers with this shape:
  ;    [handler-fn arg1 arg2 ...]
  ; and ivokes:
  ;    (apply handler-fn fsm arg1 arg2...)
  )


(deftest default-apply-fx-test
  (let [fsm {}]
    (is (matches? fsm
                  (fsm/default-apply-fx fsm nil)))
    (is (matches? {:data "hello"}
                  (fsm/default-apply-fx
                   fsm
                   [#(assoc % :data "hello")])))
    (is (matches? {:data "hello"}
                  (fsm/default-apply-fx
                   fsm
                   [assoc :data "hello"])))))



(deftest default-signal-matcher-test
  (is (fsm/default-signal-matcher {:signal "hello"}
                                  "hello"))
  (is (not (fsm/default-signal-matcher {:signal "hello"}
                                       "world")))
  (is (fsm/default-signal-matcher {:signal "hello"}
                                  #{"hello"}))
  (is (not (fsm/default-signal-matcher {:signal "hello"}
                                       #{"world"})))
  (is (fsm/default-signal-matcher {:signal "hello"}
                                  (partial = "hello")))
  (is (not (fsm/default-signal-matcher {:signal "hello"}
                                       (partial = "world")))))


(deftest transitions-for-signal-test
  (is (matches? [{:id "a"}]
                (-> {:state  :init
                     :states {:init {:on ["a" {:id "a"}
                                          "b" {:id "b"}]}}
                     :signal "a"}
                    (fsm/transitions-for-signal))))
  (is (matches? [{:id "a1"}
                 {:id "a2"}]
                (-> {:state  :init
                     :states {:init {:on ["a" {:id "a1"}
                                          "a" {:id "a2"}]}}
                     :signal "a"}
                    (fsm/transitions-for-signal))))
  (is (matches? [{:id :super-a}
                 {:id :a}
                 {:id :default-a}]
                (-> {:state   :init
                     :states  {:init {:on ["a" {:id :a}
                                           "b" {:id :b}]}}
                     :super   {:on ["a" {:id :super-a}
                                    "b" {:id :super-b}]}
                     :default {:on ["a" {:id :default-a}
                                    "b" {:id :default-b}]}
                     :signal  "a"}
                    (fsm/transitions-for-signal)))))


(deftest guards-for-transition-test
  (let [fsm        {:state   :init
                    :super   {:guards [:super-1
                                       :super-2]}
                    :states  {:init {:on     ["a" {:guards [:a-1
                                                            :a-2]}]
                                     :guards [:init-1
                                              :init-2]}}
                    :default {:guards [:default-1
                                       :default-2]}}
        transition (-> (assoc fsm :signal "a")
                       (fsm/transitions-for-signal)
                       (first))]
    (is (matches? {:guards [:a-1 :a-2]}
                  transition))
    (is (matches? [:super-1
                   :super-2
                   :init-1
                   :init-2
                   :a-1
                   :a-2
                   :default-1
                   :default-2]
                  (fsm/guards-for-transition fsm transition)))))


(def PASS (constantly true))
(def REJECT (constantly false))


(deftest filter-transitions-by-guards-test
  (let [fsm         {:state  :init
                     :states {:init {:on ["a" {:id :a}]}}}
        transitions (-> (assoc fsm :signal "a")
                        (fsm/transitions-for-signal))]
    (is (matches? [{:id :a}]
                  (fsm/filter-transitions-by-guards fsm
                                                    transitions))))
  (let [fsm         {:state  :init
                     :super  {:guards [REJECT]}
                     :states {:init {:on ["a" {:id :a}]}}}
        transitions (-> (assoc fsm :signal "a")
                        (fsm/transitions-for-signal))]
    (is (matches? nil
                  (fsm/filter-transitions-by-guards fsm
                                                    transitions))))
  (let [fsm         {:state  :init
                     :super  {:guards [PASS]}
                     :states {:init {:guards [PASS]
                                     :on     ["a" {:id     :a
                                                   :guards [PASS]}]}}}
        transitions (-> (assoc fsm :signal "a")
                        (fsm/transitions-for-signal))]
    (is (matches? [{:id :a}]
                  (fsm/filter-transitions-by-guards fsm
                                                    transitions)))))

(deftest apply-transition-test
  (let [fsm        {:state   :init
                    :super   {:fx [[update :fx conj :super]]}
                    :states  {:init {:on    ["a" {:fx [[update :fx conj :a]]}
                                             "b" {:to :next
                                                  :fx [[update :fx conj :b]]}]
                                     :stay  [[update :fx conj :init-stay]]
                                     :leave [[update :fx conj :init-leave]]}
                              :next {:enter [[update :fx conj :next-enter]]}}
                    :default {:fx [[update :fx conj :default]]}}]
    (is (matches? {:fx [:init-stay
                        :super
                        :a
                        :default]}
                  (fsm/apply-signal fsm "a")))
    (is (matches? {:fx [:init-leave
                        :super
                        :b
                        :default
                        :next-enter]}
                  (fsm/apply-signal fsm "b")))))


(deftest dry-run-test
  (let [guard (fn [_fsm pass?] pass?)
        fsm   {:state  :init
               :states {:init {:on ["a" {:to     :next
                                         :guards [[guard true]]
                                         :fx     [[update :fx conj :a-1]]}
                                    "a" {:to     :fofo
                                         :guards [[guard false]]
                                         :fx     [[update :fx conj :a-1]]}]}
                        :next {}
                        :fofo {}}}]
    (is (matches? {:state       :init
                   :signal      "a"
                   :transitions [{:to       :next
                                  :allowed? true
                                  :fx       [:a-1]}
                                 {:to       :fofo
                                  :allowed? false
                                  :fx       nil}]}
                  (fsm/dry-run fsm "a")))))
