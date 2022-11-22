(ns fsm.core
  (:require [fsm.impl.util :refer [sprintf]]))


;; TODO: It would be great if we could use a symbols as handlers. Unfortunately
;; cljs does not work with `(resolve handler)`.


(defn default-apply-fx
  "Default handler to execute fx handler"
  [fsm handler]
  (cond
    (nil? handler)
    fsm

    (or (fn? handler)
        (var? handler))
    (handler fsm)

    (and (vector? handler)
         (or (fn? (first handler))
             (var? (first handler))))
    (apply (first handler) fsm (rest handler))

    :else
    (throw (ex-info (sprintf "fsm: error: invalid hanlder")
                    {:fsm     fsm
                     :handler handler}))))


(defn default-signal-matcher
  "Default function used to match signals"
  [fsm matcher]
  (let [signal (:signal fsm)]
    (if (ifn? matcher)
      (matcher signal)
      (= matcher signal))))


(defn transitions-for-signal
  "Return all possible transitions from current state with given signal"
  [fsm]
  (let [state          (-> fsm :state)
        signal-matcher (partial (-> fsm :signal-matcher (or default-signal-matcher)) fsm)]
    (->> (concat (-> fsm :super :on)
                 (-> fsm :states (get state) :on)
                 (-> fsm :default :on))
         (partition 2)
         (keep (fn [[matcher transition]]
                 (when (signal-matcher matcher)
                   transition))))))


(defn guards-for-transition
  "Return all guards for given transition"
  [fsm transition]
  (let [state (-> fsm :state)]
    (concat (-> fsm :super :guards)
            (-> fsm :states (get state) :guards)
            (-> transition :guards)
            (-> fsm :default :guards))))


(defn fx-for-transition
  "Return all fx handlers for given transition"
  [fsm transition]
  (let [state  (or (-> transition :to)
                   (-> fsm :state))]
    (concat (-> fsm :super :fx)
            (-> fsm :states (get state) :fx)
            (-> transition :fx)
            (-> fsm :default :fx))))


(defn filter-transitions-by-guards
  "Filters transitions based of guards"
  [fsm transitions]
  (let [allows? (partial (-> fsm :apply-fx (or default-apply-fx)) fsm)]
    (->> transitions
         (filter (fn [transition]
                   (->> (guards-for-transition fsm transition)
                        (every? allows?))))
         (seq))))


(defn apply-transition
  "Perform state transition using given transition. Apply `:stay` or
   `enter` and `leave` handers, and transition fx handlers."
  [fsm transition]
  (let [prev-state     (-> fsm :state)
        next-state     (-> transition :to (or prev-state))
        _              (when-not (-> fsm :states (contains? next-state))
                         (throw (ex-info (sprintf "fsm: error: transitioning to unknown state: %s" next-state)
                                         {:fsm        fsm
                                          :signal     (-> fsm :signal)
                                          :state      prev-state
                                          :next-state next-state})))
        move?          (not= next-state prev-state)
        stay           (when-not move?
                         (-> fsm :states (get prev-state) :stay))
        leave          (when move?
                         (-> fsm :states (get prev-state) :leave))
        enter          (when move?
                         (-> fsm :states (get next-state) :enter))
        fxs            (fx-for-transition fsm transition)
        apply-handler  (-> fsm :apply-fx (or default-apply-fx))
        apply-handlers (partial reduce apply-handler)]
    (-> (assoc fsm
               :fx []
               :prev-state prev-state
               :next-state next-state)
        (apply-handlers leave)
        (apply-handlers stay)
        (apply-handlers fxs)
        (assoc :state next-state)
        (apply-handlers enter)
        (dissoc :prev-state :next-state))))


(defn get-transition
  "Get first available transition for current signal, or `nil` if
   no transition is available for signal"
  [fsm]
  (let [transitions (transitions-for-signal fsm)]
    (-> (filter-transitions-by-guards fsm transitions)
        (first))))


(defn apply-signal
  "Apply signal `signal` to given `fsm` and return updated `fsm`, or
   nil if no transition was found for given signal"
  [fsm signal]
  (let [fsm        (assoc fsm :signal signal)
        transition (get-transition fsm)]
    (when transition
      (-> (apply-transition fsm transition)
          (dissoc :signal)))))


(defn apply-signal!
  "Apply signal `signal` to given `fsm` and return updated `fsm`. Throws
   an exception if no transition was found for given signal"
  [fsm signal]
  (let [fsm        (assoc fsm :signal signal)
        transition (get-transition fsm)]
    (when-not transition
      (throw (ex-info (sprintf "fsm: error: no transition for signal %s" signal)
                      {:fsm    fsm
                       :signal signal})))
    (-> (apply-transition fsm transition)
        (dissoc :signal))))


(defn dry-run [fsm signal]
  (let [fsm (assoc fsm :signal signal)]
    {:state       (-> fsm :state)
     :signal      signal
     :transitions (->> (transitions-for-signal fsm)
                       (map (fn [transition]
                              (let [apply-handler (-> fsm :apply-fx (or default-apply-fx))
                                    guards        (->> (guards-for-transition fsm transition)
                                                       (map (fn [guard]
                                                              [guard (boolean (apply-handler fsm guard))])))
                                    allowed?      (->> guards
                                                       (map second)
                                                       (every? true?))]
                                {:to       (-> transition :to)
                                 :allowed? allowed?
                                 :guards   guards
                                 :fx       (when allowed?
                                             (-> (apply-transition fsm transition)
                                                 :fx))}))))}))


(comment

  (defn pass [_fsm & _args] true)
  (defn reject [_fsm & _args] false)

  (let [fsm {:state  :init
             :states {:init {:on ["a" {:guards [[#'pass :a]]}
                                  "b" {:to     :next
                                       :guards [[#'pass :b]]}
                                  "b" {:to     :fofo
                                       :guards [[#'reject :b]]}]}}}]
    (-> (analyze fsm "b")
        :transitions))

  {:state       :init
   :signal      "b"
   :transitions ({:to       :next
                  :allowed? true
                  :guards   ([[#'fsm.core/pass :super] true] [[#'fsm.core/pass :b] true])
                  :fx       [:init-leave :next-enter :super :b :default]}
                 {:to       :fofo
                  :allowed? false
                  :guards   ([[#'fsm.core/pass :super] true] [[#'fsm.core/reject :b] false])
                  :fx       nil})}


  {:state       :init
   :signal      "b"
   :transitions ({:to       :next
                  :allowed? true
                  :guards   ([[#'fsm.core/pass :super] true] [[#'fsm.core/pass :b] true])
                  :fx       [:init-leave :next-enter :super :b :default]})}


  {:state       :init
   :signal      "a"
   :transitions ({:to       nil
                  :allowed? true
                  :guards   ([[#'fsm.core/pass :super] true]
                             [[#'fsm.core/pass :a] true])
                  :fx       [:init-stay :super :a :default]})}


  {:state       :init
   :signal      "a"
   :transitions ({:to       nil
                  :allowed? false
                  :guards   ([[#'fsm.core/pass :super] false] [[#'fsm.core/pass :a] false])
                  :fx       nil})}


  {:state       ...
   :signal      ...
   :transitions [{:to      ...
                  :ok      false
                  :guards  [['guard-1 true]
                            ['guard-2 false]]
                  :handler ...
                  :fx      ...}
                 ...]}

  (def data {:fsm    {:state  :init
                      :states {:init {:on [#{"a"} {:to :s-1}
                                           #{"a"} {:to :s-2}
                                           #{"b"} {:to :s-3}]}}}
             :signal "a"})


  (require 'clojure.pprint)

  (defn test-analyze [_ _ _ f]
    (let [{:keys [fsm signal]} (deref #'data)]
      (println "\n\nanalyze:\n"
               "signal: " signal "\n"
               (-> (f fsm signal)
                   :transitions
                   clojure.pprint/pprint
                   (with-out-str)))))


  (do (add-watch #'data :foo #'test-analyze)
      (add-watch #'analyze :foo #'test-analyze))

  (do (remove-watch #'data :foo)
      (remove-watch #'analyze :foo))



  (remove-watch #'analyze :foo)

  ;
  )