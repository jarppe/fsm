(ns fsm.core
  (:require [fsm.impl.util :refer [sprintf]]))


(def pop-state ::pop)
(def pushback ::pushback)


(defn- default-allow? [fsm guard]
  (cond
    (vector? guard) (apply (first guard) fsm (rest guard))
    (fn? guard) (guard fsm)
    :else (throw (ex-info (sprintf "fsm: default-allow?: don't know how to handle guard: %s" (pr-str guard))
                          {:fsm   fsm
                           :guard guard}))))


(defn- default-handle-fx [fsm fx]
  (cond
    (= fx ::pushback)
    (update fsm ::pushback conj (-> fsm :signal))

    (and (vector? fx)
         (= (first fx) ::pushback))
    (update fsm ::pushback concat (map (fn [v]
                                         (if (= v ::signal)
                                           (:signal fsm)
                                           v))
                                       (rest fx)))

    (vector? fx)
    (apply (first fx) fsm (map (fn [v]
                                 (if (= v ::signal)
                                   (:signal fsm)
                                   v))
                               (rest fx)))

    (fn? fx)
    (fx fsm)

    :else
    (throw (ex-info (sprintf "fsm: default-handle-fx: don't know how to handle fx: %s" (pr-str fx))
                    {:fsm fsm
                     :fx  fx}))))


(defn- coerce-seq [v]
  (cond
    (nil? v) nil
    (seq? v) v
    (sequential? v) (seq v)
    :else (cons v nil)))


(defn signal-matches?
  "Default function used to match signals"
  [signal matcher]
  (if (or (= signal matcher)
          (and (ifn? matcher)
               (matcher signal)))
    true
    false))


(defn get-transitions-for-signal
  "Return all possible transitions from current state with given signal"
  [fsm signal]
  (let [current-state (-> fsm :state (coerce-seq) (first))]
    (->> (concat (-> fsm :super :on)
                 (-> fsm :states (get current-state) :on)
                 (-> fsm :default :on))
         (partition 2)
         (keep (fn [[matcher transition]]
                 (when (signal-matches? signal matcher)
                   transition))))))


(defn- transition-elements [fsm transition element-key]
  (let [current-state (-> fsm :state (coerce-seq) (first))
        next-state    (-> transition :to (coerce-seq) (first) (or current-state))
        move?         (not= current-state next-state)]
    (concat (-> fsm :super element-key)
            (when-not move?
              (-> fsm :states (get current-state) :stay element-key))
            (when move?
              (-> fsm :states (get current-state) :leave element-key))
            (-> transition element-key)
            (when move?
              (-> fsm :states (get next-state) :enter element-key))
            (-> fsm :default element-key))))


(defn guards-for-transition
  "Returns all guards for a given transition"
  [fsm transition]
  (transition-elements fsm transition :guards))


(defn fx-for-transition
  "Returns all effects for a given transition"
  [fsm transition]
  (transition-elements fsm transition :fx))


(defn get-transitions 
  "Returns a seq of possible transitions with provided signal. Each 
   element in the seq has following elements:
     `:transition` The transition from the fsm that is possible
     `:guards`     A seq of guards for this transition
     `:fx`         A seq of effects fot this transition
   The transitions are returned in following order:
     * possible transitions from super state in the order
       they are declared in fsm
     * possible transitions from current state in the
       order they are declared in fsm
     * possible transitions from default state in the
       order they are declared in fsm"
  [fsm signal]
  (->> (get-transitions-for-signal fsm signal)
       (map (fn [transition]
              {:transition transition
               :guards     (guards-for-transition fsm transition)
               :fx         (fx-for-transition fsm transition)}))))


;;
;; Above pure stateless stuff, below more and more non-pureness starts to 
;; show
;;


(defn get-transition 
  "Find first transition for given signal that is permitted by all
   applicable guards, or `nil` if no transition is available for given
   signal. 
   
   Two arguments version checks quards using the default guard function.
   It accepts guards that are wither a vector, or a function. See the
   documentation TODO for more information.
   
   Three arg version allows specifying the `allow?` function. 
   It must accept two arguments, first is the fsm and the second is the 
   guard data from the fsm. The signal is available in the fsm at 
   `:signal`. The `guard-allow?` must return truthy if the guard data 
   allows transition."
  ([fsm signal] (get-transition default-allow? fsm signal))
  ([allow? fsm signal]
   (let [allow? (partial allow? (assoc fsm :signal signal))]
     (->> (get-transitions fsm signal)
          (some (fn [transition]
                  (when (every? allow? (-> transition :guards))
                    transition)))))))


(defn get-transition!
  "Same as `fsm.core/get-transition`, but throws an exception if transition is
   not found."
  ([fsm signal] (get-transition! default-allow? fsm signal))
  ([allow? fsm signal]
   (or (get-transition allow? fsm signal)
       (throw (ex-info (sprintf "fsm: error: transitionin not found for signal: %s" (pr-str signal))
                       {:fsm    fsm
                        :signal signal})))))


(defn- pop-state! [states]
  (or (next states)
      (throw (ex-info "fsm: error: transition using pop state, but stack is empty" {}))))


(def ^:private default-opts {:allow?    default-allow?
                             :handle-fx default-handle-fx})

(defn apply-signal
  "Apply given signal to fsm, executes fx effects and returns possibly updated fsm."
  ([fsm signal] (apply-signal default-opts fsm signal))
  ([{:as opts :keys [allow? handle-fx]} fsm signal] 
   (let [transition-info (get-transition! allow? fsm signal) 
         prev-states     (-> fsm :state (coerce-seq)) 
         next-states     (let [to (-> transition-info :transition :to (coerce-seq))]
                           (cond
                             (nil? to) prev-states
                             (= (first to) ::pop) (concat (pop-state! prev-states)
                                                          (next to))
                             :else (concat to (next prev-states)))) 
         prev-state      (first prev-states) 
         next-state      (first next-states) 
         _               (when-not (contains? (-> fsm :states) next-state)
                           (throw (ex-info (sprintf "fsm: error: transitioning to unknown state: %s" next-state)
                                           {:fsm        fsm
                                            :signal     signal
                                            :state      prev-state
                                            :next-state next-state}))) 
         fxs             (-> transition-info :fx)
         fsm'            (-> (reduce handle-fx
                                     (assoc fsm
                                            :state next-states
                                            :signal signal
                                            :prev-state prev-state
                                            :next-state next-state)
                                     fxs)
                             (assoc :state next-states)
                             (dissoc :signal :prev-state :next-state))] 
     (if-let [pushback (seq (::pushback fsm'))]
         (recur opts
                (assoc fsm' ::pushback (next pushback))
                (first pushback))
         (dissoc fsm' ::pushback)))))


(defn machine [fsm]
  (let [fsm (atom fsm)]
    (fn [signal]
      (swap! fsm apply-signal signal))))
