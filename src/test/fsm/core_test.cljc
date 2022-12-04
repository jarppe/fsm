(ns fsm.core-test
  (:require [clojure.test :as test :refer [deftest is are testing]]
            [match.core :refer [matches?]]
            [fsm.core :as fsm]))

(comment

  ; FSM shape:
  {:id      'optional-fsm-id
   :state   'current-state
   :states  {:state-id {:guards ['guard...]
                        :on     ['signal-matchder {:to        'state
                                                   '|| ['state 'state...]
                                                   :guards    ['guard...]
                                                   :fx        ['fx...]}
                                 ...]
                        :enter  {:guards ['guard...]
                                 :fx     ['fx...]}
                        :stay   {:guards ['guard...]
                                 :fx     ['fx...]}
                        :leave  {:guards ['guard...]
                                 :fx     ['fx...]}}}
   :super   {:guards ['guard...]
             :fx     ['fx...]
             :on     ...}
   :default {:guards ['guard...]
             :fx     ['fx...]
             :on     ...}})


(deftest signal-matches?-test
  (are [target result]
       (= result
          (fsm/signal-matches? "hello"
                               target))
    "hello" true
    "world" false
    #{"hello"} true
    #{"world"} false
    (partial = "hello") true
    (partial = "world") false))


(deftest transitions-for-signal-test
  (let [fsm {:state  :init
             :states {:init {:on ["a" {:id "a"}
                                  "b" {:id "b"}]}}}]
    (is (matches? [{:id "a"}]
                  (fsm/get-transitions-for-signal fsm "a"))))
  (let [fsm {:state  :init
             :states {:init {:on ["a" {:id "a1"}
                                  "a" {:id "a2"}]}}}]
    (is (matches? [{:id "a1"}
                   {:id "a2"}]
                  (fsm/get-transitions-for-signal fsm "a"))))
  (let [fsm {:state   :init
             :states  {:init {:on ["a" {:id :a}
                                   "b" {:id :b}]}}
             :super   {:on ["a" {:id :super-a}
                            "b" {:id :super-b}]}
             :default {:on ["a" {:id :default-a}
                            "b" {:id :default-b}]}}]
    (is (matches? [{:id :super-a}
                   {:id :a}
                   {:id :default-a}]
                  (fsm/get-transitions-for-signal fsm "a")))))


(deftest guards-for-transition-test
  (let [fsm        {:state   :init
                    :super   {:guards [:super]}
                    :states  {:init {:on    ["a" {:guards [:init-a]}
                                             "b" {:to     :next
                                                  :guards [:init-b]}]
                                     :enter {:guards [:init-enter]}
                                     :stay  {:guards [:init-stay]}
                                     :leave {:guards [:init-leave]}}
                              :next {:enter {:guards [:next-enter]}
                                     :stay  {:guards [:next-stay]}
                                     :leave {:guards [:next-leave]}}}
                    :default {:guards [:default]}}]
    (testing "stay in state :init"
      (is (matches? [:super
                     :init-stay
                     :init-a
                     :default]
                    (->> (fsm/get-transitions-for-signal fsm "a")
                         (first)
                         (fsm/guards-for-transition fsm)))))
    (testing "transition to state :next"
      (is (matches? [:super
                     :init-leave
                     :init-b
                     :next-enter
                     :default]
                    (->> (fsm/get-transitions-for-signal fsm "b")
                         (first)
                         (fsm/guards-for-transition fsm)))))))

;; fx-for-transition-test has same implementation as guards-for-transition

(deftest transitions-test
  (let [fsm   {:state  :init
               :states {:init {:on ["a" {:to     :foo
                                         :guards [:guard-1]
                                         :fx     [:fx-1]}
                                    "a" {:to     :bar
                                         :guards [:guard-2]
                                         :fx     [:fx-2]}]}}}]
    (is (matches? [{:transition {:to :foo}
                    :guards     [:guard-1]
                    :fx         [:fx-1]}
                   {:transition {:to :bar}
                    :guards     [:guard-2]
                    :fx         [:fx-2]}]
                  (fsm/get-transitions fsm "a"))))
  (let [fsm   {:state   :init
               :states  {:init {:on    ["a" {:to     :foo
                                             :guards [:guard-1]
                                             :fx     [:fx-1]}
                                        "a" {:to     :bar
                                             :guards [:guard-2]
                                             :fx     [:fx-2]}]
                                :leave {:guards [:leave-a]
                                        :fx     [:leave-a]}}}
               :super   {:guards [:guard-super]
                         :fx     [:fx-super]}
               :default {:guards [:guard-default]
                         :fx     [:fx-default]}}]
    (is (matches? [{:transition {:to :foo}
                    :guards     [:guard-super
                                 :leave-a
                                 :guard-1
                                 :guard-default]
                    :fx         [:fx-super
                                 :leave-a
                                 :fx-1
                                 :fx-default]}
                   {:transition {:to :bar}
                    :guards     [:guard-super
                                 :leave-a
                                 :guard-2
                                 :guard-default]
                    :fx         [:fx-super
                                 :leave-a
                                 :fx-2
                                 :fx-default]}]
                  (fsm/get-transitions fsm "a")))))


(deftest transition-test
  (let [fsm   {:state  :init
               :states {:init {:on ["a" {:to     :foo
                                         :guards [:guard-1]
                                         :fx     [:fx-1]}
                                    "a" {:to     :bar
                                         :guards [:guard-2]
                                         :fx     [:fx-2]}]}}}]
    (is (matches? {:transition {:to :foo}}
                  (fsm/get-transition (fn [fsm' guard]
                                        (is (-> fsm' :signal (= "a")))
                                        (= guard :guard-1))
                                      fsm
                                      "a")))
    (is (matches? {:transition {:to :bar}}
                  (fsm/get-transition (fn [fsm' guard]
                                        (is (-> fsm' :signal (= "a")))
                                        (= guard :guard-2))
                                      fsm
                                      "a")))
    (is (matches? nil
                  (fsm/get-transition (constantly false)
                                      fsm
                                      "a"))))
  (let [fsm   {:state   :init
               :states  {:init {:on    ["a" {:to     :foo
                                             :guards [:guard-1]
                                             :fx     [:fx-1]}
                                        "a" {:to     :bar
                                             :guards [:guard-2]
                                             :fx     [:fx-2]}]
                                :leave {:guards [:leave-a]
                                        :fx     [:leave-a]}}}
               :super   {:guards [:guard-super]
                         :fx     [:fx-super]}
               :default {:guards [:guard-default]
                         :fx     [:fx-default]}}]
    (is (matches? {:transition {:to :foo}}
                  (fsm/get-transition (fn [_fsm guard]
                                        (not (= guard :guard-2)))
                                      fsm
                                      "a")))
    (is (matches? {:transition {:to :bar}}
                  (fsm/get-transition (fn [_fsm guard]
                                        (not (= guard :guard-1)))
                                      fsm
                                      "a")))
    (is (matches? nil
                  (fsm/get-transition (constantly false)
                                      fsm
                                      "a")))))


(deftest apply-signal-test
  (let [fsm        {:state     :init
                    :super     {:fx [[update :fx conj :super]]}
                    :states    {:init {:on    ["a" {:fx [[update :fx conj :a]]}
                                               "b" {:to :next
                                                    :fx [[update :fx conj :b]]}]
                                       :stay  {:fx [[update :fx conj :init-stay]]}
                                       :leave {:fx [[update :fx conj :init-leave]]}}
                                :next {:enter {:fx [[update :fx conj :next-enter]]}}}
                    :default   {:fx [[update :fx conj :default]]}
                    :handle-fx (fn [fsm [f & args]]
                                 (apply f fsm args))
                    :fx        []}]
    (testing "stay in state :init"
      (is (matches? {:fx [:super
                          :init-stay
                          :a
                          :default]}
                    (fsm/apply-signal fsm "a"))))
    (testing "transition to state :next"
      (is (matches? {:fx [:super
                          :init-leave
                          :b
                          :next-enter
                          :default]}
                    (fsm/apply-signal fsm "b"))))))

(deftest apply-signal-with-state-stack-test
  (let [fsm        {:state  :a
                    :states {:a {:on ["x" {:to [:b :c]}]}
                             :b {:on ["x" {:to ::fsm/pop}]}
                             :c {}}}]
    (is (matches? {:state [:b :c]}
                  (reduce fsm/apply-signal fsm ["x"])))
    (is (matches? {:state [:c]}
                  (reduce fsm/apply-signal fsm ["x" "x"]))))

  (let [fsm        {:state  :a
                    :states {:a {:on ["x" {:to [:b :c]}]}
                             :b {:on ["x" {:to :d}]}
                             :c {}
                             :d {:on ["x" {:to ::fsm/pop}]}}}]
    (is (matches? {:state [:b :c]}
                  (reduce fsm/apply-signal fsm ["x"])))
    (is (matches? {:state [:d :c]}
                  (reduce fsm/apply-signal fsm ["x" "x"])))
    (is (matches? {:state [:c]}
                  (reduce fsm/apply-signal fsm ["x" "x" "x"]))))

  (let [fsm        {:state  :a
                    :states {:a {:on ["x" {:to [:b :c]}]}
                             :b {:on ["x" {:to ::fsm/pop}]}
                             :c {:on ["x" {:to ::fsm/pop}]}}}]
    (is (matches? {:state [:b :c]}
                  (reduce fsm/apply-signal fsm ["x"])))
    (is (matches? {:state [:c]}
                  (reduce fsm/apply-signal fsm ["x" "x"])))
    (is (matches? (ex-info "fsm: error: transition using pop state, but stack is empty" {})
                  (reduce fsm/apply-signal fsm ["x" "x" "x"])))))
