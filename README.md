# FSM

Small Clojure and ClojureScript [finite state machine](https://en.wikipedia.org/wiki/Finite-state_machine).

## Features

- [FSM represented as pure data representation](#fsm-represented-as-pure-data-representation)
- [Clojure and ClojureScript support](#clojure-and-clojurescript-support)
- [Guards](#guards)
- [Super and Default states](#super-and-default-states)
- [No side-effects](#no-side-effects) (see below)
- [Dry-run](#dry-run)
- [Extendable](#extendable)

## Quick example

```clj
(ns examples.quick
  (:require [fsm.core :as fsm]))

(def fsm {:state  :flip
          :states {:flip {:on ["ping" {:to :flop}]}
                   :flop {:on ["ping" {:to :flip}]}}})

(-> fsm :state)
;;=> :flip

(-> (fsm/apply-signal fsm "ping")
    :state)
;;=> :flop

(-> (reduce fsm/apply-signal
            fsm
            ["ping" "ping" "ping"])
    :state)
;;=> :flop
```

## Rationale

State machines are an excellent tool that can reduce code complexity. They can be used in many places where external stimuli should cause well defined state changes. Protocol parsers are an obvious use-case, but so are UI components, for example see [Qt State Machine Overview](https://doc.qt.io/qt-6/qtstatemachine-overview.html) and [Tackling UI complexity with State Machines](https://medium.com/@carloslfu/tackling-ui-complexity-with-state-machines-b3f1eb6d1a97).

# FSM represented as pure data representation

This FSM implementation is idiomatic Clojure implementation with emphasis on pure data and handling of side effects.

Representing the state machine with pure data has several benefits. For example, the state machine can easily be serialized to and from database or from backend to frontend.

Also, applications can modify the FSM using basic Clojure functions like alloc and update.

## Clojure and ClojureScript support

Being able to execute the same FSM in frontend and backend is very useful in many cases where applications can check what signals are allowed to run before submitting the signals to backend.

## Guards

Guards can be used to prevent state changes. For example:

```clj
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
```

Guards can be registered to transitions (as seen above) and to the states.
Guards that are in state level apply to all transitions in that state.

## Super and default states

There are two special states, the `:super` state, and the `:default`
state.

When FSM applies the signal to FSM, it searches potential transitions
first from the `:super` state, the from current state, and finally
from `:default` state.

This order means that ...

## No side-effects

Applying signals to FSM should not cause any side effects. This means that
your guards and transition handlers should be pure functions.

Side effects should be done by adding data to the FSM, and execute side
effects after the signal has been processed. For example:

```clj
(def fsm {:state  :init
          :states {:init {:on ["hello" {:to :end
                                        :fx [[update :fx conj [:side-effect "hello"]]]}]}
                   :end  {:enter [[update :fx conj [:the-end]]]}}})

(-> (fsm/apply-signal fsm "hello")
    :fx)
;; => [[:side-effect "hello"] [:the-end]]
```

The `:fx` is initialized to have an empty vector when applying signals.
After the application of the signal the `:fx` contains what ever your
handlers have put in it.

## Dry-run

...todo...

## Extendable

...todo...
