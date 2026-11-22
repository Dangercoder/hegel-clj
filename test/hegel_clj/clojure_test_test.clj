(ns hegel-clj.clojure-test-test
  (:require [clojure [pprint :refer [pprint]]
                     [test :refer :all]]
            [clojure.tools.logging :refer [info warn]]
            [hegel-clj [generator :as g]
                       [clojure-test :refer [with control-flow?]]
                       [core :as h]])
  (:import (dev.hegel AssumeRejected
                      HegelException)
           (java.io StringWriter)
           (org.opentest4j AssertionFailedError)))

; This namespace is a little weird. We're going to use clojure.test to test
; clojure.test, so we actually use (test-ns-hook) as our test entrypoint, and
; then invoke clojure.test *again* from there to run specific test cases and
; assert they print the right things.
;
; We name our sub-level tests `foo-test-`, and the wrapper test `foo-test`.

(defmacro capture
  "Evaluates body, presumably running clojure.test assertions. Captures stdout,
  stderr, and clojure.test reports to strings and a vector, respectively.
  Returns a map of :out, :err, :reports."
  [& body]
  `(let [out# (StringWriter.)
         err# (StringWriter.)
         reports# (atom [])
         report# (fn ~'report [event#]
                   (case (:type event#)
                     :begin-test-var nil
                     :end-test-var   nil
                     (swap! reports# conj event#)))]
     (binding [*out*               out#
               *err*               err#
               clojure.test/report report#]
       ~@body)
     {:out (str out#)
      :err (str err#)
      :reports @reports#}))

; This test is here for us to run as a part of reverse-test
(deftest reverse-test*
  ; Generate integers of vectors
  (with {:test-cases 10, :seed 1}
    [xs (g/vector (g/integer))]
    ;(info (when (h/final?) :final)
    ;      :xs xs
    ;      (if (= (sort xs) (reverse xs))
    ;        :pass
    ;        :fail))
    ; Print out only the smallest vectors that make this fail
    (when (h/final?) (prn :final-xs xs))
    ; Is sorting the same as reversing? Clearly not, so this will fail,
    ; likely with a two-element vector of different numbers.
    (is (= (sort xs)
           (reverse xs)))))

(deftest reverse-test
  (let [{:keys [out err reports]} (capture (reverse-test*))
        res (mapv (juxt :type :expected :actual) reports)]
    ; Note that we only tell clojure.test about the shrunk case.
    ;(pprint res)
    ; I'm not sure why Hegel runs several final cases here
    (is (= '[[:fail
              (= (sort xs) (reverse xs))
              (not (= (-13129 8191 93150831) (93150831 8191 -13129)))]
             [:fail
              (= (sort xs) (reverse xs))
              (not (= (-13129 8191 93150831) (93150831 8191 -13129)))]
             [:fail
              (= (sort xs) (reverse xs))
              (not (= (-13129 8191 93150831) (93150831 8191 -13129)))]
             [:fail
              (= (sort xs) (reverse xs))
              (not (= (-13129 8191 93150831) (93150831 8191 -13129)))]
             [:fail (= (sort xs) (reverse xs)) (not (= (0 1) (1 0)))]]
           res))
    (is (= ":final-xs [-13129 8191 93150831]
:final-xs [-13129 8191 93150831]
:final-xs [-13129 8191 93150831]
:final-xs [-13129 8191 93150831]
:final-xs [0 1]\n"
           out))
    (is (= "" err))))

; Exceptions thrown by the body of a `with`, rather than failed assertions,
; must fail the test too. These sub-tests check that they're reported to
; clojure.test, and that Hegel's own control flow isn't.

(defn uncaught-error?
  "Is this clojure.test report an exception thrown by a test case?"
  [report]
  (and (= :error (:type report))
       (= "Uncaught exception in test case" (:message report))
       (nil? (:expected report))
       (instance? Throwable (:actual report))))

(deftest throw-some-test*
  (with {:test-cases 50, :seed 1}
    [x (g/integer)]
    (when (< 10 x)
      (throw (ex-info "too big" {:x x})))
    (is (integer? x))))

(deftest throw-some-test
  (let [{:keys [reports]} (capture (throw-some-test*))]
    ; Hegel replays a few final cases; each should report the exception.
    (is (seq reports))
    (is (every? uncaught-error? reports))
    ; Hegel still shrinks on the exception, so the last report is for the
    ; smallest input which throws.
    (is (= {:x 11} (ex-data (:actual (peek reports)))))))

(deftest throw-all-test*
  (with {:test-cases 20, :seed 1}
    [x (g/integer)]
    ; A bug in the test itself: there's no :y, so this throws a
    ; NullPointerException before we ever reach an assertion.
    (let [y (inc (:y {:x x}))]
      (is (= (inc x) y)))))

(deftest throw-all-test
  ; This used to report nothing at all, so the test passed while testing
  ; nothing.
  (let [{:keys [reports]} (capture (throw-all-test*))]
    (is (seq reports))
    (is (every? uncaught-error? reports))
    (is (every? (comp (partial instance? NullPointerException) :actual)
                reports))))

(deftest assume-test*
  (with {:test-cases 50, :seed 1}
    [x (g/integer)]
    (h/assume! (even? x))
    (is (even? x))))

(deftest assume-test
  ; Rejected test cases are not errors.
  (is (= {:out "", :err "", :reports []}
         (capture (assume-test*)))))

(deftest pass-test*
  (with {:test-cases 50, :seed 1}
    [x (g/integer)]
    (is (integer? x))))

(deftest pass-test
  (is (= {:out "", :err "", :reports []}
         (capture (pass-test*)))))

(deftest flaky-test*
  (with {:test-cases 20, :seed 1}
    [x (g/integer)]
    ; Fails every time, except when Hegel replays the final case.
    (when-not (h/final?)
      (throw (ex-info "flaky" {})))))

(deftest flaky-test
  ; Hegel says this test failed, but the final replay reported nothing to
  ; clojure.test, so `with` has to.
  (let [{:keys [reports]} (capture (flaky-test*))]
    (is (= [[:error "Hegel test failed, but reported no failure to clojure.test"]]
           (mapv (juxt :type :message) reports)))
    (is (= "flaky" (ex-message (:actual (first reports)))))))

(deftest health-check-test*
  (with {:test-cases 20, :seed 1}
    [x (g/integer)]
    (h/assume! false)))

(deftest health-check-test
  ; Every test case is rejected, so Hegel errors with a health check failure.
  ; No test case could report that, so `with` has to.
  (let [{:keys [reports]} (capture (health-check-test*))]
    (is (= [[:error "Hegel test errored"]]
           (mapv (juxt :type :message) reports)))
    (is (re-find #"FilterTooMuch" (ex-message (:actual (first reports)))))))

(deftest control-flow-test
  (let [stop-test (doto (.getDeclaredConstructor
                          (Class/forName "dev.hegel.StopTest")
                          (make-array Class 0))
                    (.setAccessible true))]
    (is (control-flow? (.newInstance stop-test (object-array 0)))))
  (is (control-flow? (AssumeRejected.)))
  (is (control-flow? (HegelException. "engine trouble")))
  (is (not (control-flow? (ex-info "boom" {}))))
  (is (not (control-flow? (NullPointerException.))))
  (is (not (control-flow? (AssertionFailedError. "nope")))))

(defn test-ns-hook
  "Main entrypoint for tests. The other deftests don't get called directly; we
  run them in a little sandbox to make sure they fail, but fail in specific ways."
  []
  (reverse-test)
  (throw-some-test)
  (throw-all-test)
  (assume-test)
  (pass-test)
  (flaky-test)
  (health-check-test)
  (control-flow-test))
