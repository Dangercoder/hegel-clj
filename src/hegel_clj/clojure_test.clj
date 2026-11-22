(ns hegel-clj.clojure-test
  "Hegel integration with clojure.test."
  (:require [hegel-clj [generator :as g]
                       [core :as h]]
            [clojure [pprint :refer [pprint]]
                     [test :as ct]]
            [clojure.tools.logging :refer [info warn]])
  (:import (dev.hegel AssumeRejected)
           (dev.hegel.lowlevel LibhegelException)
           (org.opentest4j AssertionFailedError)))

(defn pass?
  "Did every clojure.test report pass?"
  [reports]
  (every? (comp #{:pass} :type) reports))

(defmacro capture-reports
  "Evaluates body for clojure.test reports as a side effect. Returns a vector
  of those reports. In the final test phase, also sends those final reports to
  the normal clojure.test reporter."
  [& body]
  `(let [og-report# ct/report
         reports# (atom [])
         report# (fn ~'report [event#]
                   (when (h/final?) (og-report# event#))
                   (swap! reports# conj event#))]
     (binding [ct/report report#]
       ~@body)
     @reports#))

(defn origin
  "Produces an origin string for a set of reports. Hegel is... vague about what
  the origin should be; you don't want to be *too* specific or else it will
  explode the state space. I think just the file and line, perhaps?"
  [reports]
  (let [report (->> reports
                    (remove (comp #{:pass} :type))
                    first)]
    (str (:file report) ":" (:line report))))

(def ^:private stop-test-class
  "dev.hegel.StopTest is package-private, so we can't name it in a catch
  clause; we look it up reflectively instead."
  (Class/forName "dev.hegel.StopTest"))

(defn control-flow?
  "Hegel throws some exceptions through test cases which don't mean the test
  failed: AssumeRejected (from `hegel-clj.core/assume!`), StopTest (when a
  test case runs out of data), and LibhegelException (errors from the engine
  itself, including HegelException and HealthCheckFailure). These need to
  propagate to Hegel untouched and unreported."
  [t]
  (or (instance? AssumeRejected t)
      (instance? stop-test-class t)
      (instance? LibhegelException t)))

(defn report-uncaught!
  "Called with an exception thrown by a test case body. Unless it's Hegel
  control flow, reports it in the final phase the way deftest reports an
  uncaught exception. Hegel still gets the exception (the caller rethrows it),
  so it shrinks on it as usual."
  [t]
  (when (and (not (control-flow? t)) (h/final?))
    (ct/do-report {:type     :error
                   :message  "Uncaught exception in test case"
                   :expected nil
                   :actual   t})))

(defn report-unreported-failure!
  "Takes a result map from `hegel-clj.core/test!` which did not pass, and for
  which nothing reported a failure to clojure.test in the final phase (e.g. a
  flaky test, or a health check failure). Reports an error for each failure,
  and for any engine error, so the test can't look green."
  [res]
  (let [summary (dissoc res :failures)
        errors  (cond-> (mapv (fn [{:keys [exception]}]
                                [(str "Hegel test failed, but reported no "
                                      "failure to clojure.test")
                                 (or exception
                                     (ex-info "Hegel test failed" summary))])
                              (:failures res))
                  (:error res)
                  (conj ["Hegel test errored"
                         (ex-info (:error res) summary)]))
        errors  (if (seq errors)
                  errors
                  [["Hegel test did not pass"
                    (ex-info "Hegel test did not pass" summary)]])]
    (doseq [[message e] errors]
      (ct/do-report {:type     :error
                     :message  message
                     :expected nil
                     :actual   e}))))

(defmacro with
  "Can be embedded within a clojure.test deftest to generate some series of
  values and test whether they produce correct results. Takes an options map
  for `hegel-clj.test/test!*`, a binding vector, then a body. Evaluates
  body roughly `test-cases` times, with bindings provided by
  `hegel-clj.core/let`. You can generate more values using `hegel-clj.core/draw!` or
  `hegel-clj.core/let`, if needed. Make test assertions using `clojure.test/is`, as
  usual. Failing tests will be automatically shrunk and re-run with minimal
  examples. Log these examples using `note`.

  For example:

      (deftest reverse-test
        ; Generate roughly a hundred integers of vectors
        (with {:test-cases 100, :seed 5}
              [xs (gen/vector (g/integer))]

          ; Print out only the smallest vectors that make this fail
          (fprn :xs xs)

          ; Is sorting the same as reversing? Clearly not, so this will fail,
          ; likely with a two-element vector of different numbers.
          (is (= (sort xs)
              (reverse xs)))))

  `with` suppresses clojure.test reporting until the final phase, where it
  reports as normal. You can broadly pretend that this is a clojure.test case
  where you just happened to guess the right small inputs to reproduce a
  failing bug.

  An exception thrown by body (other than Hegel's own control flow, like
  `assume!`) fails the test case, just like a failed assertion. In the final
  phase it is reported to clojure.test as an error, as deftest would."
  [opts bindings & body]
  `(let [og-report# ct/report
         ; Did a non-passing report reach clojure.test in the final phase?
         reported?# (atom false)
         res# (binding [ct/report (fn ~'report [event#]
                                    (when-not (= :pass (:type event#))
                                      (reset! reported?# true))
                                    (og-report# event#))]
                (h/test! ~opts
                         ; Generate values and evaluate body, recording
                         ; clojure.test reports
                         (let [reports# (capture-reports
                                          (try (h/let ~bindings
                                                 ~@body)
                                               (catch Throwable t#
                                                 ; Report it in the final
                                                 ; phase, and let Hegel see
                                                 ; it too.
                                                 (report-uncaught! t#)
                                                 (throw t#))))]
                           ; If every clojure.test assertion passed, this test
                           ; case does too.
                           (if (pass? reports#)
                             nil
                             ; Otherwise, tell Hegel about it.
                             (throw (AssertionFailedError.
                                      (origin reports#)))))))]
     ; If Hegel found a problem but nothing told clojure.test about it, say so
     ; now, rather than letting the test pass.
     (when-not (or (:passed? res#) @reported?#)
       (report-unreported-failure! res#))
     res#))
