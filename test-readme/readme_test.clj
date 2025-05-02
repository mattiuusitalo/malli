(ns readme-test
  (:require  [clojure.test :as t]
             [clojure.pprint :refer [pprint]]
             [clojure.stacktrace]
             [clojure.string :as str]
             [malli.registry :as mr]
             ;; Following namespaces & aliases are used in the examples
             [malli.core :as m]
             [malli.error :as me]
             [malli.util :as mu]
             [malli.transform :as mt]
             [malli.generator :as mg]
             [malli.provider :as mp]
             [jsonista.core]))

(def default-registry
  @(var-get #'mr/registry*))

(defn remove-leading-comment-chars [s]
  (str/replace s #"(?m)^\s*;+" ""))

(def assertion-regex #"(?m);+\s*=>\s*")
(def pre-execution-setup "(in-ns 'readme-test)
(mr/set-default-registry! default-registry)\n")

(defn prepend-pre-exeution-setup [s]
  (str pre-execution-setup s))

(defn executable-fragments [example]
  (if-not (re-find assertion-regex example)
    [{:form example}]
    (as-> example s
      (str/split s assertion-regex)
      (partition 2 1 s)
      (map-indexed
       (fn [i [form expected]]
         {:form (if (zero? i) form
                    (second (str/split form #"\n+" 2)))
          :expected-result
          (-> (str/split expected #"\n\n") first str/trim remove-leading-comment-chars)}) s))))

(defn canonicalize-evaluation-results [clj-data]
  (->> clj-data
       pr-str
       (str "'")
       load-string))

(defn canonicalize-expectation [clj-str]
  (read-string clj-str))

(defn canonically-evaluate [form]
  (->> form prepend-pre-exeution-setup load-string canonicalize-evaluation-results))

(defn collect-exception [t form]
  {:pass? :exception
   :throwable t
   :form form})

(defn evaluate-setup-fragment [form]
  (try {:pass? :pass :form form :result
        (canonically-evaluate form)}
       (catch Throwable t
         (collect-exception t form))))

(defn evaluate-assertable-fragment [form expected]
  (try
    (let [result (canonically-evaluate form)
          canonicalized-expected (canonicalize-expectation expected)
          pass? (= result
                   canonicalized-expected)]
      {:pass? (if pass? :pass :fail)
       :form form
       :result result
       :expected canonicalized-expected})
    (catch Throwable t
      (collect-exception t form))))

(defn evaluate-fragment [{form :form expected :expected-result}]
  (if-not expected
    (evaluate-setup-fragment form)
    (evaluate-assertable-fragment form expected)))

(defn overall-result [fragment-results]
  (let [result-classes (->> fragment-results
                            (into #{} (map :pass?)))]
    (cond
      (contains? result-classes :exception) :exception
      (contains? result-classes :fail) :fail
      :else :pass)))

(defn evaluate-example [example]
  (let [fragment-results
        (->> example
             executable-fragments
             (map evaluate-fragment))]
    {:overall-result (overall-result fragment-results)
     :throwable (->> fragment-results (map :throwable) first)
     :fragment-results fragment-results}))

(def examples
  (->> (re-seq #"(?ms)```clojure(?! notest).*?```" (slurp "README.md"))
       (map #(str/replace %  #"```(clojure)?" ""))))

(defn pprint-str [clj] (with-out-str (pprint clj)))

(defn summarize-results [results]
  (->> results
       (group-by :overall-result)
       (into {:total (count results)} (map (fn [[k v]] [k (count v)])))))

(defn pprint-results [{:keys [total pass fail exception]}]
  (format "Total: %d Pass: %d Fail: %d Exception: %d" total (or pass 0) (or fail 0) (or exception 0)))

; https://clojuredocs.org/clojure.core/*out*#example-6210625ce4b0b1e3652d75a9
(def noop-writer
  ;; *out* needs to be bound to a java.io.writer, so proxy a writer
  ;; https://docs.oracle.com/javase/7/docs/api/java/io/Writer.html
  (proxy [java.io.Writer] []
    (close [] nil)
    (flush [] nil)
    (write
      ;; ... which politely ignores any calls.
      ([cbuf] nil)
      ([cbuf off len] nil))))

(defn run-tests []
  (binding [*out* noop-writer
            *err* noop-writer]
    (->>
     (doall
      (for [i (range (dec (count examples)))
            :let [ex (nth examples i)
                  test-results
                  (try
                    (evaluate-example ex)
                    (catch Throwable t
                      {:overall-result :exception
                       :throwable t}))]]
        {:tc i
         :example ex
         :overall-result (:overall-result test-results)
         :throwable (:throwable test-results)
         :result test-results})))))

(defn report-throwing-fragment [{:keys [throwable result overall-result] :as all}]
  (when (= :exception overall-result)
    (str
     "Offending fragment: \n"
     (->> result
          :fragment-results
          (filter #(= :exception (:pass? %)))
          first
          :form)
     "\n"
     (with-out-str (clojure.stacktrace/print-stack-trace throwable)))))

(defn test-report [test-results]
  (->> test-results
       #_(remove (comp (partial = :pass) :overall-result))
       (map (fn [{i :tc :keys [example overall-result result] :as all}]
              (str
               (format "-------------------------\nTEST: %d RESULT: %s\n%s\n---------------------------"
                       i overall-result example)
               "\n"
               (report-throwing-fragment all)
               "\n"
               (str/join "\n"
                         (map (fn [{:keys [pass? form result expected]}]
                                (str "Fragment result: " pass?
                                     "\nForm \n" form
                                     "\nExpected:\n" (pprint-str expected)
                                     "\n\nActual:\n" (pprint-str result)))
                              (:fragment-results result))))))
       (str/join "\n")))

(defn do-main []
  (let [test-results (run-tests)]
    (when-not (every? (comp (partial = :pass) :overall-result) test-results)
      (str
       (test-report test-results)
       (->> test-results summarize-results pprint-results)))))

(defn main [_]
  (when-let [report (do-main)]
    (println report)
    (System/exit 1)))




(comment
  (executable-fragments (nth examples 15))
  (evaluate-example (nth examples 15))
  )
