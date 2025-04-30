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
  (str/replace s #"(?m)^\w*;+" ""))

(def assertion-regex #";+\w*=>\w*")

(defn executable-fragments [example]
  (if-not (re-find assertion-regex example)
    [{:form example}]
    (as-> example s
      (str/split s assertion-regex)
      (partition 2 1 s)
      (map-indexed (fn [i [form expected]]
                     {:form (str "(in-ns 'readme-test) (mr/set-default-registry! default-registry)\n"
                                 (if (zero? i) form
                                     (second (str/split form #"\n\n" 2))))
                      :expected-result
                      (-> (str/split expected #"\n\n") first str/trim remove-leading-comment-chars)}) s))))

(defn canonicalize [clj-data]
  (->> clj-data
       pr-str
       (str "'")
       load-string))

(defn canonicalize-str [clj-str]
  (->> clj-str
       (str "'")
       load-string))

(defn evaluate-example [example]
  (let [fragment-results
        (->> example
             executable-fragments
             (map (fn [{form :form expected :expected-result}]
                    (if-not expected
                      {:pass? true
                       :result (->> form load-string canonicalize)}
                      (let [result (->> form load-string canonicalize)
                            canonicalized-expected (canonicalize-str expected)
                            pass? (= result
                                     canonicalized-expected)]
                        {:pass? pass?
                         :result result
                         :expected canonicalized-expected})))))]
    {:overall-result (if (every? :pass? fragment-results) :pass :fail)
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
  (format "Total: %d Pass: %d Fail: %d Exception: %d" total pass fail exception))

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
                       :exception t}))]]
        {:tc i
         :example ex
         :overall-result (:overall-result test-results)
         :exception (:exception test-results)
         :result test-results})))))

(defn test-report [test-results]
  (->> test-results
       (remove (comp (partial = :pass) :overall-result))
       (map (fn [{i :tc :keys [example overall-result result exception]}]
              (str
               (format "-------------------------\nTEST: %d RESULT: %s\n%s\n---------------------------"
                       i overall-result example)
               "\n"
               (when (= :exception overall-result)
                 (with-out-str (clojure.stacktrace/print-stack-trace exception)))
               "\n"
               (str/join "\n"
                         (map (fn [{:keys [pass? result expected]}]
                                (str "Fragment result: " pass?
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
