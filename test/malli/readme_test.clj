(ns malli.readme-test
  (:require  [clojure.test :as t]
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

(def assertion-regex #";+ =>\w*")

(defn executable-fragments [example]
  (if-not (re-find assertion-regex example)
    [{:form example}]
    (as-> example s
      (str/split s assertion-regex)
      (partition 2 1 s)
      (map-indexed (fn [i [form expected]]
                     {:form (str "(mr/set-default-registry! default-registry)\n"
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

(defn try-example [example]
  (->> example
       executable-fragments
       (every? (fn [{form :form expected :expected-result}]
                 (if-not expected
                   (do (load-string form)
                       true)
                   (let [result (load-string form)
                         pass? (= (canonicalize result)
                                  (canonicalize-str expected))]
                     pass?))))))


(def examples
  (->> (re-seq #"(?ms)```clojure(?! notest).*?```" (slurp "README.md"))
       (map #(str/replace %  #"```(clojure)?" ""))))

(->>
 (for [i (range (dec (count examples)))
       :let [ex (nth examples i)
             test-result
             (try
               (try-example ex)
               (catch Throwable t
                 (str "exception!" (.getMessage t))))]]
   [i ex test-result])
 (map (fn [[i ex test-result]]
        (format "-------------------------\nTEST: %d RESULT: %s\n%s\n---------------------------"
                i test-result ex)))
 (str/join "\n")
 (spit "results.txt"))


(comment
  (executable-fragments (nth examples 87))
  (try-example (nth examples 150))
  )
