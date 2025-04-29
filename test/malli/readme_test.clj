(ns malli.readme-test
  (:require  [clojure.test :as t :refer [deftest is]]
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
                         pass? (is (= (canonicalize-str expected)
                                      (canonicalize result)))]
                     pass?))))))


(def examples
  (->> (re-seq #"(?ms)```clojure(?! notest).*?```" (slurp "README.md"))
       (map #(str/replace %  #"```(clojure)?" ""))))

(deftest auto-tests
  (doall
   (for [i (range (dec (count examples)))
         :let [ex (nth examples i)
               test-result
               (try
                 (is (try-example ex) "Doesn't evaluate to expected result!")
                 (catch Throwable t
                   (is false
                       (str "Example evaluated to exception!" (.getMessage t) "\n" ex))))]]
     [i ex test-result])))


(comment
  (executable-fragments (nth examples 87))
  (try-example (nth examples 150))
  )
