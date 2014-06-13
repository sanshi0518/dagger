(ns dagger.logstash.ngx
  (:gen-class :main true)
  (:import (com.google.common.base Splitter)
           (java.net URL)
           (java.io File))
  (:require [clojure.java.io :as io]
            [selmer.parser :as selmer]))

(defonce resource-dir "logstash/ngx/")

(def ^:dynamic module)
(def ^:dynamic log-type)
(def ^:dynamic ip)
(def ^:dynamic path)

(defrecord definition-record [module type ip path])

(def splitter (-> (Splitter/on " ") (.trimResults) (.omitEmptyStrings)))


(defn meta-line? [line]
  (.startsWith line "{"))

(defn ignored-line? [line]
  (or (.isEmpty (.trim line))
      (.startsWith line "#")
      (meta-line? line)))

(defn check-first-definition-items [^URL definition]
  (with-open [rdr (io/reader definition)]
    (loop [lines (line-seq rdr)]
      (if (ignored-line? (first lines))
        (recur (rest lines))
        (let [line (first lines)
              items (-> (.split splitter line) seq)
              icount (count items)]
          (if (not= 4 icount)
            (throw (Exception. "definition error: first line must contain four items"))))))))

(defn parse-definition [^URL definition]
  (check-first-definition-items definition)
  (with-open [rdr (io/reader definition)]
    (let [result (atom {:template "common.template"})
          definitions (atom '())]
      (binding [module nil log-type nil ip nil path nil]
        (doseq [line (line-seq rdr)]
          (if-not (ignored-line? line)
            (let [items (-> (.split splitter line) seq)
                  icount (count items)]
              (when (= 1 icount)
                (swap! definitions conj (definition-record. module log-type ip (first items))))
              (when (= 2 icount)
                (set! ip (first items))
                (swap! definitions conj (definition-record. module log-type ip (second items))))
              (when (= 3 icount)
                (set! log-type (first items))
                (set! ip (second items))
                (swap! definitions conj (definition-record. module log-type ip (nth items 2))))
              (when (= 4 icount)
                (set! module (first items))
                (set! log-type (second items))
                (set! ip (nth items 2))
                (swap! definitions conj (definition-record. module log-type ip (nth items 3)))))
            (when (meta-line? line)
              (swap! result conj (read-string line))))))
      (swap! result conj {:definitions @definitions})
      @result)))

(defn aggregate-definition [parse-result]
  (let [definitions (parse-result :definitions)]
    (loop [result {} left definitions]
      (if (empty? left)
        (-> (dissoc parse-result :definitions)
            (assoc :definitions (conj result nil)))
        (let [definition (first left)
              key (keyword (str (:ip definition) "-" (:module definition)))
              logs (result key)]
          (if (nil? logs)
            (recur (conj result {key (list definition)}) (rest left))
            (if (.contains logs definition)
              (recur result (rest left))
              (recur (conj result {key (conj logs definition)}) (rest left)))))))))

(def load-definition (comp aggregate-definition parse-definition io/resource))

(defn render-file [filename context]
  (-> filename
      io/resource
      slurp
      (selmer/render context)))

(defn render-template [aggr-result]
  (let [template (aggr-result :template)
        definitions (aggr-result :definitions)
        keys (keys definitions)]
    (doseq [key keys]
      (loop [ip-module-definitions (definitions key) file-content ""]
        (let [definition (first ip-module-definitions)
              type (:type definition)
              file-template (or (-> (str "file-template-" type) keyword aggr-result)
                                "file-common.template")]
          (if (empty? ip-module-definitions)
            (let [output-dir (File. (str "output/" resource-dir))
                  output-file (File. output-dir (str (name key) ".conf"))]
              (when-not (.exists output-dir)
                (.mkdirs output-dir))
              (when-not (.exists output-file)
                (.createNewFile output-file))
              (with-open [wrt (io/writer output-file)]
                (->> (-> (str resource-dir "conf/" template)
                         (render-file (conj aggr-result {:input-files file-content
                                                         :module (second (.split (name key) "-"))})))
                     (.write wrt))))
            (recur (rest ip-module-definitions)
                   (-> (str resource-dir "include/" file-template)
                       (render-file definition)
                       (str "\n" file-content)))))))))

(defn -main [& args]
  (when (< (count args) 1)
    (throw (Exception. "please pass logs definition(s) as arguments")))
  (doseq [definition args]
    (-> (load-definition (str resource-dir "logs/" definition))
        render-template)))