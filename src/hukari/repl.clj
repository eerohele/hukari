(ns hukari.repl
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.pprint :as pprint]
            [clojure.tools.build.api :as tools.build.api])
  (:import (java.net URI URLEncoder)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)
           (java.text StringCharacterIterator)))

(set! *print-namespace-maps* false)

(def ^:private http-client (HttpClient/newHttpClient))
(def ^:private body-handler (HttpResponse$BodyHandlers/ofInputStream))
(def ^:private request-builder (HttpRequest/newBuilder))

(defn search-maven-central
  ([] (search-maven-central {}))
  ([{:keys [max-results] :or {max-results 10}}]
   (set! *print-namespace-maps* false)

   (print "Search Maven Central (:q to abort): ")
   (flush)

   (let [query (read-line)]
     (when-not (= ":q" query)
       (let [_ (println)
             uri (URI/create (format "https://search.maven.org/solrsearch/select?q=%s&rows=%d&wt=xml" (URLEncoder/encode query) max-results))
             request (.. request-builder (uri uri) (build))
             response (.send http-client request body-handler)
             result-zipper (-> response .body xml/parse zip/xml-zip)
             coordinates (mapv #(let [id (-> % :content (nth 3) (get :content) first (string/replace #":" "/") symbol)
                                      version (-> % :content (nth 4) (get :content) first)]
                                  {id {:mvn/version version}})
                           (-> result-zipper zip/down zip/right zip/children))]
         (if (empty? coordinates)
           (printf "No results for %s.\n" query)
           (let [sb (StringBuilder.)
                 _ (doall
                     (map-indexed (fn [index coord]
                                    (.append sb (format "%02d" (inc index)))
                                    (.append sb ". ")
                                    (.append sb (pr-str coord))
                                    (.append sb \newline))
                       coordinates))
                 _ (println (.toString sb))
                 _ (print "Enter the number of the artifact whose coordinates to return or :q to abort: ")
                 _ (flush)
                 choice (read)]
             (if (= :q choice)
               (println "aborting...")
               (get coordinates (dec choice))))))))))

(comment
  (search-maven-central {:max-results 5})
  ,,,)

(defn search-clojars
  []
  (print "Search Clojars (:q to abort): ")
  (flush)

  (let [query (read-line)]
    (when-not (= ":q" query)
      (let [uri (URI/create (format "https://clojars.org/search?q=%s&format=xml" (URLEncoder/encode query)))
            request (.. request-builder (uri uri) (build))
            response (.send http-client request body-handler)
            result-zipper (-> response .body xml/parse zip/xml-zip)
            {:keys [group_name jar_name version]} (-> result-zipper zip/down zip/node :attrs)]
        (println)
        (doto {(symbol group_name jar_name) {:mvn/version version}} prn)))))

(comment
  (search-clojars)
  ,,,)

;; Ported from https://stackoverflow.com/a/3758880
(defn human-readable-byte-count
  [bytes]
  (if (< -1000 bytes 1000)
    (str bytes " B")
    (let [ci (StringCharacterIterator. "kMGTPE")
          bytes (loop [bytes bytes]
                  (if (or (<= bytes -999950) (>= bytes 999950))
                    (do (.next ci) (recur (/ bytes 1000)))
                    bytes))]
      (format "%.1f %cB" (/ bytes 1000.0) (.current ci)))))

(comment
  (human-readable-byte-count -999)
  (human-readable-byte-count -1e4)
  (human-readable-byte-count 999)
  (human-readable-byte-count 0)
  (human-readable-byte-count 1e4)
  (human-readable-byte-count 1e5)
  (human-readable-byte-count 1e6)
  ,,,)

(defn path-size
  "Given a string path to a file, return the size of the file in bytes."
  [path]
  (-> path io/file .length))

(defn get-basis
  "Given a deps.edn coordinate representing a Maven dependency, return the
  tools.deps basis for the dependency.

  Removes org.clojure/clojure and its deps from the libs map."
  [coord]
  (->
    (tools.build.api/create-basis {:project {:deps coord}})
    (update :libs dissoc
      'org.clojure/clojure
      'org.clojure/spec.alpha
      'org.clojure/core.specs.alpha)))

(defn total-basis-weight
  "Given a tools.deps basis, return the total size of all the libs comprising
  the basis in bytes."
  [{:keys [libs]}]
  (transduce
    (comp
      (map val)
      (mapcat :paths)
      (map path-size))
    +
    0
    libs))

(defn dep-weight-summary
  "Given a deps.edn coordinate representing a Maven dependency, return a
  human-readable string of the total weight of the dependency that
  coordinate points to."
  [coord]
  (let [{:keys [libs] :as basis} (get-basis coord)]
    {:libs (count libs)
     :total-weight (-> basis total-basis-weight human-readable-byte-count)}))

(comment
  (dep-weight-summary '{cnuernber/dtype-next {:mvn/version "9.022"}})
  (dep-weight-summary '{org.clojure/data.csv {:mvn/version "1.0.0"}})
  ,,,)

(defn individual-dep-weights
  "Given a tools.deps basis, return a seq with data about the size of each lib
  in the basis."
  [{:keys [libs]}]
  (sort-by :size >
    (map (fn [[id {:keys [mvn/version paths]}]]
           {:artifact id
            :version version
            :size (transduce (map path-size) + 0 paths)})
      libs)))

(defn dep-weights
  "Given a deps.edn coordinate representing a Maven dependency, return a seq
  with data about the size of each lib in the basis."
  [coord]
  (-> coord get-basis individual-dep-weights))

(comment
  (dep-weights '{cnuernber/dtype-next {:mvn/version "9.022"}})
  (dep-weights '{org.clojure/data.csv {:mvn/version "1.0.0"}})
  ,,,)

(defn print-dep-weights
  "Given a deps.edn coordinate representing a Maven dependency, print
  information about the size of each of its dependencies."
  [coord]
  (pprint/print-table
    (map #(update % :size human-readable-byte-count) (dep-weights coord))))

(comment
  (print-dep-weights '{org.clojure/data.csv {:mvn/version "1.0.0"}})
  (print-dep-weights '{cnuernber/dtype-next {:mvn/version "9.022"}})
  ,,,)

(defn dump-threads
  []
  (sequence
    (comp
      (map bean)
      (map (fn [{:keys [threadId daemon threadName suspended threadState]}]
             {:id threadId
              :name threadName
              :suspended suspended
              :daemon daemon
              :state (condp = threadState
                       Thread$State/NEW :new
                       Thread$State/RUNNABLE :runnable
                       Thread$State/BLOCKED :blocked
                       Thread$State/WAITING :waiting
                       Thread$State/TIMED_WAITING :timed-waiting
                       Thread$State/TERMINATED :terminated)})))
    (.dumpAllThreads
      (java.lang.management.ManagementFactory/getThreadMXBean)
      false false)))
