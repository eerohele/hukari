(ns hukari.repl
  (:require [clojure.java.io :as io]
            [clojure.core.server :as server]
            [clojure.string :as string]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.pprint :as pprint])
  (:import (java.io PrintWriter StringWriter)
           (java.net URI URLEncoder)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.text StringCharacterIterator)
           (java.util Base64)
           (java.util.spi ToolProvider)
           (org.openjdk.jol.info GraphLayout)))

(def ^:private http-client (HttpClient/newHttpClient))

(def ^:private body-handler (HttpResponse$BodyHandlers/ofInputStream))
(def ^:private request-builder (HttpRequest/newBuilder))

(defn start-server
  [_]
  (let [server (server/start-server {:name "server" :port 0 :accept `server/repl :server-daemon false})
        port (.getLocalPort server)
        host (-> server .getInetAddress .getCanonicalHostName)
        port-file (io/file ".repl-port")]
    (.deleteOnExit port-file)
    (spit port-file port)
    (printf "Socket server listening on %s:%s\n" host port)))

(defn intern-utils
  []
  ;; Snitch
  (intern 'clojure.core (with-meta 'defn* (meta (requiring-resolve 'snitch.core/defn*))) (requiring-resolve 'snitch.core/defn*))
  (intern 'clojure.core (with-meta '*fn (meta (requiring-resolve 'snitch.core/*fn))) (requiring-resolve 'snitch.core/*fn))
  (intern 'clojure.core (with-meta 'defmethod* (meta (requiring-resolve 'snitch.core/defmethod*))) (requiring-resolve 'snitch.core/defmethod*))
  (intern 'clojure.core (with-meta '*let (meta (requiring-resolve 'snitch.core/*let))) (requiring-resolve 'snitch.core/*let))

  (require 'hato.client)
  (create-ns 'http)
  (doseq [[_ v] (ns-publics 'hato.client)]
    (intern 'http (symbol (name (symbol v))) @v)))

(defn init
  "Tutkain REPL init fn.

  See https://tutkain.flowthing.me/#customizing-your-repl."
  []
  (set! *print-namespace-maps* false)
  (set! *print-length* 16)
  (set! *print-level* 8)
  (intern-utils))

(defmacro portal
  []
  `(do
     ((requiring-resolve 'portal.api/open) {:app false})
     (add-tap (requiring-resolve 'portal.api/submit))))

(defmacro flow-storm
  []
  `(do
     (require 'flow-storm.api)
     (flow-storm.api/local-connect)))

(defmacro quick-bench
  [& body]
  `(do
     (require 'criterium.core)
     (criterium.core/quick-bench ~@body)))

(defmacro decompile-form
  [& body]
  `(do
     (require 'clj-java-decompiler.core)
     (clj-java-decompiler.core/decompile ~@body)))

(defmacro disassemble-form
  [& body]
  `(do
     (require 'clj-java-decompiler.core)
     (clj-java-decompiler.core/disassemble ~@body)))

(defmacro profile
  [& body]
  `(do
     (require 'clj-async-profiler.core)
     (clj-async-profiler.core/profile ~@body)))

(defn flamegraph
  []
  ((requiring-resolve 'clj-async-profiler.core/serve-files) 10000)
  ((requiring-resolve 'clojure.java.browse/browse-url) "http://localhost:10000"))

(defn search-maven-central
  ([] (search-maven-central {}))
  ([{:keys [max-results] :or {max-results 10}}]
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
        {(symbol group_name jar_name) {:mvn/version version}}))))

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
    ((requiring-resolve 'tools.build.api/create-basis) {:project {:deps coord}})
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

(def ^:private base64-encoder (Base64/getEncoder))

(defn base64-encode
  [s]
  (.encodeToString base64-encoder (.getBytes s)))

(def ^:private base64-decoder (Base64/getDecoder))

(defn base64-decode
  [s]
  (String. (.decode base64-decoder s) "UTF-8"))

(comment
  (-> "(def x 1)" base64-encode base64-decode)
  ,,,)

(defn memory-layout
  [obj]
  (let [layout (GraphLayout/parseInstance (into-array [obj]))]
    (map (fn [class]
           (let [count (.count (.getClassCounts layout) class)
                 size (.count (.getClassSizes layout) class)]
             {:description (.getName  class)
              :count count
              :avg (/ size count)
              :sum size}))
      (.getClasses layout))))

(comment
  (println (.toFootprint (GraphLayout/parseInstance (into-array [{:a 1}]))))
  (memory-layout {:a 1})
  ,,,)

(defn memory-use
  [obj]
  (human-readable-byte-count (transduce (map :sum) + 0 (memory-layout obj))))

(comment
  (memory-use {:a 1})
  ,,,)

(defonce javap
  (->
    (ToolProvider/findFirst "javap")
    (.orElseThrow)
    (delay)))

(defn class-path
  [dir qualified-symbol]
  (.getCanonicalPath
    (io/file dir
      (str
        (.replace (namespace qualified-symbol) \. \/)
        \$
        (munge (name qualified-symbol))
        ".class"))))

(defn bytecode
  [qualified-symbol]
  (let [outs (StringWriter.)
        outp (PrintWriter. outs)
        errs (StringWriter.)
        errp (PrintWriter. errs)
        temp-dir (.toFile (Files/createTempDirectory "tutkain-classes-" (into-array FileAttribute [])))]
    (try
      (binding [*compile-path* (.getCanonicalPath temp-dir)]
        (-> qualified-symbol namespace symbol compile)
        (.run ^ToolProvider @javap outp errp (into-array String ["-c" "-l" "-verbose" "-constants" "-private" (class-path temp-dir qualified-symbol)]))
        (.toString outs))
      (finally
        (.delete temp-dir)))))

(defn add-lib-latest
  [search]
  (let [dep (-> ((requiring-resolve 'clojure.tools.deps.alpha.repl/find-lib*) search {:max-count 1}) first :dep)]
    ((requiring-resolve 'clojure.tools.deps.alpha.repl/add-libs) dep)
    dep))

(comment
  (add-lib-latest "hiccup")
  ,,,)