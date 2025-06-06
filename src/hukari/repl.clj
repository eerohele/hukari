(ns hukari.repl
  (:require [clojure.java.io :as io]
            [clojure.core.protocols :as protocols]
            [clojure.core.server :as server]
            [clojure.datafy :refer [datafy]]
            [clojure.pprint :as pprint]
            [clojure+.error :as +error]
            [clojure+.print :as +print]
            [hukari.flyway]
            [hukari.jfr])
  (:import (java.io PrintWriter StringWriter)
           (java.lang.management ManagementFactory)
           (java.net ServerSocket)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.text StringCharacterIterator)
           (java.time Duration)
           (java.util Base64 Base64$Decoder Base64$Encoder Date)
           (java.util.spi ToolProvider)
           (org.openjdk.jol.info GraphLayout)
           (com.github.vertical_blank.sqlformatter SqlFormatter SqlFormatter$Formatter)
           (com.github.vertical_blank.sqlformatter.languages Dialect)))

(+print/install!)
(+error/install!)

(defn start-server
  [{:keys [port dir] :or {port 0 dir "."}}]
  (let [^ServerSocket server (server/start-server {:name "server"
                                                   :port port
                                                   :accept `server/repl
                                                   :server-daemon false})
        port (.getLocalPort server)
        host (-> server .getInetAddress .getCanonicalHostName)
        port-file (io/file dir ".repl-port")]
    (.deleteOnExit port-file)
    (spit port-file port)
    (printf "Socket server listening on %s:%s (Java %s, PID %d)\n" host port (Runtime/version) (.pid (java.lang.ProcessHandle/current)))))

(defn init
  "Tutkain REPL init fn.

  See https://tutkain.flowthing.me/#customizing-your-repl."
  []
  (set! *print-namespace-maps* false)
  (set! *print-length* 16)
  (set! *print-level* 8)
  (when-some [f (requiring-resolve 'clojure.spec.alpha/check-asserts)]
    (f true))
  (when-some [f (requiring-resolve 'clojure.spec.test.alpha/instrument)]
    (f))
  #_(try
      (when-some [f (requiring-resolve 'malli.dev/start!)] (f))
      (catch java.io.FileNotFoundException _)))

(defmacro portal
  []
  `(do
     ((requiring-resolve 'portal.api/open) {:app false})
     (add-tap (requiring-resolve 'portal.api/submit))))

(defmacro flow-storm
  []
  `((requiring-resolve 'flow-storm.api/local-connect)))

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
     (clj-async-profiler.core/profile {:features [:vtable :comptask]} ~@body)))

(defn flamegraph
  []
  ((requiring-resolve 'clj-async-profiler.core/serve-ui) 10000)
  ((requiring-resolve 'clojure.java.browse/browse-url) "http://localhost:10000"))

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
    ((requiring-resolve 'clojure.tools.build.api/create-basis) {:project {:deps coord}})
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
  (doto
    (map #(update % :size human-readable-byte-count) (dep-weights coord))
    pprint/print-table
    tap>))

(comment
  (print-dep-weights '{org.clojure/data.csv {:mvn/version "1.0.0"}})
  (print-dep-weights '{cnuernber/dtype-next {:mvn/version "9.022"}})
  ,,,)

(def ^:private ^Base64$Encoder base64-encoder (Base64/getEncoder))

(defn base64-encode
  [^String s]
  (.encodeToString base64-encoder (.getBytes s)))

(def ^:private ^Base64$Decoder base64-decoder (Base64/getDecoder))

(defn base64-decode
  [^String s]
  (String. (.decode base64-decoder s) "UTF-8"))

(comment
  (-> "(def x 1)" base64-encode base64-decode)
  ,,,)

(defn memory-layout
  [obj]
  (let [layout (GraphLayout/parseInstance (into-array [obj]))]
    (map (fn [^Class class]
           (let [count (.count (.getClassCounts layout) class)
                 size (.count (.getClassSizes layout) class)]
             {:description (.getName class)
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
  (with-open [outs (StringWriter.)
              ^PrintWriter outp (PrintWriter. outs)
              errs (StringWriter.)
              ^PrintWriter errp (PrintWriter. errs)]
    (let [temp-dir (.toFile (Files/createTempDirectory "tutkain-classes-" (into-array FileAttribute [])))]
      (try
        (binding [*compile-path* (.getCanonicalPath temp-dir)]
          (some-> qualified-symbol namespace symbol compile)
          (.run ^ToolProvider @javap outp errp ^"[Ljava.lang.String;" (into-array String ["-c" "-l" "-verbose" "-constants" "-private" (class-path temp-dir qualified-symbol)]))
          (println (.toString outs)))
        (finally
          (.delete temp-dir))))))

(comment
  (bytecode `class-path)
  ,,,)

(extend-protocol protocols/Datafiable
  clojure.core.Eduction
  (datafy [this]
    (seq this))

  java.net.ServerSocket
  (datafy [^java.net.ServerSocket this]
    {:channel (.getChannel this)
     :inet-address (.getInetAddress this)
     :local-port (.getLocalPort this)
     :local-socket-address (.getLocalSocketAddress this)
     :receive-buffer-size (.getReceiveBufferSize this)
     :reuse-address (.getReuseAddress this)
     :so-timeout (.getSoTimeout this)
     :bound? (.isBound this)
     :closed? (.isClosed this)
     :options (into {}
                (for [^java.net.SocketOption option (.supportedOptions this)]
                  [(.name option) (datafy (.getOption this option))]))})

  java.lang.Thread
  (datafy [^java.lang.Thread this]
    {:id (.getId this)
     :name (.getName this)
     :priority (.getPriority this)
     :thread-group (.getThreadGroup this)
     :interrupted? (.isInterrupted this)
     :alive? (.isAlive this)
     :daemon? (.isDaemon this)})

  java.util.concurrent.ThreadPoolExecutor
  (datafy [^java.util.concurrent.ThreadPoolExecutor this]
    {:active-count (.getActiveCount this)
     :completed-task-count (.getCompletedTaskCount this)
     :core-pool-size (.getCorePoolSize this)
     :largest-pool-size (.getLargestPoolSize this)
     :max-pool-size (.getMaximumPoolSize this)
     :task-count (.getTaskCount this)
     :terminated? (.isTerminated this)
     :terminating? (.isTerminating this)
     :shutdown? (.isShutdown this)})

  java.util.concurrent.BlockingQueue
  (datafy [^java.util.concurrent.BlockingQueue this]
    {:size (.size this)
     :empty? (.isEmpty this)
     :remaining-capacity (.remainingCapacity this)
     :contents (vec (.toArray this))
     :head (.peek this)})

  java.lang.management.ThreadMXBean
  (datafy [this]
    {:threads (map datafy (.dumpAllThreads this false false))
     :deadlocked-threads (vec (.findDeadlockedThreads this))
     :thread-ids (vec (.getAllThreadIds this))
     :thread-cpu-time (.getCurrentThreadCpuTime this)
     :thread-user-time (.getCurrentThreadUserTime this)
     :daemon-thread-count (.getDaemonThreadCount this)
     :peak-thread-count (.getPeakThreadCount this)
     :thread-count (.getThreadCount this)
     :started-thread-count (.getTotalStartedThreadCount this)})

  java.lang.management.MemoryMXBean
  (datafy [this]
    {:heap-memory (datafy (.getHeapMemoryUsage this))
     :non-heap-memory (datafy (.getNonHeapMemoryUsage this))})

  java.lang.management.MemoryPoolMXBean
  (datafy [this]
    (let [collection-usage-threshold-supported? (.isCollectionUsageThresholdSupported this)
          usage-threshold-supported? (.isUsageThresholdSupported this)]
      (cond->
        (array-map
          :name (.getName this)
          :type (condp = (.getType this)
                  java.lang.management.MemoryType/HEAP :heap
                  java.lang.management.MemoryType/NON_HEAP :non-heap)
          :collection-usage (datafy (.getCollectionUsage this))
          :memory-manager-names (vec (.getMemoryManagerNames this))
          :peak-usage (datafy (.getPeakUsage this))
          :usage (datafy (.getUsage this))
          :collection-usage-threshold-supported? collection-usage-threshold-supported?
          :usage-threshold-supported? usage-threshold-supported?
          :valid? (.isValid this))

        collection-usage-threshold-supported?
        (assoc
          :collection-usage-threshold (.getCollectionUsageThreshold this)
          :collection-usage-threshold-exceeded? (.isCollectionUsageThresholdExceeded this)
          :collection-usage-threshold-count (.getCollectionUsageThresholdCount this))

        usage-threshold-supported?
        (assoc
          :usage-threshold (.getUsageThreshold this)
          :usage-threshold-exceeded? (.isUsageThresholdExceeded this)))))

  java.lang.management.GarbageCollectorMXBean
  (datafy [this]
    {:name (.getName this)
     :memory-pool-names (vec (.getMemoryPoolNames this))
     :collection-count (.getCollectionCount this)
     :collection-time (.getCollectionTime this)})

  java.lang.management.RuntimeMXBean
  (datafy [this]
    {:start-time (Date. (.getStartTime this))
     :uptime (Duration/ofMillis (.getUptime this))
     :name (.getVmName this)
     :vendor (.getVmVendor this)
     :version (.getVmVersion this)})

  java.lang.management.CompilationMXBean
  (datafy [this]
    {:name (.getName this)
     :total-compilation-time (Duration/ofMillis (.getTotalCompilationTime this))
     :compilation-time-monitoring-supported? (.isCompilationTimeMonitoringSupported this)})

  java.lang.management.ClassLoadingMXBean
  (datafy [this]
    {:loaded-class-count (.getLoadedClassCount this)
     :total-loaded-class-count (.getTotalLoadedClassCount this)
     :unloaded-class-count (.getUnloadedClassCount this)
     :verbose? (.isVerbose this)})

  java.lang.management.OperatingSystemMXBean
  (datafy [this]
    (array-map
      :name (.getName this)
      :version (.getVersion this)
      :architecture (.getArch this)
      :available-processors (.getAvailableProcessors this)
      :system-load-average (.getSystemLoadAverage this)
      :committed-virtual-size (.getCommittedVirtualMemorySize ^com.sun.management.OperatingSystemMXBean this)
      :cpu-load (.getCpuLoad ^com.sun.management.OperatingSystemMXBean this)
      :free-memory-size (.getFreeMemorySize ^com.sun.management.OperatingSystemMXBean this)
      :free-physical-memory-size (.getFreePhysicalMemorySize ^com.sun.management.OperatingSystemMXBean this)
      :free-swap-space-size (.getFreeSwapSpaceSize ^com.sun.management.OperatingSystemMXBean this)
      :process-cpu-load (.getProcessCpuLoad ^com.sun.management.OperatingSystemMXBean this)
      :process-cpu-time (Duration/ofNanos (.getProcessCpuTime ^com.sun.management.OperatingSystemMXBean this))
      :total-memory-size (.getTotalMemorySize ^com.sun.management.OperatingSystemMXBean this)
      :total-physical-memory-size (.getTotalPhysicalMemorySize ^com.sun.management.OperatingSystemMXBean this)
      :total-swap-space-size (.getTotalSwapSpaceSize ^com.sun.management.OperatingSystemMXBean this)))

  java.lang.management.ThreadInfo
  (datafy [this]
    (array-map
      :thread-id (.getThreadId this)
      :thread-name (.getThreadName this)
      :thread-state (condp = (.getThreadState this)
                      Thread$State/NEW :new
                      Thread$State/RUNNABLE :runnable
                      Thread$State/BLOCKED :blocked
                      Thread$State/WAITING :waiting
                      Thread$State/TIMED_WAITING :timed-waiting
                      Thread$State/TERMINATED :terminated)
      :daemon? (.isDaemon this)
      :priority (.getPriority this)
      :blocked-count (.getBlockedCount this)
      :blocked-time (.getBlockedTime this)
      :waited-count (.getWaitedCount this)
      :waited-time (.getWaitedTime this)
      :in-native? (.isInNative this)
      :suspended? (.isSuspended this)))

  java.lang.management.MemoryUsage
  (datafy [this]
    {:committed (.getCommitted this)
     :init (.getInit this)
     :max (.getMax this)
     :used (.getUsed this)})

  java.net.URI
  (datafy [this]
    (sorted-map
      :absolute? (.isAbsolute this)
      :ascii-string (.toASCIIString this)
      :authority (.getAuthority this)
      :fragment (.getFragment this)
      :host (.getHost this)
      :opaque? (.isOpaque this)
      :path (.getPath this)
      :port (.getPort this)
      :query (.getQuery this)
      :raw-authority (.getRawAuthority this)
      :raw-fragment (.getRawFragment this)
      :raw-path (.getRawPath this)
      :raw-query (.getRawQuery this)
      :raw-scheme-specific-part (.getRawSchemeSpecificPart this)
      :raw-user-info (.getRawUserInfo this)
      :scheme (.getScheme this)
      :scheme-specific-part (.getSchemeSpecificPart this)
      :string (.toString this)
      :user-info (.getUserInfo this)))

  java.util.Locale
  (datafy [this]
    {:country (.getCountry this)
     :display-country (.getDisplayCountry this)
     :display-language (.getDisplayLanguage this)
     :display-name (.getDisplayName this)
     :display-script (.getDisplayScript this)
     :display-variant (.getDisplayVariant this)
     :iso-3-country (.getISO3Country this)
     :iso-3-language (.getISO3Language this)
     :language (.getLanguage this)
     :script (.getScript this)
     :unicode-locale-attributes (.getUnicodeLocaleAttributes this)
     :variant (.getVariant this)
     :has-extensions? (.hasExtensions this)
     :language-tag (.toLanguageTag this)})

  java.time.Duration
  (datafy [this]
    {:negative? (.isNegative this)
     :positive? (.isPositive this)
     :zero? (.isZero this)
     :days (.toDays this)
     :hours (.toHours this)
     :millis (.toMillis this)
     :minutes (.toMinutes this)
     :nanos (.toNanos this)
     :seconds (.toSeconds this)
     :iso-8601-string (.toString this)})

  java.time.temporal.TemporalUnit
  (datafy [this]
    {:duration (datafy (.getDuration this))
     :date-based? (.isDateBased this)
     :duration-estimated? (.isDurationEstimated this)
     :time-based? (.isTimeBased this)
     :name (.toString this)})

  java.util.Properties
  (datafy [this]
    (into {} this))

  javax.sql.DataSource
  (datafy [this]
    {:connection (datafy (.getConnection this))
     :login-timeout (datafy (Duration/ofSeconds (.getLoginTimeout this)))})

  java.sql.Connection
  (datafy [this]
    {:auto-commit (.getAutoCommit this)
     :catalog (.getCatalog this)
     :client-info (datafy (.getClientInfo this))
     :holdability (case (.getHoldability this)
                    1 :hold-cursors-over-commit
                    2 :close-cursors-at-commit)
     :network-timeout (datafy (Duration/ofMillis (.getNetworkTimeout this)))
     :schema (.getSchema this)
     :transaction-isolation (case (.getTransactionIsolation this)
                              1 :read-uncommitted
                              2 :read-committed
                              3 :repeatable-read
                              4 :serializable
                              5 :none)
     :closed? (.isClosed this)
     :read-only? (.isReadOnly this)}))

(defn humanize-memory-counts
  [mem]
  (->
    mem
    (update :committed human-readable-byte-count)
    (update :init human-readable-byte-count)
    (update :max human-readable-byte-count)
    (update :used human-readable-byte-count)))

(defn runtime-stats
  []
  (array-map
    :os
    (->
      (ManagementFactory/getOperatingSystemMXBean)
      (datafy)
      (update :cpu-load #(str (.setScale (bigdec (* 100 %)) 0 BigDecimal/ROUND_HALF_UP) "%"))
      (update :committed-virtual-size human-readable-byte-count)
      (update :free-memory-size human-readable-byte-count)
      (update :free-physical-memory-size human-readable-byte-count)
      (update :free-swap-space-size human-readable-byte-count)
      (update :total-memory-size human-readable-byte-count)
      (update :total-physical-memory-size human-readable-byte-count)
      (update :total-swap-space-size human-readable-byte-count))

    :runtime (datafy (ManagementFactory/getRuntimeMXBean))

    :memory
    (->
      (ManagementFactory/getMemoryMXBean)
      (datafy)
      (update :heap-memory humanize-memory-counts)
      (update :non-heap-memory humanize-memory-counts))

    :threads (datafy (ManagementFactory/getThreadMXBean))
    :gc (map datafy (ManagementFactory/getGarbageCollectorMXBeans))

    :class-loader
    (->
      (ManagementFactory/getClassLoadingMXBean)
      (datafy)
      (update :loaded-class-count #(pprint/cl-format nil "~,,' :D" %))
      (update :total-loaded-class-count #(pprint/cl-format nil "~,,' :D" %)))

    :compilation
    (datafy (ManagementFactory/getCompilationMXBean))

    :memory-pools
    (map datafy (ManagementFactory/getMemoryPoolMXBeans))))

(comment
  (datafy (java.net.ServerSocket.))
  (tap> (runtime-stats))

  (doto (java.util.concurrent.LinkedBlockingQueue. 8)
    (.put 1)
    (.put 2)
    (.put 3)
    (.take))
  ,,,)

;; Adapted from https://clojure-goes-fast.com/kb/benchmarking/time-plus/
(let [time*
      (fn [^long duration-in-ms f]
        (let [^com.sun.management.ThreadMXBean bean (ManagementFactory/getThreadMXBean)
              bytes-before (.getCurrentThreadAllocatedBytes bean)
              duration (* duration-in-ms 1000000)
              start (System/nanoTime)
              ret (f)
              delta (- (System/nanoTime) start)
              deadline (+ start duration)
              tight-iters (max (quot (quot duration delta) 10) 1)]
          (loop [iterations 1]
            (let [now (System/nanoTime)]
              (if (< now deadline)
                (do (dotimes [_ tight-iters] (f))
                  (recur (+ iterations tight-iters)))
                (let [i' (double iterations)
                      bytes-after (.getCurrentThreadAllocatedBytes bean)
                      t (/ (- now start) i')
                      time (cond (< t 1e3) (format "%.0f ns" t)
                             (< t 1e6) (format "%.2f µs" (/ t 1e3))
                             (< t 1e9) (format "%.2f ms" (/ t 1e6))
                             :else (format "%.2f s" (/ t 1e9)))
                      allocations (/ (- bytes-after bytes-before) i')]
                  (println {:time time
                            :allocations allocations
                            :iterations iterations})
                  ret))))))]
  (defmacro time+
    "Like `time`, but runs the supplied body for 2000 ms and prints the average
  time for a single iteration. Custom total time in milliseconds can be provided
  as the first argument. Returns the returned value of the FIRST iteration."
    [?duration-in-ms & body]
    (let [[duration body] (if (integer? ?duration-in-ms)
                            [?duration-in-ms body]
                            [2000 (cons ?duration-in-ms body)])]
      `(~time* ~duration (fn [] ~@body)))))

(defmacro allocated-bytes
  "Execute the body, then print a human-readable presentation of magnitude of
  bytes that were allocated during the execution of the body."
  [& body]
  `(let [^com.sun.management.ThreadMXBean bean# (ManagementFactory/getThreadMXBean)
         bytes-before# (.getCurrentThreadAllocatedBytes bean#)]
     ~@body
     (let [bytes-after# (.getCurrentThreadAllocatedBytes bean#)]
       (println
         (human-readable-byte-count (- bytes-after# bytes-before#))
         "allocated"))))

(defn set-reflection-warnings!
  "Given a regexp pattern and a boolean, for each namespace whose name matches
  the pattern, toggle reflection warnings on or off as indicated by the
  boolean and reload the namespace.

  Optionally, pass a symbol naming an entry point namespace as the first arg to
  load it prior to namespace discovery."
  ([re warn?]
   (set-reflection-warnings! nil re warn?))
  ([entry-point-ns re warn?]
   (some-> entry-point-ns require)
   (let [matching-nses (eduction (filter #(re-matches re (str (ns-name %)))) (all-ns))]
     (run! #(binding [*ns* %]
              (set! *warn-on-reflection* warn?)
              (require (ns-name *ns*) :reload))
       matching-nses))))

(comment
  (set-reflection-warnings! 'hukari.repl #"^hukari\..*" true)
  ,,,)

(def ^:private ^SqlFormatter$Formatter sql-formatter
  (SqlFormatter/of Dialect/PostgreSql))

(defn format-sql
  [^String s]
  (println (.format sql-formatter s)))

(defn file->bytes
  "Given a string path to a file, return the file content as bytes."
  [s]
  (-> s io/file .toPath Files/readAllBytes))

(defn gen
  [x]
  (cond
    ((requiring-resolve 'clojure.test.check.generators/generator?) x)
    ((requiring-resolve 'clojure.test.check.generators/generate) x)

    (keyword? x)
    ((requiring-resolve 'clojure.test.check.generators/generate)
     ((requiring-resolve 'clojure.spec.alpha/gen) x))

    :else
    ((requiring-resolve 'malli.generator/generate) x)))

(defonce ^:private -tab nil)

(defn tab
  []
  (if -tab
    ((requiring-resolve 'clojure.java.browse/browse-url)
     ((requiring-resolve 'tab.api/address) -tab))
    (alter-var-root #'-tab (fn [_] ((requiring-resolve 'tab.api/run))))))

(defn codepoint [ch]
  (format "0x%X" (int ch)))

(defn hexify [bytes]
  (let [hex [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f]
        sb (StringBuilder.)]
    (doseq [^byte b bytes]
      (let [v (bit-and b 0xFF)]
        (.append sb (hex (bit-shift-right v 4)))
        (.append sb (hex (bit-and v 0x0F)))))
    (.toString sb)))

(defn inspect
  "Given a (presumably nested) coll, start a loop for navigating the coll.

  Reads from *in*, then prints coll at accumulated keypath.

  Input ? for help.

  For all other inputs, print value at (get-in coll (conj keypath key))."
  [coll & {:keys [print print-length]
           :or {print prn print-length 16}}]
  (binding [*print-length* print-length]
    (print coll)
    (loop [keypath []]
      (printf "%s=> " keypath)
      (flush)
      (let [input (read)]
        (cond
          (= input 'q)
          :bye

          (= input '?)
          (do
            (println "Commands:

    ? — show this help
    q — quit
    . — show current value (without truncation)
    - — go up one level
          ")
            (recur keypath))

          (list? input)
          (do
            (print (eval input))
            (recur keypath))

          :else (do
                  (println input)
                  (let [v (get-in coll keypath)]
                    (cond
                      (and (= input '-) (empty? keypath))
                      (do
                        (print :top)
                        (print v)
                        (recur keypath))

                      (= input '-)
                      (let [keypath (pop keypath)]
                        (print (get-in coll keypath))
                        (recur keypath))

                      (= input '.)
                      (do
                        (binding [*print-length* nil]
                          (print v))
                        (recur keypath))

                      (or (map? v) (indexed? v))
                      (let [new-keypath (conj keypath input)]
                        (if (contains? (get-in coll keypath) input)
                          (do (print (get-in coll new-keypath))
                            (recur new-keypath))
                          (do (print :nope)
                            (print (get-in coll keypath))
                            (recur keypath))))

                      (and (seqable? v) (nat-int? input) (< input (count v)))
                      (do
                        (print (nth v input))
                        (recur keypath))

                      :else
                      (do
                        (print :nope)
                        (print v)
                        (recur keypath))))))))))
