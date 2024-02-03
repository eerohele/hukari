(ns hukari.jfr
  (:require [clojure.datafy :as datafy]
            [clojure.string :as string]
            [clojure.core.protocols :refer [Datafiable]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration)
           (java.util Date)
           (java.util.function Consumer)
           (jdk.jfr
            AnnotationElement
            Configuration
            EventType
            SettingDescriptor
            ValueDescriptor
            Recording
            Recording)
           (jdk.jfr.consumer
            RecordedClass
            RecordedClassLoader
            RecordedEvent
            RecordedFrame
            RecordedMethod
            RecordedObject
            RecordedStackTrace
            RecordedThread
            RecordedThreadGroup
            RecordingFile
            RecordingStream)))

(extend-protocol Datafiable
  Configuration
  (datafy [this]
    (sorted-map
      :name (.getName this)
      :provider (.getProvider this)
      :description (.getDescription this)
      :label (.getLabel this)
      :settings (into (sorted-map) (datafy/datafy (.getSettings this)))))

  RecordedEvent
  (datafy [this]
    (let [type-name (.getName (.getEventType this))
          stack-trace (datafy/datafy (.getStackTrace this))
          thread (.getThread this)]
      (cond->
        #:event{:duration (.toMillis (.getDuration this))
                :start-time (Date/from (.getStartTime this))
                :end-time (Date/from (.getEndTime this))
                :type (symbol type-name)
                :values (into {}
                          (comp
                            (remove (fn [field] (#{"stackTrace"} (.getName field))))
                            (map (fn [field]
                                 (let [field-name (.getName field)
                                       value (.getValue this field-name)]
                                   (clojure.lang.MapEntry. (symbol field-name)
                                     (cond
                                       (instance? RecordedClass value) (.getName value)
                                       (instance? RecordedThread value) (.getId value)
                                       :else (datafy/datafy value)))))))
                          (.getFields this))}
        stack-trace (assoc :event/stack-trace stack-trace)
        thread (assoc :event/thread (datafy/datafy thread)))))

  ValueDescriptor
  (datafy [this]
    (let [array? (.isArray this)
          label (.getLabel this)
          description (.getDescription this)
          content-type (.getContentType this)]
      (clojure.lang.MapEntry. (symbol (.getName this))
        (cond->
          #:field{:type-name (symbol (.getTypeName this))
                  :type-id (.getTypeId this)
                  :array? array?}
          label (assoc :field/label label)
          content-type (assoc :field/content-type (symbol content-type))
          description (assoc :field/description description)
          array? (assoc :field/fields (mapv datafy/datafy (.getFields this)))))))

  SettingDescriptor
  (datafy [this]
    (let [description (.getDescription this)
          content-type (.getContentType this)]
      (clojure.lang.MapEntry. (symbol (.getName this))
        (cond->
          #:setting{:label (.getLabel this)
                    :default-value (.getDefaultValue this)
                    :type-id (.getTypeId this)
                    :type-name (symbol (.getTypeName this))}
          content-type (assoc :setting/content-type (symbol content-type))
          description (assoc :setting/description description)))))

  AnnotationElement
  (datafy [this]
    ;; TODO
    )

  EventType
  (datafy [this]
    (let [description (.getDescription this)
          label (.getLabel this)]
      (cond->
        ;; TODO
        #:event-type{#_#_:annotation-elements (mapv datafy/datafy (.getAnnotationElements this))
                     :category-names (.getCategoryNames this)
                     :fields (into {} (map datafy/datafy) (.getFields this))
                     :id (.getId this)
                     :name (symbol (.getName this))
                     :settings (into {} (map datafy/datafy) (.getSettingDescriptors this))
                     :enabled? (.isEnabled this)}
        label (assoc :event-type/label label)
        description (assoc :event-type/description description))))
  

  RecordedClassLoader
  (datafy [this]
    (let [name (.getName this)
          type (.getType this)]
      (cond->
        #:class-loader{:id (.getId this)}
        name (assoc :class-loader/name name)
        type (assoc :class-loader/type (datafy/datafy type)))))

  RecordedClass
  (datafy [this]
    (let [loader (.getClassLoader this)]
      (cond->
        #:class{:id (.getId this)
                :modifiers (.getModifiers this)
                :name (.getName this)}
        loader (assoc :class/loader (datafy/datafy loader)))))

  RecordedMethod
  (datafy [this]
    (let [type (.getType this)]
      (cond->
        #:method{:descriptor (.getDescriptor this)
                 :modifiers (.getModifiers this)
                 :name (symbol (.. this getType getName) (.getName this))
                 :hidden? (.isHidden this)}
        type (assoc :method/type (datafy/datafy type)))))

  RecordedFrame
  (datafy [this]
    #:frame{:bytecode-index (.getBytecodeIndex this)
            :line-number (.getLineNumber this)
            :method (datafy/datafy (.getMethod this))
            :type (.getType this)
            :java-frame? (.isJavaFrame this)})

  RecordedObject
  (datafy [this]
    (map (fn [field]
           [(.getName field) (.getValue this (.getName field))]
           (assoc (datafy/datafy field) :field/value (.getValue this (.getName field))))
      (.getFields this)))

  RecordedStackTrace
  (datafy [this]
    #:stack-trace{:frames (mapv datafy/datafy (.getFrames this))
                  :truncated? (.isTruncated this)})

  RecordedThread
  (datafy [this]
    (let [java-name (.getJavaName this)
          group (.getThreadGroup this)]
      (cond->
        #:thread{:id (.getId this)
                 :java-thread-id (.getJavaThreadId this)
                 :os-name (.getOSName this)
                 :os-thread-id (.getOSThreadId this)
                 :virtual? (.isVirtual this)}
        group (assoc :thread/group (datafy/datafy (.getThreadGroup this)))
        java-name (assoc :thread/java-name java-name))))

  RecordedThreadGroup
  (datafy [this]
    (let [parent (.getParent this)]
      (cond-> #:thread-group{:name (.getName this)}
        parent (assoc :thread-group/parent (datafy/datafy parent))))))

(comment
  (datafy/datafy (Configuration/getConfiguration "default"))
  (datafy/datafy (Configuration/getConfiguration "profile"))
  ,,,)

(defn start
  [& {:keys [configuration]
      :or {configuration :default}}]
  (let [config (Configuration/getConfiguration (name configuration))
        destination (Files/createTempFile "repl-" ".jfr" (into-array FileAttribute []))
        recording (Recording. config)]
    (println (.getCanonicalPath (.toFile destination)))
    (.setToDisk recording true)
    (.setDestination recording destination)
    (doto recording .start)))

(defn stop
  [recording]
  (.stop recording)
  (.getDestination recording))

(comment
  (start)
  (stop *1)

  (def m (into (sorted-map)
           (zipmap (map (comp keyword str char) (range 97 123))
             (range 1 26))))

  (with-open [_recording (start :configuration :profile)]
    (tutkain.pprint/pprint m))

  (stop *2)
  ,,,)

(defmacro streaming
  [opts & body]
  `(let [consumer# (reify Consumer
                     (accept [this# arg#]
                       (tap> (datafy/datafy arg#))))
         config# (Configuration/getConfiguration (:configuration ~opts "default"))]
     (with-open [rs# (RecordingStream. config#)]
       (run! (fn [[event# val#]]
               (let [[event-name# setting#] (string/split event# #"#")]
                 (when (and (= setting# "enabled") (= val# "true"))
                   (.onEvent rs# event-name# consumer#))))
         (.getSettings config#))
       (.startAsync rs#)
       (let [ret# (do ~@body)]
         (.awaitTermination rs# (Duration/ofSeconds 5))
         ret#))))

(comment
  (def m (into (sorted-map)
           (zipmap (map (comp keyword str char) (range 97 123))
             (range 1 26))))

  (add-tap (bound-fn [x] (prn x)))

  (streaming {:configuration "default"}
    (clojure.pprint/pprint m))
  ,,,)
