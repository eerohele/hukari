(ns hukari.http
  (:require [clojure.core.protocols :refer [Datafiable]]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [clojure.datafy :as datafy]
            [clojure.java.io :as io])
  (:import (java.io EOFException)
           (java.net.http HttpClient HttpClient$Redirect HttpClient$Version HttpRequest HttpRequest$Builder HttpResponse HttpResponse$ResponseInfo HttpRequest$BodyPublishers HttpResponse$BodyHandlers HttpResponse$BodySubscribers)
           (java.net URI)
           (java.time Duration)
           (java.util.concurrent CompletableFuture)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn ^:private persist [m]
  (persistent!
    (reduce-kv
      (fn [m k v]
        (assoc! m k (vec v)))
      (transient {})
      m)))

(extend-protocol Datafiable
  HttpRequest
  (datafy [this]
    (let [timeout (.orElse (.timeout this) nil)
          version (.orElse (.version this) nil)]
      (cond->
        {:expect-continue (.expectContinue this)
         :hash-code (.hashCode this)
         :headers (persist (.map (.headers this)))
         :method (.method this)
         :uri (.uri this)}
        timeout (assoc :timeout timeout)
        version (assoc :version version))))

  HttpResponse
  (datafy [this]
    (let [body (.body this)]
      (cond-> {:status (.statusCode this)
               :headers (persist (.map (.headers this)))
               :version (.version this)
               :uri (.uri this)}
        body (assoc :body body)))))

(defonce ^:private default-http-client
  (..
    (HttpClient/newBuilder)
    (version HttpClient$Version/HTTP_2)
    (followRedirects HttpClient$Redirect/NORMAL)
    (connectTimeout (Duration/ofSeconds 10))
    (build)))

(defn ^:private add-headers [^HttpRequest$Builder request-builder headers]
  (run! (fn [[k v]] (.setHeader request-builder k v)) headers))

(defn request
  ^CompletableFuture
  [& {:keys [http-client method uri headers body-publisher body-handler]
      :or {http-client default-http-client
           body-publisher (HttpRequest$BodyPublishers/noBody)
           body-handler (HttpResponse$BodyHandlers/ofString)
           method :get}}]
  (let [method (-> method name string/upper-case)
        request-builder (doto
                          (..
                            (HttpRequest/newBuilder)
                            (method method body-publisher))
                          (add-headers headers))
        request (.. request-builder (uri (URI. uri)) (build))]
    (..
      ^HttpClient http-client
      (sendAsync request body-handler)
      (thenApply
        (fn [response]
          (->
            (datafy/datafy response)
            (with-meta {:request (datafy/datafy request)})))))))

(comment
  (def h (HttpClient/newHttpClient))

  (dotimes [_ 25]
    (let [request (..
                    (HttpRequest/newBuilder)
                    (method "GET" (HttpRequest$BodyPublishers/noBody))
                    (uri (URI. "http://localhost:8090")) (build))]
      (.send default-http-client request (HttpResponse$BodyHandlers/discarding))))

  @(request
     :method :get
     :uri "https://jsonplaceholder.typicode.com/todos/1?baz=quux"
     :headers {"foo" "bar"})
  ,,,)

(defn ^:private json-body-publisher
  [body]
  (if (some? body)
    (HttpRequest$BodyPublishers/ofString (json/write-str body))
    (HttpRequest$BodyPublishers/noBody)))

(defn ^:private content-length
  ^long [^HttpResponse$ResponseInfo response-info]
  (..
    response-info
    (headers)
    (firstValue "Content-Length")
    (orElse 0)))

(defn ^:private json-body-handler
  [_]
  (HttpResponse$BodySubscribers/mapping
    (HttpResponse$BodySubscribers/ofByteArray)
    (fn [body]
      (with-open [reader (io/reader body)]
        (try
          (json/read reader :key-fn keyword)
          (catch EOFException _ nil))))))

(defn json-request
  ^CompletableFuture [& {:keys [body] :as req}]
  (->
    req
    (assoc-in [:headers "Content-Type"] "application/json; charset=utf-8")
    (assoc-in [:headers "Accept"] "application/json; charset=utf-8")
    (assoc :body-publisher (json-body-publisher body))
    (assoc :body-handler json-body-handler)
    (request)))

(comment
  @(json-request
     :method :get
     :uri "https://jsonplaceholder.typicode.com/todos/1"
     :headers {"foo" "bar"})
  
  (:body @(json-request
            :method :get
            :uri "https://jsonplaceholder.typicode.com/todos/foo"))

  
  
  
  (import
    '(java.util.concurrent CountDownLatch Executors)
    '(java.util.concurrent TimeUnit))
  
  (defmacro concurrently
    [{:keys [threads]
      :or {threads 1000}} & body]
    `(let [executor# (Executors/newVirtualThreadPerTaskExecutor)]
       (try
         (let [latch# (CountDownLatch. ~threads)]
           (dotimes [_i# ~threads]
             (.execute executor#
               (^:once fn* []
                (.countDown latch#)
                (.await latch#)
                (do ~@body)))))
         (finally
           (.shutdown executor#)
           (.awaitTermination executor#
             Long/MAX_VALUE TimeUnit/MILLISECONDS)))))
  
  (require '[clojure.repl.deps :as deps])
  (deps/add-lib 'http-kit/http-kit)
  
  (require '[org.httpkit.server :as http-server])
  
  (def server (http-server/run-server (fn [_] (Thread/sleep 100) {:status 200 :body ""})))
  (server)
  
  (time
    (concurrently {:threads 10000}
      @(request :uri "http://localhost:8090")))
  
  (require '[org.httpkit.client :as http-client])
  
  (time
    (concurrently {:threads 10000}
      @(http-client/get "http://localhost:8090")))
  ,,,)
