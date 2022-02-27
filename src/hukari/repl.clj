(ns hukari.repl
  (:require [clojure.string :as string]
            [clojure.xml :as xml]
            [clojure.zip :as zip])
  (:import (java.net URI URLEncoder)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)))

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
  (set! *print-namespace-maps* false)

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