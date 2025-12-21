(ns sedp.main
  (:require [sedp.path :as path]
            [sedp.transform :as transform]))

(def example-doc
  [:catalog
   [:book {:id "1" :category "tech"}
    [:title "Clojure Programming"]
    [:author "Alex"]
    [:price "45.99"]]
   [:book {:id "2" :category "fiction"}
    [:title "The Novel"]
    [:author "Maria"]
    [:price "29.99"]]])

(defn -main [& args]
  (println "=== SEDP - S-Expression Data Processor ===")
  (println)

  (println "1. Все книги:")
  (doseq [book (path/query example-doc "catalog/book")]
    (println "   -" book))

  (println "\n2. Технические книги:")
  (doseq [book (path/query example-doc "catalog/book[@category='tech']")]
    (println "   -" (path/get-text (first (path/query book "title")))))

  (println "\n3. HTML представление каталога:")
  (println (transform/to-html (first (path/query example-doc "catalog")))))