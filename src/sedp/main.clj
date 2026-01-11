(ns sedp.main
  (:gen-class)
  (:require [sedp.path :as path]
            [sedp.transform :as transform]
            [sedp.schema :as schema]
            [sedp.sax :as sax]))

(def example-doc
  [:catalog
   ;; book #1: attrs стоят после детей + attrs разбиты на 2 map
   [:book
    [:title "Clojure Programming"]
    {:id "1"}                 ;; attrs не вторым
    [:author "Alex"]
    {:category "tech"}        ;; второй attrs-map
    [:price "45.99"]]

   ;; book #2: канон форма
   [:book {:id "2" :category "fiction"}
    [:title "The Novel"]
    [:author "Maria"]
    [:price "29.99"]]

   ;; book #3: attrs стоят между детьми + category отдельной map
   [:book
    {:id "3"}
    [:title "No Price Book"]
    [:author "Nina"]
    {:category "tech"}]])

(defn -main [& _args]
  (println "=== SEDP - S-Expression Data Processor ===")
  (println "=== DEMO: attrs can be anywhere inside a node ===\n")

  ;; -----------------------------
  ;; 1) XPath-lite: базовый поиск
  ;; -----------------------------
  (println "1) Все книги (query catalog/book):")
  (doseq [book (path/query example-doc "catalog/book")]
    (println "   -" book))

  (println "\n2) Поиск по атрибуту, даже если attrs НЕ вторым элементом:")
  (println "   book[@id='1']/title ="
           (path/one-text example-doc "catalog/book[@id='1']/title"))

  (println "\n3) Технические книги (query catalog/book[@category='tech'] → title):")
  (doseq [book (path/query example-doc "catalog/book[@category='tech']")]
    (println "   -" (path/one-text book "title")))

  ;; -----------------------------
  ;; 2) Модификация
  ;; -----------------------------
  (println "\n4) set-attr: сделаем вторую книгу tech и снова запросим category='tech':")
  (let [doc2 (path/set-attr example-doc "catalog/book[@id='2']" "category" "tech")]
    (doseq [book (path/query doc2 "catalog/book[@category='tech']")]
      (println "   -" (path/one-text book "title"))))

  ;; -----------------------------
  ;; 3) HTML (DOM режим)
  ;; -----------------------------
  (println "\n5) HTML (DOM → to-html-pretty) для book[@id='1'] (attrs НЕ вторым элементом):")
  (println (transform/to-html-pretty
             (path/one example-doc "catalog/book[@id='1']")))

  ;; -----------------------------
  ;; 4) Schema validation (DOM режим)
  ;; -----------------------------
  (println "6) Валидация по схеме (DOM → schema/validate) на документе с 'плавающими' attrs:")
  (let [errs (schema/validate example-doc schema/example-schema)]
    (if (empty? errs)
      (println "   OK: ошибок нет\n")
      (do (println "   Найдены ошибки:")
          (doseq [e errs] (println "   " e))
          (println))))

  ;; -----------------------------
  ;; 5) SAX режим — трансформация и валидация прямо по событиям
  ;; -----------------------------
  (let [doc-str (pr-str example-doc)]
    (println "7) SAX режим: трансформация в HTML по событиям (attrs НЕ вторым элементом):")
    (println (sax/parse doc-str (sax/make-html-writer)))

    (println "8) SAX режим: валидация по схеме по событиям на том же документе:")
    (let [errs2 (sax/parse doc-str (sax/make-validator schema/example-schema))]
      (if (empty? errs2)
        (println "   OK: ошибок нет\n")
        (do (println "   Найдены ошибки:")
            (doseq [e errs2] (println "   " e))
            (println))))

    (println "9) SAX режим: сборка DOM из событий (показываем канонический вид):")
    (let [dom (sax/parse doc-str (sax/make-dom-builder))]
      (println "   DOM из SAX:")
      (println "   " dom))))
