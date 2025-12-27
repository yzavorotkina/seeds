(ns sedp.main
  (:gen-class)
  (:require [sedp.path :as path]
            [sedp.transform :as transform]
            [sedp.schema :as schema]
            [sedp.sax :as sax]))

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

(defn -main [& _args]
  (println "=== SEDP - S-Expression Data Processor ===\n")

  ;; -----------------------------
  ;; 1) XPath-lite: базовый поиск
  ;; -----------------------------
  (println "1. Все книги (query catalog/book):")
  (doseq [book (path/query example-doc "catalog/book")]
    (println "   -" book))

  (println "\n2. Технические книги (query catalog/book[@category='tech'] → title):")
  (doseq [book (path/query example-doc "catalog/book[@category='tech']")]
    (println "   -" (path/one-text book "title")))

  ;; -----------------------------
  ;; 2) Модификация
  ;; -----------------------------
  (println "\n3. Модификация: сделаем вторую книгу tech через set-attr и снова запросим:")
  (let [doc2 (path/set-attr example-doc "catalog/book[@id='2']" "category" "tech")]
    (doseq [book (path/query doc2 "catalog/book[@category='tech']")]
      (println "   -" (path/one-text book "title"))))

  ;; -----------------------------
  ;; Доп: трансформация в HTML (DOM режим)
  ;; -----------------------------
  (println "\n4. HTML (DOM → to-html-pretty):")
  (println (transform/to-html-pretty (path/one example-doc "catalog")))

  ;; -----------------------------
  ;; 3) Schema validation (DOM режим)
  ;; -----------------------------
  (println "5. Валидация по схеме (DOM → schema/validate):")
  (let [errs (schema/validate example-doc schema/example-schema)]
    (if (empty? errs)
      (println "   OK: ошибок нет\n")
      (do (println "   Найдены ошибки:")
          (doseq [e errs] (println "   " e))
          (println))))

  ;; -----------------------------
  ;; Доп: SAX режим — трансформация и валидация прямо по событиям
  ;; -----------------------------
  (let [doc-str (pr-str example-doc)]
    (println "6. SAX режим: трансформация в HTML по событиям:")
    (println (sax/parse doc-str (sax/make-html-writer)))

    (println "7. SAX режим: валидация по схеме по событиям:")
    (let [errs2 (sax/parse doc-str (sax/make-validator schema/example-schema))]
      (if (empty? errs2)
        (println "   OK: ошибок нет\n")
        (do (println "   Найдены ошибки:")
            (doseq [e errs2] (println "   " e))
            (println))))

    (println "8. SAX режим: сборка DOM из событий:")
    (let [dom (sax/parse doc-str (sax/make-dom-builder))]
      (println "   DOM из SAX:")
      (println "   " dom))))
