(ns sedp.attrs-test
  (:require [clojure.test :refer :all]
            [sedp.node :as node]
            [sedp.path :as path]
            [sedp.schema :as schema]
            [sedp.transform :as transform]
            [sedp.sax :as sax]))

;; -----------------------------
;; Документы для тестов
;; -----------------------------

(def doc-attrs-anywhere
  ;; attrs в разных местах + два map (merge)
  [:catalog
   [:book
    [:title "Clojure Programming"]
    {:id "1"}                       ;; attrs не вторым
    [:author "Alex"]
    {:category "tech"}              ;; второй attrs-map
    [:price "45.99"]]
   [:book {:id "2" :category "fiction"} ;; канон
    [:title "The Novel"]
    [:author "Maria"]
    [:price "29.99"]]
   [:book {:id "3"}
    [:title "No Price Book"]
    [:author "Nina"]]])

(def doc-attrs-after-children
  ;; attrs в самом конце узла
  [:root
   [:a "x" [:b "y"] {:id "A"}]])

(def doc-attrs-between-children
  ;; attrs между детьми
  [:root
   [:a [:b "y"] {:id "A"} [:c "z"]]])

;; -----------------------------
;; Схема: проверим required attrs и min/max детей
;; -----------------------------

(def catalog-schema
  {:catalog {:children {:book {:min 1}}}
   :book    {:attrs {:id {:required true}}
             :children {:title  {:min 1 :max 1}
                        :author {:min 1 :max 1}
                        :price  {:min 0 :max 1}}}})

(def bad-doc-missing-id
  [:catalog
   [:book
    [:title "X"]
    [:author "Y"]
    [:price "1.0"]]])

(def bad-doc-too-many-titles
  [:catalog
   [:book {:id "10"}
    [:title "T1"]
    [:title "T2"]
    [:author "A"]]])

;; -----------------------------
;; Тесты node.el / node-parts
;; -----------------------------

(deftest node-el-canonicalization-test
  (testing "node/el merges attrs from anywhere and returns canonical form"
    (is (= [:a {:id "1"} "x" [:b "y"]]
           (node/el :a "x" {:id "1"} (node/el :b "y"))))

    (is (= [:a {:id "1" :class "hot"} "x"]
           (node/el :a {:id "1"} "x" {:class "hot"})))

    ;; канонический вид: attrs всегда вторым элементом, если они есть
    (is (= {:id "1" :category "tech"}
           (node/attrs
             (node/el :book [:title "T"] {:id "1"} {:category "tech"}))))))

;; -----------------------------
;; Тесты path/query на attrs "плавающих"
;; -----------------------------

(deftest path-query-attrs-anywhere-test
  (testing "path/query finds nodes by attribute even if attrs are not second element"
    (is (= 1 (count (path/query doc-attrs-anywhere "catalog/book[@id='1']"))))
    (is (= "Clojure Programming"
           (path/one-text doc-attrs-anywhere "catalog/book[@id='1']/title")))

    (is (= 1 (count (path/query doc-attrs-anywhere "catalog/book[@category='tech']"))))
    (is (= "Clojure Programming"
           (path/one-text doc-attrs-anywhere "catalog/book[@category='tech']/title")))

    (is (= 1 (count (path/query doc-attrs-anywhere "catalog/book[@id='2']"))))
    (is (= "The Novel"
           (path/one-text doc-attrs-anywhere "catalog/book[@id='2']/title")))))

(deftest path-set-attr-attrs-anywhere-test
  (testing "path/set-attr updates nodes even if attrs are located anywhere"
    (let [doc2 (path/set-attr doc-attrs-anywhere "catalog/book[@id='3']" "category" "tech")]
      (is (= 2 (count (path/query doc2 "catalog/book[@category='tech']"))))
      (is (= "No Price Book"
             (path/one-text doc2 "catalog/book[@id='3']/title"))))))

(deftest path-children-robustness-test
  (testing "children parsing ignores attrs maps regardless of position"
    (is (= "y" (path/one-text doc-attrs-after-children "root/a/b")))
    (is (= "y" (path/one-text doc-attrs-between-children "root/a/b")))
    (is (= "z" (path/one-text doc-attrs-between-children "root/a/c")))))

;; -----------------------------
;; Тесты schema/validate (DOM режим)
;; -----------------------------

(deftest schema-validate-attrs-anywhere-test
  (testing "schema/validate supports attrs in any place"
    (is (empty? (schema/validate doc-attrs-anywhere catalog-schema)))

    (is (= [[:missing-attr :book :id]]
           (schema/validate bad-doc-missing-id catalog-schema)))

    (is (= [[:too-many :book :title 2 1]]
           (schema/validate bad-doc-too-many-titles catalog-schema)))))

;; -----------------------------
;; Тесты transform/to-html-pretty (DOM -> HTML)
;; -----------------------------

(deftest transform-html-attrs-anywhere-test
  (testing "transform/to-html-pretty prints attributes even if they are not second element"
    (let [html (transform/to-html-pretty (path/one doc-attrs-anywhere "catalog/book[@id='1']"))]
      ;; проверяем, что id и category реально попали в открывающий тег
      (is (re-find #"<book[^>]*id=\"1\"" html))
      (is (re-find #"<book[^>]*category=\"tech\"" html))
      (is (re-find #"<title>Clojure Programming</title>" html)))))

;; -----------------------------
;; Тесты SAX режима
;; -----------------------------

(deftest sax-validator-attrs-anywhere-test
  (testing "sax/parse + make-validator validates attrs anywhere"
    (let [doc-str (pr-str doc-attrs-anywhere)
          errs (sax/parse doc-str (sax/make-validator catalog-schema))]
      (is (empty? errs)))

    (let [doc-str (pr-str bad-doc-missing-id)
          errs (sax/parse doc-str (sax/make-validator catalog-schema))]
      (is (= [[:missing-attr :book :id]] errs)))))

(deftest sax-dom-builder-attrs-anywhere-test
  (testing "sax/parse + make-dom-builder rebuilds canonical DOM"
    (let [doc-str (pr-str doc-attrs-anywhere)
          dom (sax/parse doc-str (sax/make-dom-builder))
          ;; после сборки DOM атрибуты будут в каноническом месте (вторым элементом)
          book1 (path/one dom "catalog/book[@id='1']")]
      (is (vector? dom))
      (is (= {:id "1" :category "tech"} (node/attrs book1)))
      (is (= "Clojure Programming" (path/one-text book1 "title"))))))

(deftest sax-html-writer-attrs-anywhere-test
  (testing "sax/parse + make-html-writer prints attrs anywhere"
    (let [doc-str (pr-str doc-attrs-anywhere)
          html (sax/parse doc-str (sax/make-html-writer))]
      (is (re-find #"<book[^>]*id=\"1\"" html))
      (is (re-find #"<book[^>]*category=\"tech\"" html))
      (is (re-find #"<title>Clojure Programming</title>" html)))))
