(ns sedp.test
  (:require [clojure.test :refer :all]
            [sedp.path :as path]))

(def test-doc-basic
  [:root
   [:a
    [:b "value"]]])

(def test-doc-with-attrs
  [:root
   [:a {:id "1"} "x"]
   [:a {:id "2"} "y"]])

(def test-doc-complex
  [:catalog
   [:book {:id "1" :category "tech"}
    [:title "Clojure"]
    [:price "100"]]
   [:book {:id "2" :category "fiction"}
    [:title "Novel"]
    [:price "50"]]])

(deftest basic-query-test
  (testing "Базовый запрос по пути"
    (let [result (path/query test-doc-basic "root/a/b")]
      (is (= 1 (count result)))
      (is (= [:b "value"] (first result))))))

(deftest attribute-query-test
  (testing "Запрос с атрибутом"
    (let [result (path/query test-doc-with-attrs "root/a[@id='1']")]
      (is (= 1 (count result)))
      (is (= [:a {:id "1"} "x"] (first result))))))

(deftest complex-query-test
  (testing "Комплексный запрос"
    (let [result (path/query test-doc-complex "catalog/book[@category='tech']/title")]
      (is (= 1 (count result)))
      (is (= [:title "Clojure"] (first result))))))

(deftest simple-path-test
  (testing "Простой путь"
    (let [doc [:root [:a "1"] [:b "2"]]
          result (path/query doc "root/a")]
      (is (= [:a "1"] (first result))))))

(deftest get-text-test
  (testing "Получение текста из узла"
    (let [node [:title "Book Title"]]
      (is (= "Book Title" (path/get-text node))))))

(defn test-ns-hook []
  (basic-query-test)
  (attribute-query-test)
  (complex-query-test)
  (simple-path-test)
  (get-text-test))