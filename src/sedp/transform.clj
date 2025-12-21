(ns sedp.transform
  (:require [sedp.path :as path]
            [clojure.string :as str]))

(defn to-html [node]
  (cond
    (string? node) node

    (vector? node)
    (let [[tag attrs & content] node
          attr-str (when (map? attrs)
                     (->> attrs
                          (map (fn [[k v]] (str (name k) "=\"" v "\"")))
                          (str/join " ")))
          content-html (map to-html content)]

      (if (seq attr-str)
        (str "<" (name tag) " " attr-str ">"
             (apply str content-html)
             "</" (name tag) ">")
        (str "<" (name tag) ">"
             (apply str content-html)
             "</" (name tag) ">")))

    :else (str node)))

(def html-templates
  {:book (fn [node]
           (let [title (path/get-text (first (path/query node "title")))
                 author (path/get-text (first (path/query node "author")))
                 price (path/get-text (first (path/query node "price")))]
             (str "<div class='book'>"
                  "<h3>" title "</h3>"
                  "<p>Author: " author "</p>"
                  "<p class='price'>$" price "</p>"
                  "</div>")))

   :catalog (fn [node]
              (let [books (path/query node "book")]
                (str "<html><head><title>Catalog</title></head>"
                     "<body><h1>Book Catalog</h1>"
                     (apply str (map (get html-templates :book) books))
                     "</body></html>")))})