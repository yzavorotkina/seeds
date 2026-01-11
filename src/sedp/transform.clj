(ns sedp.transform
  (:require [clojure.string :as str]
            [sedp.node :as node]))

(defn- attrs->str [attrs]
  (->> attrs
       (map (fn [[k v]] (str (name k) "=\"" v "\"")))
       (str/join " ")))

(defn- indent [level]
  (apply str (repeat level "  "))) ; 2 пробела

(defn- inline-text-node?
  [children]
  (and (seq children)
       (not-any? vector? children)))

(defn to-html-pretty
  "Преобразует дерево в HTML-строку с переносами и отступами.
   Если узел вида [:title \"Text\"] — печатаем в одну строку:
   <title>Text</title>"
  ([n] (to-html-pretty n 0))
  ([n level]
   (cond
     (string? n)
     (str (indent level) n "\n")

     (vector? n)
     (let [[tag attrs children] (node/node-parts n)
           attr-str (attrs->str attrs)
           tag-name (name tag)
           open-inline (if (seq attr-str)
                         (str "<" tag-name " " attr-str ">")
                         (str "<" tag-name ">"))
           close-inline (str "</" tag-name ">")]

       (if (inline-text-node? children)
         (str (indent level)
              open-inline
              (apply str (map str children))
              close-inline
              "\n")
         (let [open  (str (indent level) open-inline "\n")
               inner (apply str (map #(to-html-pretty % (inc level)) children))
               close (str (indent level) close-inline "\n")]
           (str open inner close))))

     :else
     (str (indent level) (str n) "\n"))))
