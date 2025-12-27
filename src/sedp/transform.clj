(ns sedp.transform
  (:require [clojure.string :as str]))

(defn- node-parts
  "Разбирает узел вида:
   [:tag {:a 1} child1 child2 ...]  или  [:tag child1 child2 ...]
   Возвращает [tag attrs children]."
  [node]
  (let [[tag & more] node
        [attrs children] (if (map? (first more))
                           [(first more) (rest more)]
                           [{} more])]
    [tag attrs children]))

(defn- attrs->str [attrs]
  (->> attrs
       (map (fn [[k v]] (str (name k) "=\"" v "\"")))
       (str/join " ")))

(defn- indent [level]
  (apply str (repeat level "  "))) ; 2 пробела на уровень

(defn- inline-text-node?
  [children]
  (and (seq children)
       (not-any? vector? children)))

(defn to-html-pretty
  "Преобразует дерево в HTML-строку с переносами и отступами.
   Если узел вида [:title \"Text\"] — печатаем в одну строку:
   <title>Text</title>"
  ([node]
   (to-html-pretty node 0))
  ([node level]
   (cond
     (string? node)
     (str (indent level) node "\n")

     (vector? node)
     (let [[tag attrs children] (node-parts node)
           attr-str (attrs->str attrs)
           tag-name (name tag)
           open-inline (if (seq attr-str)
                         (str "<" tag-name " " attr-str ">")
                         (str "<" tag-name ">"))
           close-inline (str "</" tag-name ">")]

       ;; inline: внутри только текст, без вложенных элементов
       (if (inline-text-node? children)
         (str (indent level)
              open-inline
              (apply str (map str children))
              close-inline
              "\n")

         ;; многострочный: внутри есть вложенные элементы
         (let [open  (str (indent level) open-inline "\n")
               inner (apply str (map #(to-html-pretty % (inc level)) children))
               close (str (indent level) close-inline "\n")]
           (str open inner close))))

     :else
     (str (indent level) (str node) "\n"))))
