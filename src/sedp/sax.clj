(ns sedp.sax
  (:require [clojure.string :as str]
            [sedp.node :as node]))

;; -----------------------------
;; SAX-like API
;; -----------------------------
(defprotocol Handler
  (start-document [this])
  (end-document [this])
  (start-element [this tag attrs])
  (end-element [this tag])
  (characters [this text]))

(defn- indent [n]
  (apply str (repeat n "  "))) ; 2 пробела

(defn- attrs->str [attrs]
  (when (and attrs (seq attrs))
    (->> attrs
         (map (fn [[k v]] (str (name k) "=\"" v "\"")))
         (str/join " "))))

;; -----------------------------
;; 1) DOM builder: собирает дерево из событий
;; -----------------------------
(defrecord DomBuilder [result stack]
  Handler
  (start-document [this]
    (assoc this :result nil :stack []))

  (end-document [this]
    (:result this))

  (start-element [this tag attrs]
    (let [tag   (if (keyword? tag) tag (keyword (str tag)))
          attrs (when (and attrs (seq attrs)) attrs)
          frame (cond-> [tag]
                        attrs (conj attrs))]
      (update this :stack conj frame)))

  (characters [this text]
    (let [t (str text)]
      (if (seq (:stack this))
        (update-in this [:stack (dec (count (:stack this)))] conj t)
        this)))

  (end-element [this _tag]
    (let [stack (:stack this)
          current (peek stack)
          stack' (pop stack)]
      (if (seq stack')
        (let [parent (peek stack')
              updated-parent (conj parent current)]
          (assoc this :stack (conj (pop stack') updated-parent)))
        (assoc this :result current :stack [])))))

(defn make-dom-builder []
  (->DomBuilder nil []))

;; -----------------------------
;; 2) HTML writer: HTML по событиям
;; -----------------------------
(defrecord HtmlWriter [^StringBuilder sb stack]
  Handler
  (start-document [this]
    (assoc this :sb (StringBuilder.) :stack []))

  (end-document [this]
    (str (:sb this)))

  (start-element [this tag attrs]
    (let [tag   (name tag)
          level (count (:stack this))
          a     (attrs->str attrs)
          sb    (:sb this)

          stack (if (seq (:stack this))
                  (let [p (peek (:stack this))
                        stack' (pop (:stack this))]
                    (when-not (:newline-after-open? p)
                      (.append sb "\n"))
                    (conj stack' (assoc p :has-child? true :newline-after-open? true)))
                  (:stack this))

          open  (if (seq a)
                  (str (indent level) "<" tag " " a ">")
                  (str (indent level) "<" tag ">"))]
      (.append sb open)
      (assoc this :stack
                  (conj stack {:tag tag
                               :level level
                               :text ""
                               :has-child? false
                               :newline-after-open? false}))))

  (characters [this text]
    (let [t (str text)]
      (if (seq (:stack this))
        (update this :stack
                (fn [stk]
                  (let [f (peek stk)]
                    (conj (pop stk) (update f :text str t)))))
        this)))

  (end-element [this tag]
    (let [tag   (name tag)
          sb    (:sb this)
          f     (peek (:stack this))
          stack (pop (:stack this))
          level (:level f)
          txt   (str/trim (:text f))]

      (if (:has-child? f)
        (do
          (.append sb (str "\n" (indent level) "</" tag ">\n"))
          (assoc this :stack stack))
        (do
          (when (seq txt)
            (.append sb txt))
          (.append sb (str "</" tag ">\n"))
          (if (seq stack)
            (let [p (peek stack)
                  stack' (pop stack)]
              (assoc this :stack (conj stack' (assoc p :has-child? true :newline-after-open? true))))
            (assoc this :stack stack)))))))

(defn make-html-writer []
  (->HtmlWriter (StringBuilder.) []))

;; -----------------------------
;; 3) Validator: схема по событиям
;; -----------------------------
(defrecord Validator [schema errors stack]
  Handler
  (start-document [this]
    (assoc this :errors [] :stack []))

  (end-document [this]
    (:errors this))

  (start-element [this tag attrs]
    (let [tag (if (keyword? tag) tag (keyword (str tag)))
          elem-schema (get (:schema this) tag)

          this (if elem-schema
                 (reduce
                   (fn [acc [a rule]]
                     (if (and (= true (:required rule))
                              (not (contains? (or attrs {}) a)))
                       (update acc :errors conj [:missing-attr tag a])
                       acc))
                   this
                   (:attrs elem-schema))
                 this)

          frame {:tag tag :elem-schema elem-schema :child-counts {}}]
      (update this :stack conj frame)))

  (characters [this _text] this)

  (end-element [this tag]
    (let [tag (if (keyword? tag) tag (keyword (str tag)))
          frame (peek (:stack this))
          stack' (pop (:stack this))
          elem-schema (:elem-schema frame)
          child-counts (:child-counts frame)

          this (if elem-schema
                 (reduce
                   (fn [acc [child-tag rules]]
                     (let [cnt (get child-counts child-tag 0)
                           mn  (get rules :min 0)
                           mx  (:max rules)]
                       (cond
                         (< cnt mn) (update acc :errors conj [:too-few tag child-tag cnt mn])
                         (and mx (> cnt mx)) (update acc :errors conj [:too-many tag child-tag cnt mx])
                         :else acc)))
                   this
                   (:children elem-schema))
                 this)

          this (assoc this :stack stack')]
      (if (seq stack')
        (update-in this [:stack (dec (count stack')) :child-counts tag] (fnil inc 0))
        this))))

(defn make-validator [schema]
  (->Validator schema [] []))

(defn parse [input handler]
  (let [data (if (string? input) (read-string input) input)
        handler (start-document handler)]
    (letfn [(walk [n h]
              (cond
                (vector? n)
                (let [[tag attrs children] (node/node-parts n)
                      attrs (when (seq attrs) attrs)
                      h (start-element h tag attrs)
                      h (reduce (fn [acc item] (walk item acc)) h children)
                      h (end-element h tag)]
                  h)

                (string? n)
                (characters h n)

                :else
                (characters h (str n))))]
      (end-document (walk data handler)))))
