(ns sedp.path
  (:refer-clojure :exclude [descendants])
  (:require [clojure.string :as str]
            [clojure.zip :as zip]
            [sedp.node :as node]))

(defn- tag=
  "Проверяет, что узел является элементом с заданным тегом.
   Поддерживает wildcard '*'."
  [n tag-name]
  (and (vector? n)
       (or (= tag-name "*")
           (= (first n) (keyword tag-name)))))

(defn- get-attrs
  "Возвращает map атрибутов узла или nil, если атрибутов нет.
   НЕСТРОГО: attrs могут быть в любом месте среди элементов узла."
  [n]
  (when (vector? n)
    (let [[_ attrs _] (node/node-parts n)]
      (when (seq attrs) attrs))))

(defn- get-children
  "Возвращает дочерние элементы и текст узла.
   НЕСТРОГО: attrs могут быть в любом месте."
  [n]
  (when (vector? n)
    (let [[_ _ children] (node/node-parts n)]
      children)))

(defn- get-attr
  "Возвращает значение атрибута attr-name у узла."
  [n attr-name]
  (when-let [attrs (get-attrs n)]
    (get attrs (keyword attr-name))))

(defn- vector-children
  "Возвращает только дочерние элементы (вектор-узлы), без текста."
  [n]
  (filter vector? (get-children n)))

(def ^:private desc-token "**") ;; внутренний маркер для //

(defn- parse-step
  "Разбирает один шаг XPath-подобного пути.
   Поддерживает:
   - tag
   - tag[@attr='value']
   - *"
  [step]
  (if-let [[_ tag attr value]
           (re-find #"([^\[]+)\[@([^=]+)=['\"]?([^'\"]+)['\"]?\]" step)]
    {:tag tag :attr attr :value value}
    {:tag step}))

(defn- match-step?
  "Проверяет, соответствует ли узел шагу пути
   (по тегу и, при наличии, по атрибуту)."
  [n step-info]
  (let [{:keys [tag attr value]} step-info]
    (and (tag= n tag)
         (or (nil? attr)
             (= (get-attr n attr) value)))))

(defn- descendants
  "Все потомки node (только вектор-узлы), без самого node."
  [n]
  (rest (tree-seq vector? vector-children n)))

(defn- parse-path
  "Разбирает XPath-подобный путь в последовательность шагов.
   Поддержка:
   - /
   - //
   - условия по атрибутам
   Пример: catalog//book[@id='2']"
  [path]
  (let [p (-> path
              str/trim
              (str/replace #"//+" (str "/" desc-token "/")))
        parts (->> (str/split p #"/")
                   (remove str/blank?)
                   (remove #(= % ".")))]
    (loop [xs parts
           axis :child
           acc []]
      (if-let [x (first xs)]
        (if (= x desc-token)
          (recur (rest xs) :desc acc)
          (recur (rest xs) :child (conj acc (assoc (parse-step x) :axis axis))))
        acc))))

(defn query
  "Выполняет запрос к дереву S-выражений.
   Поддерживает пути вида:
   - root/a/b
   - a/b (относительный, от корня)
   - root/a[@id='1']
   - catalog//title (переменная вложенность через //)"
  [root path]
  (let [steps (parse-path path)]
    (when (seq steps)
      (letfn [(apply-step [nodes {:keys [axis] :as step}]
                (let [candidates (case axis
                                   :child (mapcat vector-children nodes)
                                   :desc  (mapcat descendants nodes)
                                   (mapcat vector-children nodes))]
                  (filter #(match-step? % step) candidates)))]
        (let [first-step (first steps)]
          (if (and (= (:axis first-step) :child)
                   (match-step? root first-step))
            (reduce apply-step [root] (rest steps))
            (reduce apply-step [root] steps)))))))

(defn get-text [n]
  (when (vector? n)
    (->> (get-children n)
         (filter string?)
         first)))

(defn update-nodes
  "Находит все узлы по path и заменяет их результатом (f node & args)."
  [root path f & args]
  (let [z (zip/zipper vector?
                      (fn [x] (seq (get-children x)))
                      (fn [n children]
                        (let [[tag attrs _] (node/node-parts n)]
                          (node/rebuild-node tag attrs (vec children))))
                      root)
        matches (query root path)]
    (loop [loc z]
      (if (zip/end? loc)
        (zip/root loc)
        (let [n (zip/node loc)]
          (if (some #{n} matches)
            (recur (zip/next (zip/replace loc (apply f n args))))
            (recur (zip/next loc))))))))

(defn one [root path] (first (query root path)))

(defn one-text [root path] (get-text (one root path)))

(defn set-attr
  "Устанавливает/добавляет атрибут для всех узлов найденных по `path`.
   Работает при attrs в любом месте узла."
  [root path attr value]
  (update-nodes root path
                (fn [n]
                  (let [[tag attrs children] (node/node-parts n)
                        new-attrs (assoc attrs (keyword attr) value)]
                    (node/rebuild-node tag new-attrs children)))))
