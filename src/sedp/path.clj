(ns sedp.path
  (:refer-clojure :exclude [descendants])
  (:require [clojure.string :as str]
            [clojure.zip :as zip]))

;; ========== БАЗОВЫЕ УТИЛИТЫ ==========

(defn- tag=
  "Проверяет, что узел является элементом с заданным тегом.
   Поддерживает wildcard '*'."
  [node tag-name]
  (and (vector? node)
       (or (= tag-name "*")
           (= (first node) (keyword tag-name)))))

(defn- get-attrs
  "Возвращает map атрибутов узла или nil, если атрибутов нет."
  [node]
  (when (and (vector? node) (>= (count node) 2))
    (let [second-item (second node)]
      (when (map? second-item) second-item))))

(defn- get-children
  "Возвращает дочерние элементы и текст узла,
   корректно обрабатывая наличие атрибутов."
  [node]
  (when (vector? node)
    (if (map? (second node))
      (drop 2 node)
      (rest node))))

(defn- get-attr
  "Возвращает значение атрибута attr-name у узла."
  [node attr-name]
  (when-let [attrs (get-attrs node)]
    (get attrs (keyword attr-name))))

(defn- vector-children
  "Возвращает только дочерние элементы (вектор-узлы), без текста."
  [node]
  (filter vector? (get-children node)))

;; ========== ПАРСИНГ ПУТЕЙ ==========

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
  [node step-info]
  (let [{:keys [tag attr value]} step-info]
    (and (tag= node tag)
         (or (nil? attr)
             (= (get-attr node attr) value)))))

(defn- descendants
  "Все потомки node (только вектор-узлы), без самого node."
  [node]
  (rest (tree-seq vector? vector-children node)))

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

;; ========== ОСНОВНАЯ ФУНКЦИЯ ЗАПРОСОВ ==========

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

;; ========== ПОЛУЧЕНИЕ ТЕКСТА ==========

(defn get-text [node]
  (when (vector? node)
    (->> (get-children node)
         (filter string?)
         first)))

;; ========== МОДИФИКАЦИЯ ==========

(defn update-nodes [root path f & args]
  (let [z (zip/zipper vector?
                      (fn [x] (seq (get-children x)))
                      (fn [node children]
                        (if (map? (second node))
                          (vec (concat [(first node) (second node)] children))
                          (vec (concat [(first node)] children))))
                      root)
        matches (query root path)]
    (loop [loc z]
      (if (zip/end? loc)
        (zip/root loc)
        (let [node (zip/node loc)]
          (if (some #{node} matches)
            (recur (zip/next (zip/replace loc (apply f node args))))
            (recur (zip/next loc))))))))

;; ========== ДОПОЛНИТЕЛЬНЫЕ ФУНКЦИИ ==========

(defn one
  "Возвращает первый узел, найденный по path (или nil, если ничего не найдено).
   Это просто (first (query root path)), но читается намного понятнее."
  [root path]
  (first (query root path)))

(defn one-text
  "Частый кейс: взять первый найденный узел и извлечь его текст.
   Эквивалентно: (get-text (one root path))"
  [root path]
  (get-text (one root path)))

(defn set-attr
  "Устанавливает (или добавляет) атрибут для всех узлов,
   найденных по пути `path`. Возвращает новое дерево."
  [root path attr value]
  (update-nodes root path
                (fn [node]
                  (let [[tag & more] node
                        [attrs content] (if (map? (first more))
                                          [(first more) (rest more)]
                                          [{} more])
                        new-attrs (assoc attrs (keyword attr) value)]
                    (vec (concat [tag new-attrs] content))))))