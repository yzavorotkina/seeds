(ns sedp.path
  (:require [clojure.string :as str]
            [clojure.zip :as zip]))

;; ========== БАЗОВЫЕ УТИЛИТЫ ==========

(defn- tag= [node tag-name]
  (and (vector? node)
       (or (= tag-name "*")
           (= (first node) (keyword tag-name)))))

(defn- get-attrs [node]
  (when (and (vector? node) (>= (count node) 2))
    (let [second-item (second node)]
      (when (map? second-item) second-item))))

(defn- get-children [node]
  (when (vector? node)
    (if (map? (second node))
      (drop 2 node)
      (rest node))))

(defn- get-attr [node attr-name]
  (when-let [attrs (get-attrs node)]
    (get attrs (keyword attr-name))))

(defn- vector-children [node]
  (filter vector? (get-children node)))

;; ========== ПАРСИНГ ПУТЕЙ ==========

(def ^:private desc-token "**") ;; внутренний маркер для //

(defn- parse-step [step]
  ;; tag[@attr='value']  или просто tag  или *
  (if-let [[_ tag attr value]
           (re-find #"([^\[]+)\[@([^=]+)=['\"]?([^'\"]+)['\"]?\]" step)]
    {:tag tag :attr attr :value value}
    {:tag step}))

(defn- match-step? [node step-info]
  (let [{:keys [tag attr value]} step-info]
    (and (tag= node tag)
         (or (nil? attr)
             (= (get-attr node attr) value)))))

(defn- descendants
  "Все потомки node (только вектор-узлы), без самого node."
  [node]
  (rest (tree-seq vector? vector-children node)))

(defn- parse-path
  "Поддержка:
   - обычный / для перехода к детям
   - // для поиска на любой глубине (как XPath descendant-or-self, но мы берем потомков)
   Пример: catalog//title"
  [path]
  (let [p (-> path
              str/trim
              ;; превращаем '//' в '/**/' чтобы split видел отдельный сегмент
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
          ;; КЛЮЧЕВОЕ ИСПРАВЛЕНИЕ:
          ;; если первый шаг совпадает с корнем, считаем его обращением к самому root
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

(defn set-attr [root path attr value]
  (update-nodes root path
                (fn [node]
                  (if-let [attrs (get-attrs node)]
                    (assoc-in node [1 (keyword attr)] value)
                    (let [[tag & content] node]
                      (vec (concat [tag {(keyword attr) value}] content)))))))

;; ========== ДОПОЛНИТЕЛЬНЫЕ ФУНКЦИИ ==========

(defn select [root & path-parts]
  (query root (str/join "/" path-parts)))

(defn attr [node attr-name]
  (get-attr node attr-name))

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