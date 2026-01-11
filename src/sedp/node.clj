(ns sedp.node)

(defn node-parts
  "Разбирает узел-элемент вида вектора.
   Поддерживает 'нестрогую' форму, где map-атрибуты могут встречаться
   в любом месте среди потомков.

   Возвращает [tag attrs children], где attrs — слитая map (может быть {})."
  [node]
  (let [[tag & more] node
        attrs (apply merge {} (filter map? more))
        children (remove map? more)]
    [tag attrs (vec children)]))

(defn rebuild-node
  "Собирает узел обратно в каноническом виде:
   [:tag {:a 1} child1 child2 ...] (если attrs непустые)
   или [:tag child1 child2 ...] (если attrs пустые)."
  [tag attrs children]
  (if (and (map? attrs) (seq attrs))
    (vec (concat [tag attrs] children))
    (vec (concat [tag] children))))

(defn el
  "Простой builder для тега.

   (el :a {:id \"1\"} \"x\" (el :b \"y\"))
   (el :a \"x\" {:id \"1\"} (el :b \"y\"))     ;; attrs можно ставить где угодно
   (el :a {:id \"1\"} {:class \"c\"} \"x\")    ;; attrs можно дробить на несколько map

   Всегда возвращает канонический вектор узла."
  [tag & items]
  (let [attrs (apply merge {} (filter map? items))
        children (remove map? items)]
    (rebuild-node tag attrs (vec children))))

(defn attrs
  "Достаёт attrs из узла (как map) или {}."
  [node]
  (if (vector? node)
    (let [[_ a _] (node-parts node)] a)
    {}))
