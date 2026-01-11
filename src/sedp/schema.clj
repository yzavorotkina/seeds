(ns sedp.schema
  (:require [sedp.node :as node]))

(defn validate
  "Возвращает список ошибок. Схема:
   { :tag {:attrs { :id {:required true} ... }
           :children { :child-tag {:min 1 :max 2} ... }}}"
  [root schema]
  (let [errors (atom [])]
    (letfn [(v* [n]
              (when (vector? n)
                (let [[tag attrs children] (node/node-parts n)
                      elem-schema (get schema tag)]

                  ;; required attrs
                  (when elem-schema
                    (doseq [[attr attr-schema] (:attrs elem-schema)]
                      (when (and (= true (:required attr-schema))
                                 (not (contains? attrs attr)))
                        (swap! errors conj [:missing-attr tag attr])))

                    ;; children min/max
                    (let [child-elems  (filter vector? children)
                          child-counts (frequencies (map first child-elems))]
                      (doseq [[child-tag rules] (:children elem-schema)]
                        (let [cnt (get child-counts child-tag 0)
                              mn  (get rules :min 0)
                              mx  (:max rules)]
                          (when (< cnt mn)
                            (swap! errors conj [:too-few tag child-tag cnt mn]))
                          (when (and mx (> cnt mx))
                            (swap! errors conj [:too-many tag child-tag cnt mx]))))))

                  ;; recurse
                  (doseq [child (filter vector? children)]
                    (v* child)))))]
      (v* root)
      @errors)))

(def example-schema
  {:book {:attrs {:id {:required true}
                  :category {:required false}}
          :children {:title  {:min 1 :max 1}
                     :author {:min 1 :max 1}
                     :price  {:min 0 :max 1}}}})
