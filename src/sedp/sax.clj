(ns sedp.sax)

(defprotocol Handler
  (start-document [this])
  (end-document [this])
  (start-element [this tag attrs])
  (end-element [this tag])
  (characters [this text]))

(defrecord SimpleHandler [result stack]
  Handler
  (start-document [this]
    (assoc this :result nil :stack []))

  (end-document [this]
    (:result this))

  (start-element [this tag attrs]
    (update this :stack conj [(keyword tag) attrs]))

  (characters [this text]
    (if (seq (:stack this))
      (update-in this [:stack (dec (count (:stack this)))] conj text)
      this))

  (end-element [this tag]
    (let [stack (:stack this)
          current (peek stack)
          stack' (pop stack)]
      (if (seq stack')
        (let [parent (peek stack')
              updated-parent (conj parent current)]
          (assoc this :stack (conj (pop stack') updated-parent)))
        (assoc this :result current :stack [])))))

(defn parse [s handler]
  ;; ВАЖНО: это не настоящий streaming SAX — read-string читает всё целиком.
  (let [data (read-string s)
        handler (start-document handler)]
    (letfn [(process [node h]
              (if (vector? node)
                (let [[tag & rest] node
                      attrs (when (map? (first rest)) (first rest))
                      content (if (map? (first rest)) (rest rest) rest)
                      h (start-element h tag (or attrs {}))
                      h (reduce
                          (fn [acc item]
                            (if (vector? item)
                              (process item acc)
                              (characters acc (str item))))
                          h
                          content)]
                  (end-element h tag))
                h))]
      (end-document (process data handler)))))
