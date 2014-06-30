(ns quiescent)

(defmacro defcomponent
  "Creates a ReactJS component with the given name, an (optional)
  docstring, an argument vector, (optional) custom component properties
  and a body which will be used as the rendering function to quiescent/component.

  Shorthand for:

  (def name (quiescent/component (fn [value] body) props))"
  [name & forms]
  (let [has-docstr? (string? (first forms))
        has-props? (map? (if has-docstr? (nth forms 2) (second forms)))
        docstr (if has-docstr? (first forms) "")
        argvec (if has-docstr? (second forms) (first forms))
        props (if has-props? (if has-docstr? (nth forms 2) (second forms)) {})
        body (cond
               (and has-docstr? has-props?) (drop 3 forms)
               (or has-docstr? has-props?) (drop 2 forms)
               :else (drop 1 forms))]
    `(def ~name ~docstr (quiescent/component (fn ~argvec ~@body) ~props))))
