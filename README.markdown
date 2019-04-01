# lson-path

`lson-path::lpath` is simple query macro to find something from `lson` (which means a `cons cell` composed with `list`, `vector`, [jsown-object](https://github.com/madnificent/jsown)), `hash-table`.

```
;; lson example 0
`(:obj ("a" . 1)
       ("b" . (10 20 30)))

;; lson example 1
(jsown:parse "{\"a\": 1, \"b\": [10, 20, 30]}")
```


This supports nondeterministic search (like wildcard, repetition, alternation) by using a continuation(CPS) base backtracking.

This would be useful when handling parsed-json-object.


For example, you can get "version" from json-string with below snippet.

```
(lpath (jsown:parse json)
       (:* :.) "version")
```

## Usage

```
(use-package :lson-path)
(defparameter
    target
    '(:obj
       ("a" .
         (:obj
           ("b" . "c")
           ("x" .
             #(1 
              (:obj ("A" . :a))
              3))))))
                 
(lpath target "a")
;; '(:obj ("b"...
(lpath target (:* :{.}) "b")
;; "c", <"c"'s parent>
(lpath target "a" (:* :{.}) 1 "A")
;; :a, <parent>
(lpath target (:* :{.}) (lambda (x) (when (vectorp x) x)))
;; #(1 ...
(lpath target (:* :.) "A")
;; :a, <parent>
(setf (lpath target "a" "b") 100)
;; 100
;; target would be modified.
```

## Query

### `string`

Extract value from lson-obj(jsown object) which key is `string`.

### `number`

Extract value from lson-seq(list) which index is `number`.

### `:.`

Wildcard. traverse lson-obj and lson-seq.

### `:{.}`

Wildcard. traverse only lson-obj.

### `(:{} start end x)`

Repeat x lazily(not greedy) from `start` times upto `end` times.

`end == nil` means end is infinite.


### `(:* x)`, `(:+ x)` , `(:? x)`

Syntax sugar or `:{}`.

- `:*` == `:{} 0 nil`
- `:+` == `:{} 1 nil`
- `:?` == `:{} 0 1`

### `(:seq x y z,,,)`

Sequentially match x y z,,,.

### `(:or x y,,,)`

Alter match.

### `(:regex string)`

Extract values from lson-obj which keys are matched with `(ppcre:create-scanner string)`.

### `(lambda (x xp) (values next-x0 next-x1,,,))`

with function object, you can expand from x to next-x0, next-x1,,,.

example:

```
;; traverse if x is object and key is "a" or "b".
(lambda (x xp)
  (declare (ignorable xp)) ;; xp is x's parent.
  (and (lson-obj-p x)
       (values (lson-obj-ref x "a")
               (lson-obj-ref x "b"))))
```

## Bugs

- Not optimized speed yet.

- `lpath-all` is not yet. Need-p?

- `(setf (lpath x path0 path1,,, path-last) new-value)` has limitations. path-last must be `string` or `integer`.

- Not support circulated structure like `#1=(1 2 3 #1#)`. I think it is OK to handle parsed json.

## Author

* fgatherlet (fgatherlet@gmail.com)

## Copyright

Copyright (c) 2019 fgatherlet (fgatherlet@gmail.com)

## License

Licensed under the MIT License.
