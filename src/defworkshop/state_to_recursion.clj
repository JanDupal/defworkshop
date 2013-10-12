(ns defworkshop.state-to-recursion
  (:require [workshoplib.tdd :refer […]]))

;; More often than not, state can be avoided by using recursion. Let's see how it works

(defn reverse-coll
  "Reverse the collection `coll`.

   You can use `loop/recur` construct to loop over the sequence.
   `cons` prepends items to the list, try that out."
  [coll]
  (loop [c coll res []]
    (if (empty? c)
      res
      (recur (rest c) (cons (first c) res)))))

(defn recursive-sum
  "We've already implemented sum using reduce, now let's move to implementing it via recursion!"
  [[head & tail]]
  (if (empty? tail)
      (or head 0)
      (+ head (recursive-sum tail))))

(defn recursive-sum-tc
  "with a tail-recursive version of sum, we can avoid stack overflows."
  ([coll]
    (recursive-sum-tc coll 0))
  ([[head & tail] acc]
    (if (empty? tail)
        (+ head acc)
        (recursive-sum-tc tail (+ head acc)))))

(defn max-from-list
  "Get the maximum from list using recursion"
  [[head & tail]]
  (if (empty? tail)
    head
    (let [max (max-from-list tail)]
         (if (> head max) head max))))

(defn my-reduce
  "generalizing the recursive sum example, write your own implementation of reduce! (for empty coll, just return nil.)"
  ([f [head & tail]]
     (my-reduce f head tail))
  ([f acc-init coll]
    (if (empty? coll)
        acc-init
        (my-reduce f (f acc-init (first coll)) (rest coll)))))

(defn max-from-list-tc
  "Get the maximum from list using tail recursion (avoid stack overflow)"
  ([coll]
     (if (empty? coll)
         nil
         (max-from-list-tc coll 0)))
  ([[head & tail] m]
     (if (empty? tail)
         (max m head)
         (max-from-list-tc tail (max m head)))))

(defn loop-recur-sum
  "This implementation is somewhat easier to understand for people coming from imperative style."
  [numbers]
  (loop [coll numbers s 0]
    (if (empty? coll)
        s
        (recur (rest coll) (+ s (first coll))))))

(defn map-from-keys-and-vals
  "Something that we've already implemented previously in terms of `zipmap`, but are going to implement again
   in terms of recursion. Usually you use `loop/recur` construct whenever you have a one or multiple accumulators
   or several collections you iterate over."
  [keys vals]
  (loop [ks keys vs vals m {}]
    (if (or (empty? ks) (empty? vs))
        m
        (recur (rest ks)
               (rest vs)
               (assoc m (first ks) (first vs))))))

(defn parentheses-balanced?
  "Check wether the given string has balanced parantheses or no.

   You can use `cond` statement to avoid deeply nested ints.
   It's a recursive problem, so you'll have to build up stack to solve it.

   `inc` increments a number, `dec` decrements a number."
  ([str] (parentheses-balanced? str 0))
  ([[current & tail] count]
    (cond
      (nil? current) (= count 0)
      (= current \() (parentheses-balanced? tail (inc count))
      (= current \)) (parentheses-balanced? tail (dec count))
      :else (parentheses-balanced? tail count))))
