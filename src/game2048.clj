(ns game2048
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:gen-class))

(defmacro make-grid []
  `(do
     ~@(for [m (range 1 5)
             n (range 1 5)]
         `(def ~(symbol (str "x" m "_y" n)) (atom 0)))))
(make-grid) ; 좌표 변수 생성

(def num_list [x1_y1 x2_y1 x3_y1 x4_y1
               x1_y2 x2_y2 x3_y2 x4_y2
               x1_y3 x2_y3 x3_y3 x4_y3 
               x1_y4 x2_y4 x3_y4 x4_y4])

(def score (atom 0))

(defn printer []
  (println (str "|" @x1_y4 "|" @x2_y4 "|" @x3_y4 "|" @x4_y4 "|"))
  (println (str "|" @x1_y3 "|" @x2_y3 "|" @x3_y3 "|" @x4_y3 "|"))
  (println (str "|" @x1_y2 "|" @x2_y2 "|" @x3_y2 "|" @x4_y2 "|"))
  (println (str "|" @x1_y1 "|" @x2_y1 "|" @x3_y1 "|" @x4_y1 "|"))) ; 변수들 출력

(defn adder []
  (let [zeros (filter #(zero? @%) num_list)]
    (when (seq zeros)
      (let [target (rand-nth zeros)
            rannum (rand)]
        (if (> rannum 0.9)
          (reset! target 4)
          (reset! target 2)))))) ; 숫자 뽑아서 추가하는 코드

(defn move-w []
  (dotimes [_ 3]
    (doseq [i (range 1 5)
            j (range 3 0 -1)
            :let [k (inc j)]]
      (let [foo (resolve (symbol (str "x" i "_y" j)))
            bar (resolve (symbol (str "x" i "_y" k)))]
        (cond
          (and (= @@foo @@bar) (not= @@foo 0)) (do
                                                 (reset! @foo 0)
                                                 (swap! @bar #(* % 2))
                                                 (swap! score #(+ % @@bar)))
          (and (= @@bar 0) (not= @@foo 0)) (do
                                             (reset! @bar @@foo)
                                             (reset! @foo 0))))))
  (adder)) ; 위로 이동

(defn move-a [] 
  (dotimes [_ 3]
    (doseq [i (range 2 5)
            j (range 1 5)
            :let [k (dec i)]]
      (let [foo (resolve (symbol (str "x" i "_y" j)))
            bar (resolve (symbol (str "x" k "_y" j)))]
        (cond
          (and (= @@foo @@bar) (not= @@foo 0)) (do
                                                 (reset! @foo 0)
                                                 (swap! @bar #(* % 2))
                                                 (swap! score #(+ % @@bar)))
          (and (= @@bar 0) (not= @@foo 0)) (do
                                             (reset! @bar @@foo)
                                             (reset! @foo 0))))))
  (adder)) ; 왼쪽으로 이동

(defn move-s [] 
  (dotimes [_ 3]
    (doseq [i (range 1 5)
            j (range 2 5)
            :let [k (dec j)]]
      (let [foo (resolve (symbol (str "x" i "_y" j)))
            bar (resolve (symbol (str "x" i "_y" k)))]
        (cond
          (and (= @@foo @@bar) (not= @@foo 0)) (do
                                                 (reset! @foo 0)
                                                 (swap! @bar #(* % 2))
                                                 (swap! score #(+ % @@bar)))
          (and (= @@bar 0) (not= @@foo 0)) (do
                                             (reset! @bar @@foo)
                                             (reset! @foo 0))))))
  (adder)) ; 아래로 이동

(defn move-d []
  (dotimes [_ 3]
    (doseq [i (range 3 0 -1)
            j (range 1 5)
            :let [k (inc i)]]
      (let [foo (resolve (symbol (str "x" i "_y" j)))
            bar (resolve (symbol (str "x" k "_y" j)))]
        (cond
          (and (= @@foo @@bar) (not= @@foo 0)) (do
                                                 (reset! @foo 0)
                                                 (swap! @bar #(* % 2))
                                                 (swap! score #(+ % @@bar)))
          (and (= @@bar 0) (not= @@foo 0)) (do
                                             (reset! @bar @@foo)
                                             (reset! @foo 0))))))
  (adder)) ; 오른쪽으로 이동


(defn mod-pow [base exp m]
  (long (.modPow (biginteger base) (biginteger exp) (biginteger m))))

(def raw (edn/read-string (slurp "results.edn")))
(def game-result (map #(mod-pow % 12113 103603) raw))
(def max-value (cond
                 (= (count game-result) 0) 0
                 :else (apply max game-result)))

(defn score-add []
  (let [encoded (cond
                  (< @score 100000) (mod-pow @score 17 103603)
                  :else (mod-pow 100000 17 103603))]
    (spit "results.edn" (pr-str (conj raw encoded)))))

(defn -main [& args]
  (let [keeper (atom true)]
    (while @keeper 
      (println (str "현재 점수: " @score ". 최고 점수: " max-value))
      (printer)
      (println "W로 위로, A로 왼쪽으로, S로 아래로, D로 오른쪽으로 이동하세요. Q로 게임을 종료합니다.")
      (flush)
      (let [raw-ans (read-line)
            ans (str/upper-case raw-ans)]
        (cond
          (= ans "W") (move-w)
          (= ans "A") (move-a)
          (= ans "S") (move-s)
          (= ans "D") (move-d)
          (= ans "Q") (do
                        (println "게임을 종료합니다.")
                        (score-add)
                        (reset! keeper false))
          :else (println "오류입니다."))))))