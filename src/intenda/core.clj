(ns intenda.core
  (:gen-class))

(defn calc-probability [n f]
  (double (/ (reduce + (take n (repeatedly f)))
             n)))

(defn game-show [change-doors]
  (let [winning-door (rand-int 3)
        initial-guess (rand-int 3)
        a-door-with-goat (first (disj #{0 1 2} initial-guess winning-door))
        other-door (first (disj #{0 1 2} a-door-with-goat initial-guess))]
    (if (= winning-door
           (if change-doors
             other-door
             initial-guess))
      1 
      0)))

(defn game-show-main []
  (let [n 100000
        change-doors-results (calc-probability n #(game-show true))
        dont-change-doors-results (calc-probability n #(game-show false))]
    (println "Game show problem:")
    (println "Probability of winning with a strategy of changing doors = " change-doors-results)
    (println "Probability of winning with a strategy of not changing doors = " dont-change-doors-results)))


(defn prisoners-random-pick [drawers my-number]
  (some #(= my-number %)
        (take 50 (shuffle drawers))))

(defn prisoners-strategic-pick [drawers my-number]
  (loop [drawer-to-try my-number
         i 0]
    (if (= i 50)
      false
      (let [next-number (nth drawers drawer-to-try)]
        (if (= my-number next-number)
          true
          (recur next-number (inc i)))))))

(defn prisoners-problem [pick-fn]
  (let [drawers (shuffle (range 100))
        fifty-prisoners (take 50 (shuffle (range 100)))
        result (->> fifty-prisoners
                    (map #(pick-fn drawers %))
                    (every? identity))]
    (if result
      1
      0)))

(defn prisoners-main []
  (let [n 100000
        random-result (calc-probability n #(prisoners-problem prisoners-random-pick))
        strategic-result (calc-probability n #(prisoners-problem prisoners-strategic-pick))]
    (println "100 Prisoners problem:")
    (println "Probability of prisoners being released with random picking = " random-result)
    (println "Probability of prisoners being releaded with strategic pricking = " strategic-result)))

(defn -main []
  (game-show-main)
  (prisoners-main))