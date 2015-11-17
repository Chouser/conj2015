(ns conj2015.error-handler
  (:require [clojure.repl :refer :all]))

    (def swiper (atom "swiper"))

    (def dora (agent "dora"
                :error-handler
                (fn [a e]
                  (reset! swiper "no swiping"))))

    (send dora #(str % " and boots"))
    @dora   ;=> "dora and boots"
    @swiper ;=> "swiper"

    (send dora dec)
    @swiper ;=> "no swiping"

