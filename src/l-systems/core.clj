(ns l-systems.core
  (:require [quil.core :refer :all]
            [data.grid2d :as grid]
            [quil.middleware :as m]))

(defn apply-rules [grammar sentence]
  (apply str (replace (:rules grammar) sentence)))

(defn l-system [grammar]
  (iterate #(apply-rules grammar %) (:start grammar)))

(def hilbert
  {:start [\X]
   :rules {\X "-YF+XFX+FY-"
           \Y "+XF-YFY-FX+"}
   :angle 90
   :commands {\F :forward
              \+ :left
              \- :right}})

(def dragon-curve
  {:start [\F \X]
   :rules {\X "X+YF+"
           \Y "-FX-Y"}
   :angle 90
   :commands {\F :forward
              \+ :left
              \- :right}})

(def tree
  {:start [\X]
   :rules {\F "FF"
           \X "F[+X]F[-X]+X"}
   :angle 20
   :commands {\F :forward
              \+ :left
              \- :right
              \[ :push
              \] :pop}})

(def tree2
  {:start [\F]
   :rules {\F "FF+[+F-F-F]-[-F+F+F]"}
   :angle 20
   :commands {\F :forward
              \+ :left
              \- :right
              \[ :push
              \] :pop}})

(def hexagonal-gosper
  {:start [\X \F]
   :rules {\X "X+YF++YF-FX--FXFX-YF+"
           \Y "-FX+YFYF++YF+FX--FX-Y"}
   :angle 60
   :commands {\F :forward
              \+ :left
              \- :right
              \[ :push
              \] :pop}})

;; Gets good around angles 5-10
(defn weird-spider-thing [angle]
  {:start [\F]
   :rules {\F "FF+[F-XF]"
           \X "+FX--FX"}
   :angle angle
   :commands {\F :forward
              \+ :left
              \- :right
              \[ :push
              \] :pop}})

(defn test [angle]
  {:start [\F \F]
   :rules {\F "F+X-F++F-X--F"
           \X "+F--F+F++X-F"}
   :angle angle
   :commands {\F :forward
              \+ :left
              \- :right
              \[ :push
              \] :pop}})

(defn sentence->commands [grammar sentence]
  (let [{:keys [commands]} grammar]
    (remove nil? (map #(get commands %) sentence))))

(defn exec [grammar commands]
  (let [deg (/ PI 180)
        rad (* (:angle grammar) deg)
        step 1]
    (doseq [cmd commands]
      (condp = cmd
        :left
        (rotate (- rad))

        :right
        (rotate rad)

        :push
        (push-matrix)

        :pop
        (pop-matrix)

        :forward
        (do 
          (line 0 0 step 0)
          (translate step 0)))
      )))

(defn setup []
  (let [grammar (test 45)
        steps 7]
    (frame-rate 1)
    (color-mode :hsb)
    
    (background 0)
    (stroke 255)

    (push-matrix)
    
    (translate 400 100)
    (rotate (-  (/ PI 2)))

    (->> 
     (nth (l-system grammar) steps)
     (sentence->commands grammar)
     (exec grammar))

    (pop-matrix)

    (save "generated/weird-spider-thing-11.png")))

(defn draw []
  (no-loop)
  )

(defsketch l-systems
  :title "l-systems"
  :settings
  (fn []
    (smooth 2)
    (pixel-density 2))
  :size [800 1000]
  :setup setup
  :draw draw)
