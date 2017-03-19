(ns rhythmrail.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))            

(use 'overtone.live)

(def reol-no-title-notes [2514
                          3064
                          3485
                          3616
                          3784
                          4193
                          4527
                          4844
                          4977
                          5378
                          5695
                          6043
                          6176
                          6309
                          6593
                          6893
                          7411
                          7845
                          8256
                          8408
                          8574
                          8976
                          9307
                          9624
                          9791
                          10207
                          10508
                          10841
                          10990
                          11124
                          11408
                          11747])

(defn setup []
  ; Set frame rate to 120 frames per second.
  (q/frame-rate 120)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (q/text-size 25)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0
   :notes reol-no-title-notes})

(declare find-best-hit)

(def empty-note {:distance/absolute 0 :distance/milliseconds 0 :note 0})

(defn update-state [state]
  ; Update sketch state by changing circle color and position
  (let [now (if (:playing/started-at state)
              (- (System/currentTimeMillis)
                 (:playing/started-at state))
              0)
        prev (if (:playing/started-at state)
               (find-best-hit
                 (:notes state) 
                 now
                 false)
               empty-note)
        next (if (:playing/started-at state)
               (find-best-hit
                 (:notes state) 
                 now)
               empty-note)
        distance (- (:note prev) 
                    (:note next))
        current-relative (- (:note prev)
                            now)]
    (-> state
        (assoc :color (mod (+ (:color state) 0.7) 255))
        (assoc :angle (+ (:angle state) 1))
        (assoc :nearest-past prev)
        (assoc :nearest-next next)
        (assoc :next-note-incoming 
          (if (not (= current-relative 0)) 
            (/ distance current-relative)
            0)))))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  (q/fill (:color state) 255 255)
  ; Calculate x and y coordinates of the circle.
  (let [angle (:angle state)
        x 0
        y 0]
    ; Move origin point to the center of the sketch.
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      ; Draw the circle.
      (q/ellipse x y 300 300)
      (q/text 
        (or (when (not (nil? (:latest-note-score state)))
              (str (:latest-note-score state) " - Total score: " (:score state))) 
            "Start the music with F and \npress Z or X to hit the notes!") 
        (- x 160) (- y 230))
      (q/text (str (:distance/milliseconds (:nearest-past state))) (- x 30) (+ y 200))
      (q/text (str (:distance/milliseconds (:nearest-next state))) (+ x 30) (+ y 200))
      (q/fill (- 255 (:color state)) 255 200)
      (q/ellipse x y
                   (if (:playing/started-at state)
                     (* (:next-note-incoming state) 2)
                     (* (/ (:color state) 255) 300))  
                   (if (:playing/started-at state)
                     (* (:next-note-incoming state) 2)
                     (* (/ (:color state) 255) 300))))))
                 

(defn analyze-note [hit]
  (fn [note]
    (let [distance (- note hit)]
      {:distance/milliseconds distance
       :distance/absolute (Math/abs distance)
       :note note})))

(defn find-best-hit
  ([notes keyboard-hit] (find-best-hit notes keyboard-hit true))
  ([notes keyboard-hit next]
   (let [analyzed-hits (map (analyze-note
                              keyboard-hit)
                            notes)
         future-or-past-hits (if next
                               (filter #(neg? (:distance/milliseconds %)) analyzed-hits)
                               (filter #(pos? (:distance/milliseconds %)) analyzed-hits))]
     (if (> (count future-or-past-hits)
            0)
       (apply min-key :distance/absolute future-or-past-hits)
       {:distance/milliseconds 0 :distance/absolute 0 :note 0}))))

(defn rate-hit [hit]
  (let [abs-hit (:distance/absolute hit)]
    (cond
      (< abs-hit 50) 300
      (< abs-hit 100) 100
      (< abs-hit 300) 50
      :else 0)))

(defn on-hit-note [state]
  (let [hit-time (- (System/currentTimeMillis)
                    (or (:playing/started-at state)
                        (System/currentTimeMillis)))
        hit (rate-hit
              (find-best-hit 
                (:notes state)
                hit-time))]
    (-> state
        (assoc :latest-note-score 
          hit)
        (update :score (fn [score] (+ (or score 0) hit))))))

(defn key-pressed [state {:keys [key] :as event}]
  (case key
    :f (-> state
           (assoc :playing/buffer (stereo-partial-player
                                      (load-sample "~/Reol_-_No_Title.wav")))
           (assoc :playing/started-at (System/currentTimeMillis)))       
    :s (let []
        (stop)
        (-> state
            (dissoc :playing/started-at)
            (dissoc :latest-note-score)
            (dissoc :score)))
    :x (on-hit-note state)
    :z (on-hit-note state)
    state))

(q/defsketch rhythmrail
  :title "ok"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :key-pressed key-pressed
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

