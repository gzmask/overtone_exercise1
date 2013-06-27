(ns overtone-always.core 
    (:require [leipzig.melody :as lm]
              [leipzig.scale :as ls]
              [leipzig.canon :as lc]
              [leipzig.live :as ll]
              [overtone.live :as overtone]
              [overtone.synth.stringed :as strings]))

;(overtone/boot-external-server)     ;boot an external server

;sound synthesis. wave functions takes amplitude and frequency and outputs amplitude
;thus these functions are manapulating with the outputs, with are amplitudes.
(overtone/definst sin-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (overtone/env-gen (overtone/lin-env attack sustain release) 1 1 0 1 overtone/FREE)
     (+ (overtone/sin-osc freq)
        (overtone/sin-osc (* freq 4)))
     vol))

(strings/gen-stringed-synth ektara 1 (true))

(overtone/odoc strings/gen-stringed-synth)
(overtone/odoc lm/phrase)
(overtone/odoc lm/where)

(defn pick [distort amp {midi :pitch, start :time, length :duration}]
  (let [synth-id (overtone/at start
                              (ektara midi :distort distort :amp amp :gate 1))]
    (overtone/at (+ start length) (overtone/ctl synth-id :gate 0))))

;polyrism kinda functions
(defmethod ll/play-note :leader [note]
  (pick 0.7 1.0 note))
(defmethod ll/play-note :follower [note]
  (pick 0.3 1.0 note))
(defmethod ll/play-note :bass [note]
  (pick 0.9 0.2 (update-in note [:pitch] #(- % 12))))

(def melody "A simple melody built from durations and pitches."
               ; Row, row, row your boat,
  (->> (lm/phrase [3/3 3/3 2/3 1/3 3/3]
               [  0   0   0   1   2])
    (lm/then
               ; Gently down the stream,
       (lm/phrase [2/3 1/3 2/3 1/3 6/3]
               [  2   1   2   3   4]))
    (lm/then
               ; Merrily, merrily, merrily, merrily,
       (lm/phrase (repeat 12 1/3) 
               (mapcat (partial repeat 3) [7 4 2 0])))
    (lm/then
               ; Life is but a dream!
       (lm/phrase [2/3 1/3 2/3 1/3 6/3] 
               [  4   3   2   1   0]))
    (lm/where :part (lm/is :leader))))

(def bass "A bass part to accompany the melody."
  (->> (lm/phrase [1  1 2]
               [0 -3 0])
     (lm/where :part (lm/is :bass))
     (lm/times 4)))

(defn row-row
  "Play the tune 'Row, row, row your boat' as a round."
  [speed key]
  (->> melody
    (lm/with bass)
    (lm/times 2)
    (lc/canon (comp (lc/simple 4)
                 (partial lm/where :part (lm/is :follower))))
    (lm/where :time speed)
    (lm/where :duration speed)
    (lm/where :pitch key)
    ll/play))

(doc ls/minor)

(row-row (lm/bpm 120) (comp ls/C ls/sharp ls/major))

(row-row (lm/bpm 90) (comp ls/low ls/B ls/flat ls/minor))

(overtone/stop)
