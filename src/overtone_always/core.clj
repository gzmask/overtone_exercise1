(ns overtone-always.core 
    (:use [overtone.core]))

(boot-external-server)    ; boot an external server

;sound synthesis. wave functions takes amplitude and frequency and outputs amplitude
;thus these functions are manapulating with the outputs, with are amplitudes.
(definst sin-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (+ (sin-osc freq)
        (sin-osc (* freq 4)))
     vol))

(demo (sin-wave))
; here, how I can read the fucking source code!
(meta #'overtone.core/sin-osc)
(meta #'overtone.core/saw)

(defn wave [music-note]
    (sin-wave (midi->hz (note music-note))))

(defn play-chord [a-chord]
  (doseq [note a-chord] (wave note)))
(odoc meta)

;play-chord with delay
(defn play-chord2 [a-chord time]
      (doseq [[note timestamp] (map list a-chord [time (+ time 100) (+ time 200)])]
             (at timestamp (wave note))))
(map list (chord :c4 :major) [(now) (+ (now) 100) (+ (now) 200)])
(chord :c4 :major)
(chord :c4 :minor)
(play-chord2 (chord :c4 :major) (now))
(play-chord (chord :c4 :minor))

(def metro (metronome 200))

(defn play-scale [beats notes metro beat-num]
  (doseq [[beat note] (map list beats notes)]
    (at (metro (+ beat beat-num)) (play-chord2 (chord note :major) (metro (+ beat beat-num))))))

(play-scale [0    1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17   18   19  20  21  22  23  24  25  26  27  28  29]
            [:c4 :c0 :g3 :a3 :b3 :b0 :a3 :g3 :g3 :e3 :e3 :g3 :g3 :b3 :a3 :g3 :g3 :f#3 :f#3 :g3 :g3 :a3 :a3 :b3 :b3 :g3 :g3 :e3 :e3 :e3]
            metro
            (metro))

(defn play-scale1 [beats notes metro beat-num]
    (doseq [[beat note] (map list (reductions + beats) notes)]
      (let [time (metro (+ beat beat-num))] 
        (at time (play-chord2 (chord note :major) time)))))

(play-scale1 [0   2   1    1   2   1   1   2   1   1   2   1    1    2    1  1   2   2   2   2]
             [:c4 :g3 :a3 :b3 :a3 :g3 :e3 :e3 :g3 :b3 :a3 :g3 :f#3 :f#3 :g3 :a3 :b3 :g3 :e3 :e3]
             metro
             (metro))

(odoc let)
(= [[1 1] [2 2] [3 3]] (map list [1 2 3] [1 2 3]))

(= '(1 2 3) [1 2 3])

(defn play-scale2 [beat-notes metro beat-num]
  (doseq [[beat note] beat-notes]
    (at (metro (+ beat beat-num)) (play-chord (chord note :major))))
)
(play-scale2 [[0 :c4] [2 :g3] [3 :a3] [4 :b3] [6 :a3] [7 :g3]]
             metro
             (metro))

(defn play-scale3 [beat-notes metro beat-num]
  (doseq [[beat note] (partition 2 beat-notes)]
    (at (metro (+ beat beat-num)) (play-chord2 (chord note :major) (metro (+ beat beat-num)))))
)
(play-scale3 [0 :c4 2 :g3 3 :a3 4 :b3 6 :a3 7 :g3 8 :e3 10 :e3 11 :g3 12 :b3 14 :a3 15 :g3 16 :f#3 18 :f#3 19 :g3 20 :a3 22 :b3 24 :g3 26 :e3 28 :e3]
             metro
             (metro))

  

(defn ray-beat [m b]
    (at (m (+ 0 b)) (play-chord (chord :c4 :major)))
    (at (m (+ 2 b)) (play-chord (chord :g3 :major)))
    (at (m (+ 3 b)) (play-chord (chord :a3 :major)))
    (at (m (+ 4 b)) (play-chord (chord :b3 :major)))
    (at (m (+ 6 b)) (play-chord (chord :a3 :major)))
    (at (m (+ 7 b)) (play-chord (chord :g3 :major)))
    (at (m (+ 8 b)) (play-chord (chord :e3 :major)))
    (at (m (+ 10 b)) (play-chord (chord :e3 :major)))
    (at (m (+ 11 b)) (play-chord (chord :g3 :major)))
    (at (m (+ 12 b)) (play-chord (chord :b3 :major)))
    (at (m (+ 14 b)) (play-chord (chord :a3 :major)))
    (at (m (+ 15 b)) (play-chord (chord :g3 :major)))
    (at (m (+ 16 b)) (play-chord (chord :f#3 :major)))
    (at (m (+ 18 b)) (play-chord (chord :f#3 :major)))
    (at (m (+ 19 b)) (play-chord (chord :g3 :major)))
    (at (m (+ 20 b)) (play-chord (chord :a3 :major)))
    (at (m (+ 22 b)) (play-chord (chord :b3 :major)))
    (at (m (+ 24 b)) (play-chord (chord :g3 :major)))
    (at (m (+ 26 b)) (play-chord (chord :e3 :major)))
    (at (m (+ 28 b)) (play-chord (chord :e3 :major)))
    (apply-at (m (+ 32 b)) ray-beat m (+ 32 b) []))
(ray-beat metro (metro))
(stop)
