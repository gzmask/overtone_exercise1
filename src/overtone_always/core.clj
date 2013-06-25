(ns overtone-always.core 
    (:use [overtone.core]))

(boot-external-server)    ; boot an external server

; learning my basic kicks
(def kick (sample (freesound-path 2086)))

(def one-twenty-bpm (metronome 120))


(defn looper [nome sound]    
    (let [beat (nome)]
        (at (nome beat) (sound))
        (apply-at (nome (inc beat)) looper nome sound [])))

(looper one-twenty-bpm kick)

;sound synthesis. wave functions takes amplitude and frequency and outputs amplitude
;thus these functions are manapulating with the outputs, with are amplitudes.
(definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     (saw (- freq 220))
     vol))

(definst sin-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (sin-osc freq)
     (sin-osc (* freq 3))
     vol))
(demo (sin-wave))

(definst square-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (lf-pulse freq)
    vol))

(definst noisey [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (pink-noise) ; also have (white-noise) and others...
     vol))

(definst triangle-wave [freq 440 attack 0.01 sustain 0.1 release 0.4 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (lf-tri freq)
     vol))

(definst spooky-house [freq 440 width 0.2 
                         attack 0.3 sustain 4 release 0.3 
                         vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (sin-osc (+ freq (* 20 (lf-pulse:kr 0.5 0 width))))
     vol))

;(saw-wave 440)
;(saw-wave 523.25)
;(saw-wave 261.62) ; This is C4
;(saw-wave (midi->hz 69))
;(saw-wave (midi->hz 72))
;(saw-wave (midi->hz 60)) ; This is C4
;(saw-wave (midi->hz (note :A4)))
;(saw-wave (midi->hz (note :C5)))
;(saw-wave (midi->hz (note :C4))) ; This is C4! Surprised?

(defn note->hz [music-note]
    (midi->hz (note music-note)))


(defn wave [music-note]
    (sin-wave (midi->hz (note music-note))))

(defn a []
  (do 
    (wave :A4)
    (wave :C5)
    (wave :C4)))

(defn play-chord [a-chord]
  (doseq [note a-chord] (wave note)))

;play-chord with delay
(defn play-chord2 [a-chord time]
      (doseq [[note timestamp] (map list a-chord [time (+ time 100) (+ time 200)])]
             (at timestamp (wave note))))

(chord :c4 :major)
(chord :c4 :minor)
(play-chord2 (chord :c4 :major) (now))
(play-chord (chord :c4 :minor))

(defn chord-progression-time []
  (let [time (now)]
    (at time (play-chord (chord :C4 :major)))
    (at (+ 1000 time) (play-chord (chord :G3 :major)))
    (at (+ 2000 time) (play-chord (chord :F3 :sus4)))
    (at (+ 3300 time) (play-chord (chord :F3 :major)))
    (at (+ 4000 time) (play-chord (chord :G3 :major)))))

(def metro (metronome 200))
(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major)))
  (at (m (+ 4 beat-num)) (play-chord (chord :G3 :major)))
  (at (m (+ 8 beat-num)) (play-chord (chord :A3 :minor)))
  (at (m (+ 12 beat-num)) (play-chord (chord :F3 :major)))  
)

(defn play-scale [beats notes metro beat-num]
  (doseq [[beat note] (map list beats notes)]
    (at (metro (+ beat beat-num)) (play-chord2 (chord note :major) (metro (+ beat beat-num)))))
)
(play-scale [0   2   3   4   6   7   8   10   11 12  14  15  16   18   19  20  22  24  26  28]
            [:c4 :g3 :a3 :b3 :a3 :g3 :e3 :e3 :g3 :b3 :a3 :g3 :f#3 :f#3 :g3 :a3 :b3 :g3 :e3 :e3]
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
(looper one-twenty-bpm kick)
(stop)
