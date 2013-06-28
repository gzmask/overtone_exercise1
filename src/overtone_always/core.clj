(comment "repls"
(metro-bpm metro 100);lol not a good idea to set it higher than 10000
(play-hat (metro))
(play-kick (metro))
(metro)
)

(ns overtone-always.core 
    (:use [overtone.live]))

(definst tone [note 60 amp 0.6 dur 0.4]
  (let [snd (sin-osc (midicps note))
        env (env-gen (perc 0.01 dur) :action FREE)]
    (* env snd amp)))

(definst kick [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(def metro (metronome 120))

(defn play-hat [beat]
  (at (metro (+ 0.25 beat)) (c-hat))
  (apply-at (metro (+ 1 beat)) #'play-hat (+ 1 beat) []))

(defn play-kick [beat]
  (at (metro beat) (kick))
  (apply-at (metro (+ 1 beat)) #'play-kick (+ 1 beat) []))

(stop)
