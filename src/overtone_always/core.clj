(comment "repls")
(dub-base-i :note 25)
(dub-base-ii :note 25)
(kill dub-base-i)
(kill dub-base-ii)
(ctl dub-base-ii :note 38)
(demo 10 (wobble-saw))
(demo 10 (wobble (sin-osc 220) 1))
(demo 10 (wobble (detuned-saw 40) 1))
(demo (detuned-saw 40))
(odoc midicps)

(ns overtone-always.core 
    (:use [overtone.live]))

;a ugen is just a signal processing block they can be connected together. an oscillator is implemented as a ugen but it doesn't have to be an oscillator, it can be a filter. for example a cgen is apparently a macro that creates a set of connected ugens in one step, but I've never used it. ugens are kind of classic in computer music software (like a signal processing box in Pd, or ugens in csound. ugen is short for unit generator.

(defcgen detuned-saw
  "A detuned saw wave."
  [freq {:default 40 :expands? true}]
  (:ar (apply + (saw (* [0.99 1.01] freq)))))
; this is a macro to define a cgen, some sort of oscillator
; apply is, applying a funciton, add, to a list of items(vector)
; the list of items are saw waves, which are formed of two channels
; where the frequency of first is slightly lower than second, 0.99:1.01.
; (:ar x) means the audio rate of this cgen is x. in this case, the summation of two saw waves

(defcgen wobble
  "wobble the the input"
  [in   {:doc "input source to wobble"
         :default 0}
   freq {:doc "wobble frequency"
         :default 1}]
  (:ar (let [sweep (lf-tri freq)
             sweep (lin-exp sweep -1 1 40 3000)]
         (lpf in sweep))))
; the catch is the in argument has to be another cgen/oscillator
; lf-tri: a non-band-limited triangle oscillator 
; lin-exp: Convert from a linear range to an exponential range
;         [in 0.0, srclo 0.0, srchi 1.0, dstlo 1.0, dsthi 2.0]
;         in    - Input to convert 
;         srclo - Lower limit of input range 
;         srchi - Upper limit of input range 
;         dstlo - Lower limit of output range 
;         dsthi - Upper limit of output range 
; so the final sweep is a wwwwww shape wave sweeping between 40 and 3000
; lpf: a low pass filter which removes frequencies above a defined cut-off point
; the final result is the lower part of the wwwwww shape wave, whatever the input is




(defcgen wobble-saw
  "Generate a wobbly, detuned saw wave!"
  [freq     {:doc "Main frequency"
             :default 40}
   wob-freq {:doc "Wobble frequency"
             :default 0.5}]
  (:ar (-> (detuned-saw freq)
           (wobble wob-freq)
           normalizer)))
; the threading macro means (normalizer (wobble (detuned-saw freq) wob-freq))
; normalizer: normals the signal (wobble (detuned-saw freq) wob-freq) to 1.0 and -1.0 amplitutde

(definst dub-base-i [out-bus 0 bpm 140 wobble 6 note 30 v 2]
 (let [trig (impulse:kr (/ bpm 140))
       freq (midicps note)
       swr (demand trig 0 (dseq [wobble] INF))
       sweep (lin-exp (lf-tri swr) -1 1 40 3000)
       wob (apply + (wobble-saw (* freq [0.99 1 1.01])))
       wob (lpf wob sweep)
       wob (* 0.9 (normalizer wob))
       wob (+ wob (bpf wob 1500 2))
       wob (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))]
   (* v (clip2 (+ wob) 1))))

(definst dub-base-ii [out-bus 0 bpm 140 wobble 3 note 40  v 2]
 (let [trig (impulse:kr (/ bpm 140))
       freq (midicps note)
       swr (demand trig 0 (dseq [wobble] INF))
       sweep (lin-exp (lf-tri swr) -1 1 40 3000)
       wob (apply + (saw (* freq [0.99 1.01])))
       wob (lpf wob sweep)
       wob (* 0.9 (normalizer wob))
       wob (+ wob (bpf wob 1500 2))
       wob (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))]
   (* v (clip2 (+ wob) 1))))

(defn ugen-cents
  "Returns a frequency computed by adding n-cents to freq.  A cent is a
  logarithmic measurement of pitch, where 1-octave equals 1200 cents."
  [freq n-cents]
  (with-overloaded-ugens
    (* freq (pow 2 (/ n-cents 1200)))))

(definst rise-fall-pad [freq 440 split -5 t 4]
  (let [f-env (env-gen (perc t t) 1 1 0 1 FREE)]
    (rlpf (* 0.3 (saw [freq (ugen-cents freq split)]))
          (+ (* 0.6 freq) (* f-env 2 freq)) 0.2)))

(definst resonant-pad [freq 440 split -5 t 4 lfo 0.5 depth 10]
  (let [f-env (env-gen (perc t t) 1 1 0 1 FREE)
        lfo (* depth (sin-osc:kr lfo))]
    (rlpf (* 0.3 (+ (square freq) (lf-tri (+ lfo (ugen-cents freq split)))))
          (+ (* 0.8 freq) (* f-env 2 freq)) 3/4)))

(defsynth plop [freq 440 len 0.4]
  (* 0.4 (env-gen (perc 0.02 len) 1 1 0 1 FREE)
     (sin-osc [(+ (* 3 (sin-osc 20)) freq) (/ freq 2)])))

(stop)
