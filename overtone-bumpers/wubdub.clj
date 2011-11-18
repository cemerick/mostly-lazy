(ns mostly.lazy.wubdub
  (:use [overtone.live]
        [overtone.helpers.file]
        [clojure.core.match :only [match]]
        [polynome.core :as poly]))

;;Performed with Overtone 0.6-dev

;;Ian Hall's drum pack: http://www.freesound.org/people/ianhall/packs/691/
(def drum-path "~/Desktop/samples/drums")
(def hhat-path (mk-path [drum-path "13246__ianhall__hihat-closed.aif"]))
(def snar-path (mk-path [drum-path  "13253__ianhall__snare.aif"]))
(def spls-path (mk-path [drum-path  "13247__ianhall__hihat-open.aif"]))

;; load sample buffers

(def hhat-bf (load-sample hhat-path))
(def snar-bf (load-sample snar-path))
(def spls-bf (load-sample spls-path))

;;HardPCM's kick drum: http://www.freesound.org/people/HardPCM/sounds/30669/
(def kick-bf (load-sample "~/Desktop/samples/30669__hardpcm__hardkick002.wav"))


;; create a separate control bus for each drum
(def kick-b (control-bus))
(def hhat-b (control-bus))
(def hht2-b (control-bus))
(def snar-b (control-bus))
(def spls-b (control-bus))


;; create an instrument to play each drum sample
(definst kick-i [bus 10 f 10 vol 0]
  (* vol (* 1 (lpf (clip2 (play-buf 1 kick-bf 0.8 (in:kr kick-b)) 0.7) f))))

(definst hhat-i [bus 10 f 10 vol 0]
  (* vol (* 0.5 (lpf (play-buf 1 hhat-bf 1 (in:kr hhat-b)) f))))

(definst hht2-i [bus 10 f 10 vol 0]
  (* vol (* 0.5 (lpf (g-verb (play-buf 1 hhat-bf 0.9 (in:kr hht2-b)) 10  0.4 0.4) f))))

(definst snar-i [bus 10 f 10 vol 0]
  (* vol (* 1.5  (lpf (g-verb (play-buf 1 snar-bf 0.9 (in:kr snar-b)) 100 3 0.3) f))))

(definst spls-i [bus 10 f 10 vol 0]
  (* vol (*   0.5 (lpf (g-verb (play-buf 1 spls-bf 0.8 (in:kr spls-b)) 200 0.3 0.3) f))))

(definst dubstep [note 30 wob 0.5 hi-man 0 lo-man 0 sweep-man 0 deci-man 0 tan-man 0 shape 0 sweep-max-freq 3000 hi-man-max 1000 lo-man-max 500 beat-vol 0 f 500 vol 0]
  (let [rate 1.3

        wob (* wob rate)

        shape (select shape [(lf-tri wob) (lf-saw wob)])
        sweep (lin-exp shape -1 1 40 sweep-max-freq)
        snd   (mix (saw (* (midicps note) [0.99 1.01])))
        snd   (lpf snd sweep)
        snd   (normalizer snd)

        snd   (+ snd (bpf snd 1500 2))
        ;;special flavours
        ;;hi manster
        snd   (select (> hi-man 0.05) [snd (* 4 (hpf snd hi-man-max))])

        ;;sweep manster
        snd   (select (> sweep-man 0.05) [snd (* 4 (hpf snd sweep))])

        ;;lo manster
        snd   (select (> lo-man 0.05) [snd (lpf snd lo-man-max)])

        ;;decimate
        snd   (select (> deci-man 0.05) [snd (round snd 0.1)])

        ;;crunch
        snd   (select (> tan-man 0.05) [snd (tanh (* snd 5))])

        snd   (* 0.5 (+ (* 0.8 snd) (* 0.3 (g-verb snd 100 0.7 0.7))))

        kick-pat [1 0 0 0  1 0 0 0  0 0 0 0  0 0 1 0]
        hhat-pat [0 0 1 0  0 0 1 0  0 0 0 0  0 0 1 0]
        hht2-pat [0 0 0 1  0 0 0 1  0 0 0 0  0 0 0 1]
        snar-pat [0 0 0 0  1 0 1 0  0 0 0 0  0 0 0 0]
        spls-pat [1 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0]

        drum-trig (impulse:kr (* rate 8))

        kick (demand drum-trig 0 (dseq kick-pat INF))
        hhat (demand drum-trig 0 (dseq hhat-pat INF))
        hht2 (demand drum-trig 0 (dseq hht2-pat INF))
        snar (demand drum-trig 0 (dseq snar-pat INF))
        spls (demand drum-trig 0 (dseq spls-pat INF))

        ]
    (out:kr kick-b kick)
    (out:kr hhat-b hhat)
    (out:kr hht2-b hht2)
    (out:kr snar-b snar)
    (out:kr spls-b spls)
    (* vol (lpf snd f))

    ))

;;start things up

(let [drum-g (group :tail @synth-group*)]
  (kick-i :tgt drum-g)
  (hhat-i :tgt drum-g)
  (hht2-i :tgt drum-g)
  (snar-i :tgt drum-g)
  (spls-i :tgt drum-g)
  (dubstep))

;;play about and have fun!

;; fade in...
(do
  (ctl drum-g :vol 0.7)
  (dorun (map #(do (ctl drum-g :f (inc %)) (Thread/sleep 1)) (range 5000))))

(do
  (dorun (map #(do (ctl dubstep :f (inc %)) (ctl dubstep :vol (min 1.1 (scale-range % 0 1000 0 10))) (Thread/sleep 5)) (range 1000))))


;;modulate params directly

(ctl dubstep :f 10000)
(ctl dubstep :wob 0.5)
(ctl dubstep :wob 3)
(ctl dubstep :note 22)
(ctl dubstep :vol 1)
(ctl dubstep :sweep-man 0)
(ctl dubstep :sweep-man 1)


;; fade out...
(do
  (dorun (map #(do (ctl dubstep :vol (/ % 100)) (ctl drum-g :vol (/ % 100)) (Thread/sleep 45))    (reverse (range 20 100)) ))
  (dorun (map #(do (ctl dubstep :vol (/ % 100)) (ctl drum-g :vol (/ % 100)) (Thread/sleep 200))    (reverse (range 1 20)) ))
  (ctl dubstep :vol 0)
  (ctl drum-g :vol 0))
