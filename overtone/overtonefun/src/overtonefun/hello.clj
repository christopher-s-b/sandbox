(definst foo [] (saw 200))

(definst kick [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

                                        ;(kick)

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

                                        ;(c-hat)


(def metro (metronome 128))

(metro) ; => current beat number
(metro 100) ; => timestamp of 100th beat

(def metro (metronome 128))

(defn player [beat]
  (at (metro beat) (kick))
  (at (metro (+ 0.5 beat)) (c-hat))
  (apply-at (metro (inc beat)) #'player (inc beat) []))

(player (metro))
