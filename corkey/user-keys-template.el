;;; -*- no-byte-compile: t -*-

;; This is your user keys file, here you can configure key bindings that will
;; get added to Corgi. You can also override Corgi's default bindings this way.
(bindings
 ;; "global" bindings are always active regardless of Evil's "state" (= vim mode)
 (global
  )

 ;; Bindings for commands are usually only active in normal and visual state
 (normal|visual
  ("SPC"
   ("p"
    ;; Try `SPC t t'
    ("t" "Play Tetris" tetris)
    ("d" "Play the Dunnet text adventure" dunnet)))))
