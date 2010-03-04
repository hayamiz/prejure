
(require 'prejure)
(require 'prejure.piclang)
(use 'clojure.contrib.swing-utils)
(use 'clojure.contrib.command-line)
(import '(javax.swing JFrame))
(import '(java.awt Color))

(defn main* [args]
  (do-swing-and-wait
   (let [terminal (prejure/make-jframe-terminal)]
     (.setDefaultCloseOperation (:frame terminal) JFrame/EXIT_ON_CLOSE)
     (dosync (ref-set (:painter terminal)
                      prejure/default-presentation))
     ((:show terminal)))))

(defn main [args]
  (let [player (prejure/make-default-player
		(list
		 (prejure.piclang/with-background
		   Color/WHITE
		   (prejure.piclang/with-color
		     Color/BLACK
		     (prejure.piclang/draw-line 0 0 1 1)))))]
    (do-swing-and-wait
     (let [terminal (prejure/jframe-terminal player)]
       (doto terminal
	 (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
	 (.setVisible true))))))


(main *command-line-args*)
