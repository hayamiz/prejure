
(require 'prejure)
(use 'prejure.piclang)
(use 'clojure.contrib.swing-utils)
(use 'clojure.contrib.command-line)
(import '(javax.swing JFrame))
(import '(java.awt Color))

;;(defn main* [args]
;;  (do-swing-and-wait
;;   (let [terminal (prejure/make-jframe-terminal)]
;;     (.setDefaultCloseOperation (:frame terminal) JFrame/EXIT_ON_CLOSE)
;;     (dosync (ref-set (:painter terminal)
;;                      prejure/default-presentation))
;;     ((:show terminal)))))

(defn main [args]
  (let [player
	(prejure/make-player
	 {:width 800, :height 600}
	 (list
	  (with-background
	    Color/WHITE
	    (with-color
	      Color/BLACK
	      (let [line
		    (draw-line 0 0 1 1)]
		(beside
		 (below
		  (draw-fitted-plain-text
		   "Hello world")
		  (draw-wrapped-plain-text
		   (apply str
			  (interpose
			   ". " (repeat 10 "The quick brown fox jumps over the lazy dog")))))
		 (below
		  line (flip-vert line))))))
	  (with-background
	    Color/WHITE
	    (with-color
	      Color/BLACK
	      (let [line
		    (draw-line 0 0 1 1)]
		(beside
		 line (flip-vert line)))))))]
    (do-swing-and-wait
     (let [terminal (prejure/jframe-terminal player)]
       (doto terminal
	 (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
	 (.setVisible true))))))


(main *command-line-args*)
