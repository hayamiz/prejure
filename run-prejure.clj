
(require 'prejure)
(use 'clojure.contrib.swing-utils)
(use 'clojure.contrib.command-line)
(import '(javax.swing JFrame))

(defn main* [args]
  (do-swing-and-wait
   (let [terminal (prejure/make-jframe-terminal)]
     (.setDefaultCloseOperation (:frame terminal) JFrame/EXIT_ON_CLOSE)
     (dosync (ref-set (:painter terminal)
                      prejure/default-presentation))
     ((:show terminal)))))

(defn main [args]
  (let [player (prejure/make-default-player
		(list (prejure/pl-drawline 0 0 1 1)))]
    (do-swing-and-wait
     (let [terminal (prejure/jframe-terminal player)]
       (doto terminal
	 (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
	 (.setVisible true))))))


(main *command-line-args*)
