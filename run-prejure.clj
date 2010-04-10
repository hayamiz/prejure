
(require 'prejure)
(use 'prejure.piclang)
(use 'prejure.gui)
(use 'clojure.contrib.swing-utils)
(use 'clojure.contrib.command-line)
(import '(javax.swing JFrame))
(import '(java.awt Color))

(defn main [args]
  (let [player
	(prejure/make-player
	 {:width 800, :height 600}
	 (list
	  (within-rect
	   [1/4 1/4 1/2 1/2]
	   (with-background
	     Color/BLACK
	     nop-painter)))
	 (list
	  (do-painters
	   (with-background
	     Color/WHITE
	     (with-color
	       Color/BLACK
	       (corner-split wave 6)))
	   (within-rect
	    [1/4 1/4 1/2 1/2]
	    (with-background
	      Color/BLACK
	      nop-painter))
	   (within-rect
	    [1/4 1/4 1/2 1/2]
	    (with-color
	      Color/WHITE
	      (draw-wrapped-plain-text
		      (apply str
			     (interpose
			      ". " (repeat 10 "The quick brown fox jumps over the lazy dog"))))))))
	 (list
	  (with-background
	    Color/WHITE
	    (with-color
	      Color/BLACK
	      (verticals [1/2 (draw-line 0 0 1 1)]
			 [1/4 (draw-line 0 0 1 1)]
			 [1/8 (draw-line 0 0 1 1)]
			 [1/16 (draw-line 0 0 1 1)]
			 [1/32 (draw-line 0 0 1 1)]
			 [1/64 (draw-line 0 0 1 1)]
			 [*   (draw-line 0 0.5 1 0.5)]))))
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
		  (corner-split wave 3)
		  (below
		   line (flip-vert line)))))))
	  (with-background
	    Color/WHITE
	    (with-color
	      Color/BLACK
	      (let [line
		    (draw-line 0 0 1 1)]
		(beside
		 line (flip-vert line))))))
	 (list
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

(defn main [args]
  (do-swing-and-wait
   (.setVisible prejure.gui/*main-window* true)
   ))

(main *command-line-args*)
