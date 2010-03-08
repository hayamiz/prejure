
(ns prejure.piclang.text
  (:use [clojure core]
        [clojure.contrib str-utils java-utils])
  (import (java.awt Rectangle Graphics Graphics2D Color Font)
	  (java.awt.geom Point2D Point2D$Double AffineTransform)
	  (java.awt.font LineBreakMeasurer TextLayout TextAttribute)
	  (java.text AttributedString))
  )

(defn draw-wrapped-text [#^AttributedString s
			 #^Graphics2D g
			 #^Point2D start-pt
			 #^Float width]
  (let [measurer (LineBreakMeasurer. (.getIterator s), (.getFontRenderContext g))]
    (doall
     (take-while
      #(not (nil? %))
      (iterate (fn [arg]
		 (let [[layout y-pos] arg]
		   (if layout
		     (let [next-y-pos (+ y-pos (.getAscent layout))
			   next-layout (.nextLayout measurer width)]
		       (println (format "(x, y): (%f, %f), width: %f",
					(.x start-pt), (.y start-pt), width))
		       (.draw layout g (.x start-pt) next-y-pos)
		       (if next-layout
			 (list next-layout next-y-pos)
			 nil))
		     nil)))
	       (list (.nextLayout measurer width) (.y start-pt)))))))