
(ns prejure.piclang.text
  (:use [clojure core]
        [clojure.contrib str-utils java-utils])
  (import (java.awt Rectangle Graphics Graphics2D Color Font Shape)
	  (java.awt.geom Point2D Point2D$Double AffineTransform GeneralPath)
	  (java.awt.font LineBreakMeasurer TextLayout TextAttribute)
	  (java.text AttributedString CharacterIterator))
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
		       (.draw layout g (.x start-pt) next-y-pos)
		       (if next-layout
			 (list next-layout next-y-pos)
			 nil))
		     nil)))
	       (list (.nextLayout measurer width) (.y start-pt)))))))

(defn draw-fitted-text [a-strs
			#^Graphics2D g
			#^Point2D top-left
			#^Point2D bottom-right]
  (let [frc (.getFontRenderContext g),
	text-shape (GeneralPath.)
	y-pos (ref 0)]
    (doseq [layout (map #(TextLayout. (.getIterator %), frc) a-strs)]
      (let [width (.getAdvance layout)
	    outline (.getOutline layout,
				 (AffineTransform/getTranslateInstance
				  (double (- (/ width 2))) (double @y-pos)))]
	(.append text-shape, outline, false)
	(dosync (alter y-pos + (.getAscent layout)))))
    (let [bounds (.getBounds text-shape),
	  w (- (.x bottom-right) (.x top-left)),
	  h (- (.y bottom-right) (.y top-left)),
	  scaling (double (min (/ w (.width bounds))
			       (/ h (.height bounds)))),
	  affine (AffineTransform.)]
      (.translate affine
		  (double (+ (.x top-left)
			     (/ (- w (* scaling (.width bounds))) 2)))
   		  (double (+ (.y top-left)
   			     (/ (- h (* scaling (.height bounds))) 2))))
      (.scale affine scaling scaling)
      (.translate affine (double (- (.x bounds)))
		  (double (- (.y bounds))))
      (.transform text-shape affine)
      (.fill g text-shape)
      (let [bounds (.getBounds text-shape)]
	(.drawRect g (.x bounds) (.y bounds)
		   (.width bounds) (.height bounds)))
      (.drawRect g (int (.x top-left)) (int (.y top-left))
		 (int w) (int h))
      text-shape)))
