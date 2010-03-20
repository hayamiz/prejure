
(ns prejure.piclang
  (:use [clojure core]
        [clojure.contrib str-utils java-utils])
  (:require [prejure.piclang text image])
  (:import (java.awt Rectangle Graphics Graphics2D Color Font)
	   (java.awt.geom Point2D$Double AffineTransform)
	   (java.awt.font LineBreakMeasurer TextLayout TextAttribute)
	   (java.text AttributedString)
	   (java.io File)
	   (javax.imageio ImageIO))
  )

(defn point [#^Double x #^Double y]
  (Point2D$Double. x y))

(defn add-pt
  ([p1 p2]
     (point (+ (.x p1) (.x p2))
	    (+ (.y p1) (.y p2))))
  ([p1 p2x p2y]
     (point (+ (.x p1) p2x)
	    (+ (.y p1) p2y))))

(defn mult-pt [scale pt]
  (point (* (.x pt) scale)
	 (* (.y pt) scale)))

(defstruct frame :origin :edge-x :edge-y)

(defn make-frame [width height]
  (struct frame
	  (point 0 0)
	  (point width 0)
	  (point 0 height)))

(defn move-frame
  ([frame vec]
     (assoc frame
       :origin (add-pt (:origin frame) vec)))
  ([frame dx dy]
     (assoc frame :origin (add-pt (:origin frame) dx dy))))

(defn scale-frame [frame scale-x scale-y]
  (assoc frame
    :edge-x (mult-pt scale-x (:edge-x frame))
    :edge-y (mult-pt scale-y (:edge-y frame))))

(defn frame-affine [frame]
  (let [origin (:origin frame)
	edge-x (:edge-x frame)
	edge-y (:edge-y frame)]
    (AffineTransform. (.x edge-x) (.y edge-x)
		      (.x edge-y) (.y edge-y)
		      (.x origin) (.y origin))))

(defn frame-coord-map [frame]
  (let [affine (frame-affine frame)]
    (fn
      ([pt]
	 (.transform affine pt nil))
      ([x y]
	 (.transform affine (point x y) nil)))))

(defn with-background [color painter]
  (fn [frame env g]
    (let [prev-bg (.getBackground g)
	  coord-map (frame-coord-map frame)
	  top-left (coord-map 0 0)
	  bottom-right (coord-map 1 1)]
      (doto g
	(.setBackground color)
	(.clearRect (int (.x top-left))
		    (int (.y top-left))
		    (int (- (.x bottom-right) (.x top-left)))
		    (int (- (.y bottom-right) (.y top-left)))))
      (painter frame env g)
      (.setBackground g prev-bg))))

(defn with-color [color painter]
  (fn [frame env g]
    (let [prev-color (.getColor g)]
      (.setColor g color)
      (painter frame env g)
      (.setColor g prev-color))))

(defn with-stroke [stroke painter]
  (fn [frame env g]
    (let [prev-stroke (.getStroke g)]
      (.setStroke g stroke)
      (painter frame env g)
      (.setStroke g prev-stroke))))

(defn with-font [[font-name font-size] painter]
  (let [f (Font. font-name, 0, font-size)]
    (fn [frame env g]
      (painter frame (assoc env :font f) g)
	)))

(defn flip-vert [painter]
  (fn [frame env g]
    (painter (move-frame (scale-frame frame 1 -1)
			 (:edge-y frame))
	     env
	     g)))

(defn flip-horiz [painter]
  (fn [frame env g]
    (painter (move-frame (scale-frame frame 1 -1)
			 (:edge-x frame))
	     env
	     g)))

(defn beside [painter1 painter2]
  (fn [frame env g]
    (let [left (scale-frame frame 0.5 1)
	  right (move-frame left (:edge-x left))]
      (painter1 left env g)
      (painter2 right env g))))

(defn below [painter1 painter2]
  (fn [frame env g]
    (let [up (scale-frame frame 1 0.5)
	  down (move-frame up (:edge-y up))]
      (painter1 up env g)
      (painter2 down env g))))

(defn verticals [& painter-width-pairs]
  (let [width-sum (apply + (map first
				(filter #(number? (first %))
					painter-width-pairs)))
	num-non-number (count (filter #(not (number? (first %))) painter-width-pairs)),
	non-number-width (/ (max 0 (- 1 width-sum)) (max 1 num-non-number)),
	painter-width-pairs (map (fn [pair]
				   (let [width (first pair)
					 painter (second pair)]
				     (list (if (number? width)
					     (if (>= width-sum 1) ;; normalize
					       (/ width width-sum)
					       width)
					     non-number-width)
					   painter)))
				 painter-width-pairs)]
    (fn [frame env g]
      (let [x-offset (ref 0)]
	(doseq [[w painter] painter-width-pairs]
	  (let [pane-frame (move-frame
			    (scale-frame frame w 1)
			    (mult-pt @x-offset (:edge-x frame)))]
	    (painter pane-frame env g)
	    (dosync (alter x-offset + w))))))))

(defn horizontals [& painter-height-pairs]
  (let [height-sum (apply + (map first
				(filter #(number? (first %))
					painter-height-pairs)))
	num-non-number (count (filter #(not (number? (first %))) painter-height-pairs)),
	non-number-height (/ (max 0 (- 1 height-sum)) (max 1 num-non-number)),
	painter-height-pairs (map (fn [pair]
				   (let [height (first pair)
					 painter (second pair)]
				     (list (if (number? height)
					     (if (>= height-sum 1) ;; normalize
					       (/ height height-sum)
					       height)
					     non-number-height)
					   painter)))
				 painter-height-pairs)]
    (fn [frame env g]
      (let [y-offset (ref 0)]
	(doseq [[h painter] painter-height-pairs]
	  (let [pane-frame (move-frame
			    (scale-frame frame 1 h)
			    (mult-pt @y-offset (:edge-y frame)))]
	    (painter pane-frame env g)
	    (dosync (alter y-offset + h))))))))


(defn draw-line [x0 y0 x1 y1]
  (fn [frame env g]
    (let [coord-map (frame-coord-map frame)
	  start-pt (coord-map x0 y0)
	  end-pt (coord-map x1 y1)]
      (.drawLine g (.x start-pt) (.y start-pt) (.x end-pt) (.y end-pt)))))

(defn draw-poly [close? points]
  (let [n-points (int (count points))]
    (fn [frame env g]
      (let [coord-map (frame-coord-map frame)
	    points (map (fn [[x y]]
			  (let [pt (coord-map x y)]
			    (list (int (.x pt)) (int (.y pt)))))
			points)
	    x-points (int-array (map first points))
	    y-points (int-array (map second points))]
	(if close?
	  (.drawPolygon g x-points y-points n-points)
	  (.drawPolyline g x-points y-points n-points))))))

(defn draw-segment [points]
  (draw-poly false points))

(defn segments->painter [& segments]
  (let [painters
	(map draw-segment segments)]
    (fn [frame env g]
      (doseq [painter painters]
	(painter frame env g)))))

(def wave
     (segments->painter
      '((0.0 0.14) (0.16 0.4) (0.28 0.35) (0.42 0.35) (0.35 0.14) (0.42 0.0))
      '((0.58 0.0) (0.65 0.14) (0.58 0.35) (0.76 0.35) (1.0 0.65))
      '((1.0 0.86) (0.6 0.54) (0.76 1.0))
      '((0.58 1.0) (0.5 0.83) (0.42 1.0))
      '((0.24 1.0) (0.35 0.49) (0.3 0.41) (0.16 0.59) (0.0 0.35))))

(defn split [split1 split2]
  (fn [painter n]
    (if (= n 0)
      painter
      (let [smaller ((split split1 split2) painter (- n 1))]
	(split1 painter (split2 smaller smaller))))))

(defn right-split [painter n]
  ((split beside below) painter n))
(defn up-split [painter n]
  ((split below beside) painter n))

(defn corner-split [painter n]
  (if (= n 0)
    painter
    (let [up (up-split painter (- n 1))
	  right (right-split painter (- n 1))
	  top-left (beside up up)
	  bottom-right (below right right)
	  corner (corner-split painter (- n 1))]
      (beside (below painter top-left)
	      (below bottom-right corner)))))

(defn do-painters [painter & painters]
  (fn [frame env g]
    (doseq [p (cons painter painters)]
      (p frame env g))))

(def nop-painter (fn [frame env g] ))

(defn within-rect [[x y width height] painter]
  (fn [frame env g]
    (let [coord-map (frame-coord-map frame),
	  new-orig (coord-map x y),
	  frame (scale-frame frame width height)
	  frame (assoc frame :origin new-orig)]
      (painter frame env g))))

(defn with-padding
  ([sz painter]
     (within-rect [sz sz (- 1 (* 2 sz)) (- 1 (* 2 sz))] painter))
  ([x-sz y-sz painter]
     (within-rect [x-sz y-sz (- 1 (* 2 x-sz)) (- 1 (* 2 y-sz))] painter)))

(defn clone-astr [astr]
  (AttributedString. (.getIterator astr)))

(defn draw-text [str]
  (let [astrs (map #(AttributedString. %) (re-split #"\n" str))]
    (fn [frame env g]
      (let [coord-map (frame-coord-map frame),
	    start-pt (coord-map 0 0)]
	(when (:font env)
	  (doseq [astr astrs]
	    (.addAttribute astr TextAttribute/FONT (:font env))))
	(prejure.piclang.text/draw-text astrs g start-pt)))))

(defn draw-wrapped-text [astr]
  (fn [frame env g]
    (let [coord-map (frame-coord-map frame),
	  start-pt (coord-map 0 0),
	  top-left (coord-map 0 0),    ;; top-right
	  top-right (coord-map 1 0),   ;; top-right
	  width (- (.x top-right) (.x top-left)),
	  y-pos (.y top-left)]
      (prejure.piclang.text/draw-wrapped-text
       (do
	 (when (:font env)
	   (.addAttribute astr TextAttribute/FONT (:font env)))
	 astr)
       g start-pt width))))

(defn draw-wrapped-plain-text [str]
  (draw-wrapped-text (AttributedString. str)))

(defn draw-fitted-text [astrs]
  (fn [frame env g]
    (let [coord-map (frame-coord-map frame)]
      (prejure.piclang.text/draw-fitted-text
       astrs g (coord-map 0 0) (coord-map 1 1)))))

(defn draw-fitted-plain-text [str]
  (draw-fitted-text
   (map #(AttributedString. %) (re-split #"\n" str))))

(defn draw-fit-image [filename]
  (let [img (ImageIO/read (File. filename))]
    (fn [frame evn g]
      (let [coord-map (frame-coord-map frame)]
	(prejure.piclang.image/draw-fit-image
	 img g (coord-map 0 0) (coord-map 1 1))))))

(defn draw-fill-image [filename]
  (let [img (ImageIO/read (File. filename))]
    (fn [frame evn g]
      (let [coord-map (frame-coord-map frame)]
	(prejure.piclang.image/draw-fill-image
	 img g (coord-map 0 0) (coord-map 1 1))))))