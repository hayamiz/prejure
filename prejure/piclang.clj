
(ns prejure.piclang
  (:use [clojure core]
        [clojure.contrib str-utils java-utils])
  (import (java.awt Rectangle Graphics Graphics2D Color)
	  (java.awt.geom Point2D$Double AffineTransform))
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
  (fn [frame g]
    (let [prev-bg (.getBackground g)
	  coord-map (frame-coord-map frame)
	  top-left (coord-map 0 0)
	  bottom-right (coord-map 1 1)]
      (doto g
	(.setBackground color)
	(.clearRect (int (.x top-left))
		    (int (.y top-left))
		    (int (.x bottom-right))
		    (int (.y bottom-right))))
      (painter frame g)
      (.setBackground g prev-bg))))

(defn with-color [color painter]
  (fn [frame g]
    (let [prev-color (.getColor g)]
      (.setColor g color)
      (painter frame g)
      (.setColor g prev-color))))

(defn with-stroke [stroke painter]
  (fn [frame g]
    (let [prev-stroke (.getStroke g)]
      (.setStroke g stroke)
      (painter frame g)
      (.setStroke g prev-stroke))))

(defn draw-line [x0 y0 x1 y1]
  (fn [frame g]
    (let [coord-map (frame-coord-map frame)
	  start-pt (coord-map x0 y0)
	  end-pt (coord-map x1 y1)]
    (.drawLine g (.x start-pt) (.y start-pt) (.x end-pt) (.y end-pt)))))
