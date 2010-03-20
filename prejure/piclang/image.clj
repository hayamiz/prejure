
(ns prejure.piclang.image
  (:use [clojure core]
        [clojure.contrib str-utils java-utils])
  (:import (java.awt Image Rectangle Graphics Graphics2D Color Font Shape)
	   (java.awt.geom Point2D Point2D$Double AffineTransform GeneralPath)
	   (java.awt.image BufferedImage))
  )

(defn draw-fill-image
  ([#^Image img
    #^Graphics g
    #^Point2D top-left
    #^Point2D bottom-right]
     (let [h (- (.y bottom-right) (.y top-left)),
	   w (- (.x bottom-right) (.x top-left))]
       (.drawImage g img
		   (int (.x top-left))
		   (int (.y top-left))
		   (int w) (int h)
		   nil ;; observer
		   ))))

(defn draw-fit-image
  ([#^Image img
    #^Graphics g
    #^Point2D top-left
    #^Point2D bottom-right]
     (let [region-width (- (.x bottom-right) (.x top-left)),
	   region-height (- (.y bottom-right) (.y top-left)),
	   ratio (/ (.getHeight img) (.getWidth img)),
	   region-ratio (/ region-height region-width)]
       (println (double ratio), region-ratio)
       (if (> ratio region-ratio)
	 (let [h region-height,
	       w (* (.getWidth img) (/ region-height (.getHeight img)))]
	   (println w h (/ h w))
	   (.drawImage g img
		       (int (+ (.x top-left) (/ (- region-width w) 2)))
		       (int (.y top-left))
		       (int w) (int h)
		       nil ;; observer
		       ))
	 (let [w region-width,
	       h (* (.getHeight img) (/ region-width (.getWidth img)))]
	   (println w h (/ h w))
	   (.drawImage g img
		       (int (.x top-left))
		       (int (+ (.y top-left) (/ (- region-height h) 2)))
		       (int w) (int h)
		       nil ;; observer
		       ))))))
