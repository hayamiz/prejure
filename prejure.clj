;; -*- indent-tabs-mode: nil; mode: clojure  -*-

(ns prejure
  (:use [clojure core]
        [clojure.contrib str-utils java-utils])
  (:require prejure.piclang)
  (:import (java.awt Graphics Graphics2D
                     Font FontMetrics
                     RenderingHints
                     GraphicsEnvironment Color Dimension
                     Rectangle)
           (java.awt.image BufferedImage)
           (java.awt.font TextLayout)
           (java.awt.event KeyListener KeyEvent ActionListener)
           (javax.swing JFrame JPanel Timer)
           (java.util.concurrent SynchronousQueue ConcurrentLinkedQueue)))

(declare on-key toggle-fullscreen)

(def *main-frame* (ref nil))

(defn get-gdev []
  (.. GraphicsEnvironment
      getLocalGraphicsEnvironment
      getDefaultScreenDevice))

(defn slide-panel [player]
  (let [buf (ref (BufferedImage. (:width player)
                                 (:height player)
                                 BufferedImage/TYPE_4BYTE_ABGR))]
    (proxy [JPanel KeyListener] []
      (getPreferredSize []
                        (let [buf @buf]
                          (Dimension. (.getWidth buf) (.getHeight buf))))
      (paintComponent [g]
                      (proxy-super paintComponent g)
                      (let [buf @buf
                            g-buf (.createGraphics buf)]
                        (.setRenderingHint g-buf
                                           RenderingHints/KEY_TEXT_ANTIALIASING
                                           RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
                        (@(:current-painter player)
                         (prejure.piclang/make-frame (.getWidth buf) (.getHeight buf))
                         g-buf)
                        (.drawImage g buf 0 0
                                    (.getWidth this) (.getHeight this) this)))
      (keyPressed [e]
                  (let [key-ev {:type 'key-event,
                                :keycode (.getKeyCode e),
                                :keymodifier (.getModifiers e)}]
                    ((:send-event player) key-ev)))
      (keyReleased [e])
      (keyTyped [e]))))

(defn make-player [params & pages]
  (let [event-queue (ConcurrentLinkedQueue.)
        timer-handler
        (proxy [ActionListener] []
          (actionPerformed [e]
                           (let [ev (.poll event-queue)]
                             (when ev
                               (println (.toString ev))))
                           ))
        timer (Timer. 20 timer-handler)]
    (.start timer)
    {:pages pages,
     :current-page (ref (first pages)),
     :current-painter (ref (first (first pages))),
     :timer timer,
     :send-event (fn [ev] (.add event-queue ev)),
     :width (:width params 640),
     :height (:height params 480)}
  ))

(defn make-jframe-terminal [& rest]
  (let [frame (JFrame. "Prejure: Presentation Software")
        font (Font. "VL ゴシック", (+ Font/BOLD), 30)
        buff (ref nil)
        painter (ref nil)
        idx (ref 0)
        pane
        (proxy [JPanel] []
          (update [g]
                  (.paint this g))
          (paint [g]
                 (let [w (.getWidth this) h (.getHeight this)]
                   (when-not (and @buff (= w (.getWidth @buff)) (= h (.getHeight @buff)))
                     (let [b (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
                           g (.getGraphics b)]
                       (dosync (ref-set buff b))))
                   (let [g (.getGraphics @buff)]
                     (doto g
                       (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING
                                          RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
                       (.setColor Color/BLACK)
                       (.setBackground Color/WHITE)
                       (.setFont font)
                       (.clearRect 0 0 w h))
                     (println (nth @painter @idx))
                     ((nth @painter @idx) g w h)))
                 (.drawImage g @buff 0 0 this)))]
    (doto frame
      (.setBounds 0 0
                  (* 0.8 (.. (get-gdev) getDisplayMode getWidth))
                  (* 0.8 (.. (get-gdev) getDisplayMode getHeight)))
      (.setContentPane pane)
      (.addKeyListener
       (proxy [KeyListener] []
         (keyPressed [ke]
                     (let [str (.toString ke)]
                       (condp = (.getKeyCode ke)
                         KeyEvent/VK_F (toggle-fullscreen frame)
                         KeyEvent/VK_ENTER (do
                                             (println @idx)
                                             (nth @painter @idx)
                                             (dosync (alter idx + 1))
                                             (println @idx)
                                             (nth @painter @idx)
                                             (.repaint frame))
                         (println (.getKeyCode ke)))))
         (keyReleased [ke] )
         (keyTyped [ke] ))))
    (doto pane
      (.revalidate))
    {:show (fn [] (.setVisible frame true))
     :painter painter
     :frame frame
     }))

(defn jframe-terminal [player]
  (let [panel (slide-panel player)
        frame (JFrame. "Prejure: Presentation in Clojure")]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack))
    frame))

(defn toggle-fullscreen [frame]
  (let [gdev
        (.. GraphicsEnvironment
            getLocalGraphicsEnvironment
            getDefaultScreenDevice)]
    (if (.getFullScreenWindow gdev)
      (.setFullScreenWindow gdev nil)
      (.setFullScreenWindow gdev frame))))

(def font (Font/decode "VL Gothic"))

(defn get-pane [frame]
  (.getContentPane frame))

(defn get-graphics [comp]
  (.getGraphics comp))

(defn set-font [g font]
  (.setFont g font))

(defn draw-text [text x y]
  (fn [g]
    (.drawString g text x y)))

(defn set-visible [comp bool]
  (.setVisible comp bool))

(defn set-bounds [comp x y w h]
  (.setBounds comp x y w h))

(defn get-font-metrics [g]
  (.getFontMetrics g))

(defn collect-font-scale [g font text]
  (let [size100 (.deriveFont font (float 100.0))
        size200 (.deriveFont font (float 200.0))]
    (.setFont g size100)
    (let [fm (.getFontMetrics g)
          w100 (.stringWidth fm text)
          h100 (.getHeight fm)
          a100 (.getAscent fm)
          d100 (.getDescent fm)]
      (.setFont g size200)
      (let [fm (.getFontMetrics g)
            w200 (.stringWidth fm text)
            h200 (.getHeight fm)
            a200 (.getAscent fm)
            d200 (.getDescent fm)
            scales
            {:text text,
             :width (/ (- w200 w100) 100)
             :height (/ (- h200 h100) 100)
             :ascent (/ (- a200 a100) 100)
             :descent (/ (- d200 d100) 100)}]
        (.setFont g font)
        scales))))

(defn painter-center-text [text]
  (fn [g w h]
    (let [fm (get-font-metrics g)]
      (let [x (int (- (/ w 2) (/ (.stringWidth fm text) 2)))
            y (int (+ (/ h 2) (/ (.getHeight fm) 2)))]
        (.drawRect g x (- y (.getHeight fm))
                   (.stringWidth fm text)
                   (.getHeight fm))
        (.drawString g text x (- y (.getDescent fm)))
        ))))

(defn painter-fit-text [text]
  (fn [g w h]
    (let [scales (collect-font-scale g (.getFont g) text)
          h-size (/ h (:height scales))
          w-size (/ w (:width scales))
          size (min h-size w-size)]
      (let [font (.getFont g)]
        (.setFont g (.deriveFont font (float size)))
        ((painter-center-text text) g w h)
        (.setFont g font)))))

(defn set-bg [comp]
  (.setBackground comp Color/WHITE))

(defn foo []
  (def frame (JFrame. "Prejure"))
  (def g (get-graphics (get-pane frame)))
  (def font (Font. "Serif", (+ Font/BOLD Font/ITALIC), 100))
  (def font (Font. "VL ゴシック", (+ Font/BOLD), 30))
  (set-font g font)

  (set-bg (get-pane frame))
  ((painter-center-text "ほげほげ"
                     (.getWidth (get-pane frame))
                     (.getHeight (get-pane frame))) g)
  ((painter-center-text "aa"
                     (.getWidth (get-pane frame))
                     (.getHeight (get-pane frame))) g)
  ((draw-text "abc") (get-pane frame))
  (.drawString g "foo" 0 10)
  (.getFont g)

  (def fm (.getFontMetrics g))
  (.getAscent fm)
  (.getDescent fm)
  (.getHeight fm)
  )

(def default-presentation
     (map (fn [str]
            (fn [g w h]
              ((painter-fit-text str) g w h)))
          '("たのしいClojure" "たのしいPleajure" "Pleasure with Clojure")))

