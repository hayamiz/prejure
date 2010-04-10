;; -*- indent-tabs-mode: nil; mode: clojure  -*-

(ns prejure
  (:use [clojure core]
        [clojure.contrib str-utils java-utils]
        [prejure.misc])
  (:require prejure.piclang)
  (:import (java.awt Graphics Graphics2D
                     Font FontMetrics
                     RenderingHints
                     GraphicsEnvironment Color Dimension
                     Rectangle
                     Dialog$ModalExclusionType)
           (java.awt.image BufferedImage)
           (java.awt.font TextLayout)
           (java.awt.event KeyListener KeyEvent ActionListener ComponentListener)
           (javax.swing JFrame JPanel Timer)
           (java.util.concurrent SynchronousQueue ConcurrentLinkedQueue)))

(declare on-key toggle-fullscreen)

(defn player-current-page [player]
  (first @(:current-page player)))

(defn player-current-painter [player]
  (first @(:current-painter player)))

(defn player-next-page [player]
  (dosync
   (when (not (empty? (rest @(:current-page player))))
     (alter (:current-page player) rest)
     (alter (:current-page-num player) inc)
     (ref-set (:current-painter player) (first @(:current-page player))))))

(defn player-prev-page [player]
  (dosync
   (let [page-num @(:current-page-num player)]
     (when (> page-num 0)
       (ref-set (:current-page player) (nthnext (:pages player) (- page-num 1)))
       (ref-set (:current-page-num player) (- page-num 1))
       (ref-set (:current-painter player) (first @(:current-page player)))))))

(defn make-player [params & pages]
  (let [player {:pages pages,
                :current-page (ref pages),
                :current-painter (ref (first pages)),
                :current-page-num (ref 0),
                :width (:width params 640),
                :height (:height params 480)}]
    (let [event-queue (ConcurrentLinkedQueue.)
          timer-hooks (ref ())
          key-hooks (ref ())
          timer-handler
          (proxy [ActionListener] []
            (actionPerformed [e]
                             (let [ev (.poll event-queue)]
                               (when ev
                                 (println (.toString ev))
                                 (condp = (:type ev)
                                   'key-pressed
                                   (do (case (:keycode ev)
                                             (KeyEvent/VK_ENTER KeyEvent/VK_RIGHT)
                                             (do
                                               (player-next-page player)
                                               (println (:type ev)))

                                             (KeyEvent/VK_BACK_SPACE KeyEvent/VK_LEFT)
                                             (do
                                               (player-prev-page player)
                                               (println (:type ev)))

                                             (println "No matching key")
                                             )
                                       (doseq [hook @key-hooks]
                                         (hook ev)))))
                               (doseq [hook @timer-hooks]
                                 (hook))
                               )))
          timer (Timer. 20 timer-handler)]
      (.start timer)
      (let [player (assoc player :send-event (fn [ev] (.add event-queue ev)))
            player (assoc player :timer timer)
            player (assoc player :timer-hooks timer-hooks)
            player (assoc player :key-hooks key-hooks)]
        player))
  ))

(def *main-frame* (ref nil))

(defn get-gdev []
  (.. GraphicsEnvironment
      getLocalGraphicsEnvironment
      getDefaultScreenDevice))

(defn enable-anti-aliasing [g]
  (doto g
    (.setRenderingHint
     RenderingHints/KEY_ANTIALIASING
     RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint
     RenderingHints/KEY_TEXT_ANTIALIASING
     RenderingHints/VALUE_TEXT_ANTIALIAS_ON)))

(defn slide-panel [player]
  (let [buf (ref (BufferedImage. (:width player)
                                 (:height player)
                                 BufferedImage/TYPE_4BYTE_ABGR))
        panel
        (proxy [JPanel KeyListener ComponentListener] []
          (getPreferredSize []
                            (let [buf @buf]
                              (Dimension. (.getWidth buf) (.getHeight buf))))
          (paintComponent [g]
                          (proxy-super paintComponent g)
                          (let [buf @buf,
                                g-buf (.createGraphics buf)]
                            (doto g-buf
                              (.setRenderingHint
                               RenderingHints/KEY_ANTIALIASING
                               RenderingHints/VALUE_ANTIALIAS_ON)
                              (.setRenderingHint
                               RenderingHints/KEY_TEXT_ANTIALIASING
                               RenderingHints/VALUE_TEXT_ANTIALIAS_ON))
                            ((player-current-painter player)
                             (prejure.piclang/make-frame (.getWidth buf) (.getHeight buf))
                             {}
                             g-buf)
                            (.drawImage g buf 0 0
                                        (.getWidth this) (.getHeight this) this)))
          (keyPressed [e]
                      (condp = (.getKeyCode e)
                        KeyEvent/VK_F5
                        (let [frame (.. this getParent getParent getParent getParent)]
                          (toggle-fullscreen frame)
                          (.requestFocus this))
                        
                        (let [key-ev {:type 'key-pressed,
                                      :keycode (.getKeyCode e),
                                      :keymodifier (.getModifiers e)}]
                          ((:send-event player) key-ev))))
          (keyReleased [e])
          (keyTyped [e])
          (componentResized [e]
                            (dosync
                             (ref-set
                              buf (BufferedImage. (.getWidth this)
                                                  (.getHeight this)
                                                  BufferedImage/TYPE_4BYTE_ABGR)))
                            (.repaint this))
          (componentHidden [e])
          (componentMoved [e])
          (componentShown [e])
          )]
    (dosync
     (ref-set (:key-hooks player)
              (cons (fn [key-event]
                      (.repaint panel))
                    @(:key-hooks player))))
    panel
    ))

(defn jframe-terminal [player]
  (let [panel (slide-panel player)
        frame (JFrame. "Prejure: Presentation in Clojure")]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel)
      (.addComponentListener panel))
    (doto frame
      (.setFocusable false)
      (.add panel)
      (.pack))
    frame))

(defn toggle-fullscreen [frame]
  (let [gdev
        (.. GraphicsEnvironment
            getLocalGraphicsEnvironment
            getDefaultScreenDevice)]
    (.hide frame)
    (.removeNotify frame)
    (if (.getFullScreenWindow gdev)
      (do
        (.setUndecorated frame false)
        (.show frame)
        (.setFullScreenWindow gdev nil))
      (do
        (.setUndecorated frame true)
        (.show frame)
        (.setFullScreenWindow gdev frame)))))

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
