
(ns prejure.gui
  (:use [clojure core]
        [clojure.contrib str-utils java-utils]
	[prejure piclang])
  (:import [javax.swing JFrame JPanel JLabel JButton
	    JSplitPane JFileChooser
	    Timer]
	   [java.io File]
	   [javax.swing.filechooser FileNameExtensionFilter]
	   [java.awt.event ActionListener])
  )

(def *main-window*
     (let [frame (JFrame. "Prejure"),
	   split-pane (JSplitPane. JSplitPane/VERTICAL_SPLIT)
	   button-action 
	   (proxy [ActionListener] []
	     (actionPerformed
	      [e]
	      (let [filter (FileNameExtensionFilter. "Prejure presentation file",
						     (into-array ["prj", "clj"]))
		    chooser (JFileChooser.)]
		(.setFileFilter chooser filter)
		(.setCurrentDirectory chooser (.. (File. ".")
						  getAbsoluteFile
						  getParentFile))
		(when (= JFileChooser/APPROVE_OPTION
			 (.showOpenDialog chooser frame))
		  (println "You choosed: "
			   (.. chooser getSelectedFile getPath))
		  (let [slide-sexp
			(read-string (slurp (.. chooser getSelectedFile getPath))),
			slide (eval slide-sexp)
			player (apply prejure/make-player
				      {:width 800, :height 600}
				      slide)
			terminal (prejure/jframe-terminal player)]
		    (.setVisible terminal true))))))]
       (let [button (JButton. "Open presentation")]
	 (.addActionListener button button-action)
	 (doto split-pane
	   (.add (JLabel. "Welcome to Prejure"))
	   (.add button)))
       (doto frame
	 (.add split-pane)
	 (.setSize 200 200)
	 (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
       ))
