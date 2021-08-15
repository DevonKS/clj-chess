(ns devonks.chess-ui.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]))

;; -------------------------
;; Views
(defn square->coords [square]
  (let [x (case (first square)
            "a" 0
            "b" 100
            "c" 200
            "d" 300
            "e" 400
            "f" 500
            "g" 600
            "h" 700)
        y (case (second square)
            "1" 700
            "2" 600
            "3" 500
            "4" 400
            "5" 300
            "6" 200
            "7" 100
            "8" 0)]
    [x y]))

(defn piece-comp [piece [x y]]
  [:image {:x x
           :y y
           :width 100
           :height 100
           :draggable true
           :href (str "img/" piece ".svg")}])

(def drag-state (atom {:svg-document nil
                       :svg-root nil
                       :true-coords nil
                       :grab-point nil
                       :back-drop nil
                       :drag-target nil}))

(defn get-true-coords! [e]
  (let [{:keys [svg-root true-coords]} @drag-state
        new-scale (.-currentScale svg-root)
        translation (.-currentTranslate svg-root)]
    (set! (.-x true-coords) (/ (- (.-clientX e) (.-x translation))
                               new-scale))
    (set! (.-y true-coords) (/ (- (.-clientY e) (.-y translation))
                               new-scale))))

(defn init-drag-piece! [e]
  (swap! drag-state assoc :svg-document (.. e -target -ownerDocument))

  ;; FIXME this statement causes a type inference warning. fix it.
  (swap! drag-state assoc :svg-root (.. e -target -ownerSVGElement))

  (swap! drag-state assoc :true-coords (.createSVGPoint (get @drag-state :svg-root)))
  (swap! drag-state assoc :grab-point (.createSVGPoint (get @drag-state :svg-root)))
  (swap! drag-state assoc :back-drop (.getElementById (get @drag-state :svg-document) "BackDrop")))

(defn grab-piece! [e]
  (let [target-element (.-target e)
        draggable (.. target-element -attributes -draggable)]
    (when (and (not (= target-element (get @drag-state :back-drop)))
               draggable)
      (let [drag-target target-element
            trans-matrix (.getCTM ^EventTarget drag-target)
            {:keys [grab-point true-coords]} @drag-state]
        (swap! drag-state assoc :drag-target drag-target)
        (.appendChild (.-parentNode drag-target) drag-target)
        (.setAttributeNS drag-target nil, "pointer-events", "none")
        (set! (.-x grab-point) (- (.-x true-coords) (js/Number (.-e trans-matrix))))
        (set! (.-y grab-point) (- (.-y true-coords) (js/Number (.-f trans-matrix))))))))

(defn drag-piece! [e]
  (get-true-coords! e)
  (let [drag-target (get @drag-state :drag-target)]
    (when drag-target
      (let [{:keys [true-coords grab-point]} @drag-state
            new-x (- (.-x true-coords) (.-x grab-point))
            new-y (- (.-y true-coords) (.-y grab-point))]
        (.setAttributeNS drag-target nil "transform" (str "translate(" new-x "," new-y ")"))))))

;; FIXME Snap back to starting spot if target is an image or the move is invalid
(defn drop-piece! [e]
  (let [drag-target (get @drag-state :drag-target)]
    (when drag-target
      (get-true-coords! e)
      (let [target-element (.-target e)
            new-x (.. target-element -attributes -x -value)
            new-y (.. target-element -attributes -y -value)]
        (.setAttribute drag-target "x" new-x)
        (.setAttribute drag-target "y" new-y)
        (.setAttributeNS drag-target nil "transform" "")
        (.setAttributeNS drag-target nil "pointer-events" "all")
        (if (= (.. target-element -parentNode -id) "folder")
          (do (.appendChild (.-parentNode target-element) drag-target)
              (js/alert (str (.-id drag-target) " has been dropped into a folder, and has been inserted as a child of the containing group.")))
          (js/alert (str (.-id drag-target) " has been dropped on top of " (.-id target-element))))
        (swap! drag-state assoc :drag-target nil)))))

;; TODO
;; Validate that piece is moved to a legal square
;; Refactor drag event code to be more functional and use clojure data structures where ever possible
;; Highlight original square when moving piece
;; Draw dots on legal squares when moving a piece
;; allow for highlighting squares
;; allow for drawing arrows
;; allow for moving piece by clicking instead of dragging
(defn chess-board [pieces]
  (let [svg [:svg {:style {:width "800px"
                           :height "800px"}
                   :onLoad init-drag-piece!
                   :onMouseDown grab-piece!
                   :onMouseMove drag-piece!
                   :onMouseUp drop-piece!}]
        board (map
               (fn [i]
                 (let [row (Math/floor (/ i 8))
                       column (rem i 8)
                       fill (cond
                              (or (and (zero? (mod column 2))
                                       (zero? (mod row 2)))
                                  (and (not (zero? (mod column 2)))
                                       (not (zero? (mod row 2)))))
                              "#d8dee9"

                              (or (and (not (zero? (mod column 2)))
                                       (zero? (mod row 2)))
                                  (and (zero? (mod column 2))
                                       (not (zero? (mod row 2)))))
                              "#5e81ac")]

                   [:rect {:width "100"
                           :height "100"
                           :x (* 100 column)
                           :y (* 100 row)
                           :style {:fill fill}}]))
               (range 64))
        rank-coordinates (mapv
                          (fn [i]
                            (let [offset (- 800 (* i 100))
                                  class (if (zero? (mod i 2))
                                          "coordinate-dark"
                                          "coordinate-light")]
                              [:text {:x 5
                                      :y (+ 20 offset)
                                      :font-size 20
                                      :class class}
                               i]))
                          (range 1 9))
        file-coordinates (mapv
                          (fn [i]
                            (let [offset (* (dec i) 100)
                                  class (if (zero? (mod i 2))
                                          "coordinate-dark"
                                          "coordinate-light")
                                  label (case i
                                          1 "a"
                                          2 "b"
                                          3 "c"
                                          4 "d"
                                          5 "e"
                                          6 "f"
                                          7 "g"
                                          8 "h")]
                              [:text {:x (+ 80 offset)
                                      :y 795
                                      :font-size 20
                                      :class class}
                               label]))
                          (range 1 9))
        pieces (mapv
                (fn [[piece square]]
                  (let [coords (square->coords square)]
                    (piece-comp piece coords)))
                pieces)]
    (-> svg
        (into board)
        (into rank-coordinates)
        (into file-coordinates)
        (into pieces))))

(defn home-page []
  [:div {:class "main-div"}
   [chess-board [["r" "a8"] ["n" "b8"] ["b" "c8"] ["q" "d8"] ["k" "e8"] ["b" "f8"] ["n" "g8"] ["r" "h8"]
                 ["p" "a7"] ["p" "b7"] ["p" "c7"] ["p" "d7"] ["p" "e7"] ["p" "f7"] ["p" "g7"] ["p" "h7"]
                 ["P" "a2"] ["P" "b2"] ["P" "c2"] ["P" "d2"] ["P" "e2"] ["P" "f2"] ["P" "g2"] ["P" "h2"]
                 ["R" "a1"] ["N" "b1"] ["B" "c1"] ["Q" "d1"] ["K" "e1"] ["B" "f1"] ["N" "g1"] ["R" "h1"]]]])

;; -------------------------
;; Initialize app


(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
