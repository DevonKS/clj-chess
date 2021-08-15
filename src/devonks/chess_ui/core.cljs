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

(def drag-state (atom {:svg-element nil
                       :drag-target nil
                       :offset nil}))

(defn init-drag-piece
  [e state]
  (assoc state :svg-element (.-target e)))

(defn- get-svg-coordinates
  [e svg]
  (let [ctm (.getScreenCTM ^SVGGraphicsElement svg)
        svg-x (/ (- (.-clientX e) (.-e ctm))
                 (.-a ctm))
        svg-y (/ (- (.-clientY e) (.-f ctm))
                 (.-d ctm))]
    [svg-x svg-y]))

(defn grab-piece
  [e state]
  (if (.. e -target -attributes -draggable)
    (let [target (.-target e)
          [x y] (get-svg-coordinates e (:svg-element state))
          offset-x (- x (js/parseFloat (.getAttributeNS target nil "x")))
          offset-y (- y (js/parseFloat (.getAttributeNS target nil "y")))
          offset-vec [offset-x offset-y]]
      (.preventDefault e)
      (assoc state
             :drag-target target
             :offset offset-vec))
    state))

(defn drag-piece
  [e state]
  (if (:drag-target state)
    (let [drag-target (:drag-target state)
          [coord-x coord-y] (get-svg-coordinates e (:svg-element state))
          [offset-x offset-y] (:offset state)
          new-x (- coord-x offset-x)
          new-y (- coord-y offset-y)]
      (.preventDefault e)
      (.setAttributeNS drag-target nil "x" new-x)
      (.setAttributeNS drag-target nil "y" new-y)
      state)
    state))

(defn drop-piece
  [e state]
  (if (:drag-target state)
    (let [drag-target (:drag-target state)
          [x y] (get-svg-coordinates e (:svg-element state))
          new-x (* 100 (Math/floor (/ x 100)))
          new-y (* 100 (Math/floor (/ y 100)))]
      (.preventDefault e)
      (.setAttributeNS drag-target nil "x" new-x)
      (.setAttributeNS drag-target nil "y" new-y)
      (assoc state :drag-target nil))
    state))

(defn handle-drag-event!
  [event-fn e]
  (let [state @drag-state
        new-state (event-fn e state)]
    (reset! drag-state new-state)))

;; TODO
;; Validate that piece is moved to a legal square
;; DONE - Refactor drag event code to be more functional and use clojure data structures where ever possible
;; Highlight original square when moving piece
;; Draw dots on legal squares when moving a piece
;; allow for highlighting squares
;; allow for drawing arrows
;; allow for moving piece by clicking instead of dragging
(defn chess-board [pieces]
  (let [svg [:svg {:width "800"
                   :height "800"
                   :viewport "0 0 800 800"
                   :onLoad (partial handle-drag-event! init-drag-piece)
                   :onMouseDown (partial handle-drag-event! grab-piece)
                   :onMouseMove (partial handle-drag-event! drag-piece)
                   :onMouseUp (partial handle-drag-event! drop-piece)}]
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
