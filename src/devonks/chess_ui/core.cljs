(ns devonks.chess-ui.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]))

;; -------------------------
;; Views
(defn- square->coords [square]
  (let [x (case (first square)
            "a" 0 "b" 100 "c" 200 "d" 300 "e" 400 "f" 500 "g" 600 "h" 700)
        y (case (second square)
            "1" 700 "2" 600 "3" 500 "4" 400 "5" 300 "6" 200 "7" 100 "8" 0)]
    [x y]))

(defn- coords->square
  [[x y]]
  (let [file (case x
               0 "a" 100 "b" 200 "c" 300 "d" 400 "e" 500 "f" 600 "g" 700 "h")
        rank (case y
               700 "1" 600 "2" 500 "3" 400 "4" 300 "5" 200 "6" 100 "7" 0 "8")]
    (str file rank)))

(defn- row+column->square
  [row column]
  (let [file (case column
               0 "a" 1 "b" 2 "c" 3 "d" 4 "e" 5 "f" 6 "g" 7 "h")
        rank (case row
               7 "1" 6 "2" 5 "3" 4 "4" 3 "5" 2 "6" 1 "7" 0 "8")]
    (str file rank)))

(defn- normalise-coords
  [[x y]]
  [(* 100 (Math/floor (/ x 100)))
   (* 100 (Math/floor (/ y 100)))])

(defn- piece-comp [piece [x y]]
  [:image {:x x
           :y y
           :width 100
           :height 100
           :draggable true
           :href (str "img/" piece ".svg")}])

(defonce game-state (r/atom {:pieces [["r" "a8"] ["n" "b8"] ["b" "c8"] ["q" "d8"] ["k" "e8"] ["b" "f8"] ["n" "g8"] ["r" "h8"]
                                      ["p" "a7"] ["p" "b7"] ["p" "c7"] ["p" "d7"] ["p" "e7"] ["p" "f7"] ["p" "g7"] ["p" "h7"]
                                      ["P" "a2"] ["P" "b2"] ["P" "c2"] ["P" "d2"] ["P" "e2"] ["P" "f2"] ["P" "g2"] ["P" "h2"]
                                      ["R" "a1"] ["N" "b1"] ["B" "c1"] ["Q" "d1"] ["K" "e1"] ["B" "f1"] ["N" "g1"] ["R" "h1"]]
                             :drag-piece nil
                             :drag-coords nil
                             :offset nil
                             :square-classes {}
                             :right-mouse-down-square nil}))

(defn- get-svg-coordinates
  [e]
  (let [ctm (.getScreenCTM (.getElementById js/document "chess-board-svg"))
        svg-x (/ (- (.-clientX e) (.-e ctm))
                 (.-a ctm))
        svg-y (/ (- (.-clientY e) (.-f ctm))
                 (.-d ctm))]
    [svg-x svg-y]))

(defn- get-square
  [e]
  (let [svg-coords (get-svg-coordinates e)
        coords (normalise-coords svg-coords)
        square (coords->square coords)]
    square))

(defn- unhighlight-squares
  [state]
  (assoc state :square-classes {}))

(defn- handle-drag
  [state e]
  (let [target (.-target e)
        [x y] (get-svg-coordinates e)
        square (get-square e)
        new-square-classes (assoc (:square-classes state)
                                  square "original-square")
        piece-x (js/parseFloat (.getAttributeNS target nil "x"))
        piece-y (js/parseFloat (.getAttributeNS target nil "y"))
        offset-x (- x piece-x)
        offset-y (- y piece-y)
        offset-vec [offset-x offset-y]]
    (.preventDefault e)
    (assoc state
           :drag-piece (first (filter #(= square (second %)) (:pieces state)))
           :drag-coords [piece-x piece-y]
           :offset offset-vec
           :square-classes new-square-classes)))

(defn- handle-highlight
  [state e]
  (.preventDefault e)
  (let [square (get-square e)
        square-classes (:square-classes state)
        already-highlighted (contains? square-classes square)
        highlight-class (cond
                          already-highlighted
                          ""

                          (not (or (.-altKey e)
                                   (.-ctrlKey e)
                                   (.-shiftKey e)))
                          "default-highlight"

                          (and (.-ctrlKey e)
                               (not (or (.-altKey e)
                                        (.-shiftKey e))))
                          "highlight-1"

                          (and (.-altKey e)
                               (not (or (.-ctrlKey e)
                                        (.-shiftKey e))))
                          "highlight-2"

                          (and (.-shiftKey e)
                               (not (or (.-ctrlKey e)
                                        (.-altKey e))))
                          "highlight-3")]
    (if already-highlighted
      (update state :square-classes dissoc square)
      (update state :square-classes assoc square highlight-class))))

(defn- handle-mouse-down
  [e state]
  (when (= (.-button e) 0)
    (unhighlight-squares state))
  (cond
    (and (= (.-button e) 0)
         (.. e -target -attributes -draggable))
    (-> state
        unhighlight-squares
        (handle-drag e))

    (= (.-button e) 0)
    (unhighlight-squares state)

    (= (.-button e) 2)
    (assoc state :right-mouse-down-square (get-square e))

    :else
    state))

(defn- handle-mouse-move
  [e state]
  (if (:drag-piece state)
    (let [[coord-x coord-y] (get-svg-coordinates e)
          [offset-x offset-y] (:offset state)
          new-x (- coord-x offset-x)
          new-y (- coord-y offset-y)]
      (.preventDefault e)
      (assoc state :drag-coords [new-x new-y]))
    state))

(defn- handle-mouse-up
  [e state]
  (cond
    (and (= (.-button e) 0)
         (:drag-piece state))
    (let [pieces (:pieces state)
          [_ old-square] (:drag-piece state)
          new-square (get-square e)
          new-pieces (map (fn [[p s :as r]]
                            (if (= old-square s)
                              [p new-square]
                              r))
                          pieces)]
      (.preventDefault e)
      (assoc state
             :drag-piece nil
             :drag-coords nil
             :pieces new-pieces
             :square-classes {}
             :offset nil))

    (= (.-button e) 2)
    (let [down-square (:right-mouse-down-square state)
          up-square (get-square e)]
      (if (= down-square up-square)
        (handle-highlight state e)
        state))

    :else
    state))

(defn- handle-mouse-event!
  [event-fn e]
  (let [state @game-state
        new-state (event-fn e state)]
    (reset! game-state new-state)))

;; TODO
;; DONE - Refactor drag event code to be more functional and use clojure data structures where ever possible
;; DONE - Highlight original square when moving piece
;; DONE - allow for highlighting squares
;; allow for drawing arrows
;; allow for moving piece by clicking instead of dragging
;; Validate that piece is moved to a legal square
;; Draw dots on legal squares when moving a piece
(defn chess-board []
  (let [state @game-state
        square-classes (:square-classes state)
        pieces (:pieces state)
        svg [:svg {:id  "chess-board-svg"
                   :width "800"
                   :height "800"
                   :viewport "0 0 800 800"
                   :onMouseDown (partial handle-mouse-event! handle-mouse-down)
                   :onMouseMove (partial handle-mouse-event! handle-mouse-move)
                   :onMouseUp (partial handle-mouse-event! handle-mouse-up)
                   :onContextMenu #(.preventDefault %)}]
        board (map
               (fn [i]
                 (let [row (Math/floor (/ i 8))
                       column (rem i 8)
                       square (row+column->square row column)
                       class (cond
                               (contains? square-classes square)
                               (get square-classes square)

                               (or (and (zero? (mod column 2))
                                        (zero? (mod row 2)))
                                   (and (not (zero? (mod column 2)))
                                        (not (zero? (mod row 2)))))
                               "square-light"

                               (or (and (not (zero? (mod column 2)))
                                        (zero? (mod row 2)))
                                   (and (zero? (mod column 2))
                                        (not (zero? (mod row 2)))))
                               "square-dark")

                       x (* 100 column)
                       y (* 100 row)
                       coords [x y]
                       square (coords->square coords)]
                   [:rect {:width "100"
                           :height "100"
                           :x x
                           :y y
                           :id square
                           :class class}]))
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
                  (if (= square (second (:drag-piece state)))
                    (piece-comp piece (:drag-coords state))
                    (piece-comp piece (square->coords square))))
                pieces)]
    (-> svg
        (into board)
        (into rank-coordinates)
        (into file-coordinates)
        (into pieces))))

(defn home-page []
  [:div {:class "main-div"}
   [chess-board]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
