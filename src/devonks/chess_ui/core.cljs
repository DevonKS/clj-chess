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

(defn coords->square
  [[x y]]
  (let [file (case x
               0 "a"
               100 "b"
               200 "c"
               300 "d"
               400 "e"
               500 "f"
               600 "g"
               700 "h")
        rank (case y
               700 "1"
               600 "2"
               500 "3"
               400 "4"
               300 "5"
               200 "6"
               100 "7"
               0 "8")]
    (str file rank)))

(defn normalise-coords
  [[x y]]
  [(* 100 (Math/floor (/ x 100)))
   (* 100 (Math/floor (/ y 100)))])

(defn piece-comp [piece [x y]]
  [:image {:x x
           :y y
           :width 100
           :height 100
           :draggable true
           :href (str "img/" piece ".svg")}])

(def drag-state (atom {:drag-target nil
                       :square-svg-element nil
                       :original-square-class nil
                       :offset nil
                       :highlighted-squares []}))

(defn- get-svg-coordinates
  [e]
  (let [ctm (.getScreenCTM (.getElementById js/document "chess-board-svg"))
        svg-x (/ (- (.-clientX e) (.-e ctm))
                 (.-a ctm))
        svg-y (/ (- (.-clientY e) (.-f ctm))
                 (.-d ctm))]
    [svg-x svg-y]))

(defn- unhighlight-squares
  [highlighted-squares]
  (doseq [[elem c] highlighted-squares]
    (.setAttribute elem "class" c)))

(defn- highlight-square
  [state e c]
  (let [svg-coords (get-svg-coordinates e)
        coords (normalise-coords svg-coords)
        square-svg-element (.getElementById js/document (coords->square coords))
        original-square-class (.getAttribute square-svg-element "class")]
    (.preventDefault e)
    (.setAttribute square-svg-element "class" c)
    (update state :highlighted-squares conj [square-svg-element original-square-class])))

(defn grab-piece
  [e state]
  (cond
    (and (= (.-button e) 0)
         (.. e -target -attributes -draggable))
    (let [target (.-target e)
          [x y :as svg-coords] (get-svg-coordinates e)
          coords (normalise-coords svg-coords)
          square-svg-element (.getElementById js/document (coords->square coords))
          original-square-class (.getAttribute square-svg-element "class")
          offset-x (- x (js/parseFloat (.getAttributeNS target nil "x")))
          offset-y (- y (js/parseFloat (.getAttributeNS target nil "y")))
          offset-vec [offset-x offset-y]]
      (.preventDefault e)
      (.setAttribute square-svg-element "class" "original-square")
      (unhighlight-squares (:highlighted-squares state))
      (assoc state
             :drag-target target
             :offset offset-vec
             :square-svg-element square-svg-element
             :original-square-class original-square-class
             :highlighted-squares []))

    (and (= (.-button e) 0)
         (not (.. e -target -attributes -draggable)))
    (do (unhighlight-squares (:highlighted-squares state))
        (assoc state :highlighted-squares []))

;; FIXME Need to handle the case when the square is already highlighted
    (and (= (.-button e) 2)
         (not (or (.-altKey e)
                  (.-ctrlKey e)
                  (.-shiftKey e))))
    (highlight-square state e "default-highlight")

    (and (= (.-button e) 2)
         (.-ctrlKey e)
         (not (or (.-altKey e)
                  (.-shiftKey e))))
    (highlight-square state e "highlight-1")

    (and (= (.-button e) 2)
         (.-altKey e)
         (not (or (.-ctrlKey e)
                  (.-shiftKey e))))
    (highlight-square state e "highlight-2")

    (and (= (.-button e) 2)
         (.-shiftKey e)
         (not (or (.-ctrlKey e)
                  (.-altKey e))))
    (highlight-square state e "highlight-3")

    (= (.-button e) 2)
    (do (.preventDefault e)
        state)

    :else
    state))

(defn drag-piece
  [e state]
  (if (:drag-target state)
    (let [drag-target (:drag-target state)
          [coord-x coord-y] (get-svg-coordinates e)
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
  (cond
    (and (= (.-button e) 0)
         (:drag-target state))
    (let [drag-target (:drag-target state)
          svg-coords (get-svg-coordinates e)
          [new-x new-y] (normalise-coords svg-coords)]
      (.preventDefault e)
      (.setAttributeNS drag-target nil "x" new-x)
      (.setAttributeNS drag-target nil "y" new-y)
      (.setAttribute (:square-svg-element state) "class" (:original-square-class state))
      (assoc state
             :drag-target nil
             :square-svg-element nil
             :original-square-class nil
             :offset nil))

    (= (.-button e) 2)
    (do (.preventDefault e)
        state)

    :else
    state))

(defn handle-drag-event!
  [event-fn e]
  (let [state @drag-state
        new-state (event-fn e state)]
    (reset! drag-state new-state)))

;; TODO
;; DONE - Refactor drag event code to be more functional and use clojure data structures where ever possible
;; DONE - Highlight original square when moving piece
;; Validate that piece is moved to a legal square
;; Draw dots on legal squares when moving a piece
;; allow for highlighting squares
;; allow for drawing arrows
;; allow for moving piece by clicking instead of dragging
(defn chess-board [pieces]
  (let [svg [:svg {:id  "chess-board-svg"
                   :width "800"
                   :height "800"
                   :viewport "0 0 800 800"
                   :onMouseDown (partial handle-drag-event! grab-piece)
                   :onMouseMove (partial handle-drag-event! drag-piece)
                   :onMouseUp (partial handle-drag-event! drop-piece)
                   :onContextMenu #(.preventDefault %)}]
        board (map
               (fn [i]
                 (let [row (Math/floor (/ i 8))
                       column (rem i 8)
                       class (cond
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
