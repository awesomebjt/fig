(ns fig.core
  (:require [uncomplicate.clojurecl.core :refer :all]
            [uncomplicate.clojurecl.info :refer :all]
            [cljfx.api :as fx]

  )
  (:import [java.awt.image BufferedImage]
           [java.io File IOException InputStream]
           [javax.imageio ImageIO]
           [javafx.scene.image Image]
           )
)

(def HEIGHT 1000)
(def WIDTH 1000)
(def RANGE 3.0)
(def RANGEW 3.0)
(def RANGEH 3.0)
(def BASE-STEP (/ RANGE HEIGHT))
(def ZOOM 1)
(def CENTER {:x 0.0 :y -0.5})
;(def CENTER [0.26 0.378])
(def STEP (/ BASE-STEP ZOOM))
(def HMIN (- (:y CENTER) (/ STEP (/ 2 HEIGHT))))
(def HMAX (+ (:y CENTER) (/ STEP (/ 2 HEIGHT))))
(def WMIN (- (:x CENTER) (/ STEP (/ 2 WIDTH))))
(def WMAX (+ (:x CENTER) (/ STEP (/ 2 WIDTH))))
;; the extremity of the numbers we explore divided by the size of the graphic
(def WSTEP (/ (* 2 RANGEW) WIDTH))
(def HSTEP (/ (* 2 RANGEH) HEIGHT))
;; we initalize a 2d array of cells where each one contains an integer array representing its coordinates
(def PIXELS (into-array
              (map (fn [y] (into-array (map #(list % y) (range HMIN (- HMAX STEP) STEP))))
                   (range WMIN (- WMAX STEP) STEP))))

(def DEPTH 50)

;(def red-gradient (vec (map #(int %) (range 0xFF0000 0x000000 (* -1 (/ (- 0x0000FF 0x000000) DEPTH))))))
;(def green-gradient (vec (map #(int %) (range 0x00FF00 0x000000 (* -1 (/ (- 0x0000FF 0x000000) DEPTH))))))
;(def blue-gradient (vec (map #(int %) (range 0x0000FF 0x000066 (* -1 (/ (- 0x0000FF 0x000066) DEPTH))))))
(def red-green-gradient (vec (range 0x000100 0xFFFF00 0x000F00)))

(def RAINBOW (vec (concat
                            (vec (range 0xFFFFFF 0x00FFFF (* -1 0x0F0000))) ; white to blue-green
                            (vec (range 0x00FFFF 0x0000FF (* -1 0x000F00))) ; blue-green to blue
                            (vec (range 0x0000FF 0x000000 (* -1 0x00000F))) ; blue to black
                            ;(vec (range 0x0000FF 0x00FFFF 0x000100)) ; blue to blue-green
                            ;(vec (range 0x00FFFF 0x00FF00 (* -1 0x000001))) ; blue-green to green
                            ;(vec (range 0x00FF00 0xFFFF00 0x010000)) ; green to yellow
                            ;(vec (range 0xFFFF00 0xFF0000 (* -1 0x000100))) ; yellow to red
                            ;(vec (range 0xFF0000 0x000000 (* -1 0x010000))) ; red to black
                            ;(vec (range 0x000000 0xFFFFFF 0x010101)) ; black to white
                            )))
(def RRAINBOW (vec (reverse RAINBOW)))

;(def PLATFORM (first (platforms)))
;(def GPU (first (devices PLATFORM)))
;(def CTX (context [GPU]))
;(def GPU-ARRAY (cl-buffer CTX 1024 :read-write))
;
;(def QUEUE (command-queue CTX GPU-ARRAY PIXELS))


(defn cmul [[r1 i1] [r2 i2]]
  (let [r3 (+ (* r1 r2) (* i1 i2 -1))
        i3 (+ (* r1 i2) (* i1 r2))]
    [r3 i3]))

(defn cadd [[r1 i1] [r2 i2]]
  [(+ r1 r2) (+ i1 i2)])

(defn mbrot [z c]
  (cadd (cmul z z) c))

(defn recursive-mandelbrot [z c]
    (lazy-seq (cons z (recursive-mandelbrot (mbrot z c) c))))

(defn slope [[x1 y1] [x2 y2]]
  [(abs (- x1 x2)) (abs (- y1 y2))])

(defn final-slope [zs]
  (let [zs-reversed (reverse zs)
        p1 (second zs-reversed)
        p2 (first zs-reversed)]
    (slope p1 p2)))


(defn in-set [v]
                  (try (let [i (first v)
                        j (last v)]
                    (and (not (Double/isNaN i))
                         (not (Double/isNaN j))
                         (not (= ##Inf i))
                         (not (= ##Inf j))
                         (< i 2.0)
                         (< j 2.0)
                         ))
                       (catch Exception e false)))


(defn color-point [x y]
  (let [zs (take-while #(in-set %) (take DEPTH (recursive-mandelbrot [0 0] [x y])))
        z-count (count zs)
        is-in-set (= z-count DEPTH)
        ]
    (if is-in-set
      0x000000
      (if (nil? (get RRAINBOW z-count))
        0
        (get RRAINBOW z-count))
      )))

(defn color-row [row]
  (vec (pmap #(color-point (first %) (last %)) row)))


(defn process-image [pixel-array]
  (vec (pmap color-row pixel-array)))

(defn write-image [pixels file-path]
  (let [width (count pixels)
        height (count (first pixels))
        colored-pixels (process-image pixels)
        image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
    (doseq [x (range width)
            y (range height)]
      (.setRGB image x y (get-in colored-pixels [y x])))
    (try
      (let [output (File. file-path)]
        (ImageIO/write image "jpg" output))
      (catch IOException e
        (.printStackTrace e)))))

(defn draw-image [canvas zoom]
  (let [gc (.getGraphicsContext2D (:node canvas))]
    (doseq [x (range 0 (int (.getWidth canvas)))
            y (range 0 (int (.getHeight canvas)))]
      (.setFill gc 0) ;; Set fill color to black
      (.fillRect gc x y 1 1))))


;; Define application state

(def *state
  (atom {:title "Fig: the Fractal Image Generator"
         :zoom 1
         :center {:x 0 :y 0}}))

;; Define render functions

(defn title-input [{:keys [title]}]
  {:fx/type :text-field
   :on-text-changed #(swap! *state assoc :title %)
   :text title})
(defn zoom-input [{:keys [zoom]}]
  {:fx/type :text-field
   :on-text-changed #(swap! *state assoc :zoom %)
   :text zoom})

(defn root [{:keys [zoom title]}]
  (let [canvas {:fx/type :canvas :height 300 :width 300 :fill 0}]
    {:fx/type :stage
   :showing true
   :title title
   :scene {:fx/type :scene
           :root {:fx/type :h-box
                  :children [canvas
                             {:fx/type :v-box
                              :children [{:fx/type :h-box
                                          :children [
                                                     {:fx/type :label
                                                      :text "Zoom"}
                                                     {:fx/type zoom-input
                                                      :title zoom}
                                                     ]}
                                         {:fx/type :button
                                          :text "Render"
                                          :on-action (fn [_] (println (:node canvas)))}]}]}}}))

;; Create renderer with middleware that maps incoming data - description -
;; to component description that can be used to render JavaFX state.
;; Here description is just passed as an argument to function component.

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type root)))

;; Convenient way to add watch to an atom + immediately render app

(defn -main [& args]
  (fx/mount-renderer *state renderer)
  )