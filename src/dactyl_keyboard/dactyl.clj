(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))


(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

(def nrows 5)
(def ncols 6)


;;Code as of 2/20/19

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

;@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;;;;;;;Wrist rest;;;;;;;;;;
;@@@@@@@@@@@@@@@@@@@@@@@@@@
(def wrist-rest-on 1) 						;;0 for no rest 1 for a rest connection cut out in bottom case
(def wrist-rest-back-height 29)				;;height of the back of the wrist rest--Default 34
(def wrist-rest-angle 0) 				 ;;angle of the wrist rest--Default 20
(def wrist-rest-rotation-angle 0)			;;0 default The angle in counter clockwise the wrist rest is at
(def wrist-rest-ledge 3.5)					;;The height of ledge the silicone wrist rest fits inside
(def wrist-rest-y-angle 15)					;;0 Default.  Controls the wrist rest y axis tilt (left to right)

;;Wrist rest to case connections
(def right_wrist_connecter_x   (if (== ncols 5) 13 19))
(def left_wrist_connecter_x   (if (== ncols 5) -25 -10))
(def wrist_right_nut_y (if (== ncols 5) 10 20.5))
(def wrist_brse_position_x -1)

;@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;;;;;;;bottom cover;;;;;;;;;;
;@@@@@@@@@@@@@@@@@@@@@@@@@@
(def bottom-cover 1)						;;0= no cover  	1=cover


;@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;;;;;;;random;;;;;;;;;;
;@@@@@@@@@@-@@@@@@@@@@@@@@@@
;;;0= box 1=cherry 2= Alps
(def switch-type 0)

(def α (deg2rad 15))                        ;default 15 curvature of the columns
(def β (deg2rad 4))                        ;default 5 curvature of the rows
;(def centerrow  2)
(def centerrow (- nrows 3))             ;default 3 controls front-back tilt
(def centercol 4.5)                       ;default 3 controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ (* π 20) 180))            ;default 15 or, change this for more precise tenting control
(def column-style
  (if (> nrows 5) :orthographic :standard))  ; options include :standard, :orthographic, and :fixed

(defn column-offset [column]
  (cond (= column 2) [0 2.82 -4.5]
        (>= column 4) [0 -12 5.64]            ; original [0 -5.8 5.64]
        :else [0 0 0]))

(def thumb-offsets [10 -12 28])

(def keyboard-z-offset (if (>= nrows 5) 9 14))  ; default (> nrows 5) 9 14)----options include :standard, :orthographic, and :fixed)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 2.5)                   ; extra space between the base of keys; original= 2
(def extra-height 1)                  ; original= 0.5

(def wall-z-offset -5)                 ; length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset (if (> nrows 5) 5 7))  ; options include :standard, :orthographic, and :fixed)
                                           ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 2)                  ; wall thickness parameter; originally 5

;; Settings for column-style == :fixed
;; The defaults roughly match Maltron settings
;;   http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14) ;; Was 14.1, then 14.25
(def keyswitch-width 14);Nub side original 14.5 last 14.8----14.65 works for both.  box slightly loose

(def cherry-keyswitch-height 14) ;; Was 14.1, then 14.25
(def cherry-keyswitch-width 14)

(def alps-keyswitch-height 12.9) ;;12.9
(def alps-keyswitch-width 15.5)  ;;15.5
(def alps-width 15.55)		 	  ;;15.55
(def alps-notch-width 15.48)		;;15.4
(def alps-notch-height 1)			;;1
(def alps-height 12.85)				;;12.7

(def sa-profile-key-height 12.7)

(def plate-thickness 5)
(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))

(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) 1.5 plate-thickness)
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                       (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                   0
                                   (/ plate-thickness 2)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 plate-thickness)
                                 (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                             0
                                             (/ plate-thickness 2)]))))
        plate-half (union top-wall
                          left-wall)
        swap-holder (->> (cube (+ keyswitch-width 3) (/ (+ keyswitch-height 3) 2) 3)
                         (translate [0 (/ (+ keyswitch-height 3) 4) -1.5]))
        main-axis-hole (->> (cylinder (/ 4.0 2) 10)
                            (with-fn 12))
        plus-hole (->> (cylinder (/ 3.3 2) 10)
                       (with-fn 8)
                       (translate [-3.81 2.54 0]))
        minus-hole (->> (cylinder (/ 3.3 2) 10)
                        (with-fn 8)
                        (translate [2.54 5.08 0]))
        plus-hole-mirrored (->> (cylinder (/ 3.3 2) 10)
                                (with-fn 8)
                                (translate [3.81 2.54 0]))
        minus-hole-mirrored (->> (cylinder (/ 3.3 2) 10)
                                 (with-fn 8)
                                 (translate [-2.54 5.08 0]))
        friction-hole (->> (cylinder (/ 1.7 2) 10)
                           (with-fn 8))
        friction-hole-right (translate [5 0 0] friction-hole)
        friction-hole-left (translate [-5 0 0] friction-hole)
        hotswap-base-shape (->> (cube 19 6.2 3)
                                (translate [0 4 -2.6]))
        hotswap-holder (difference swap-holder
                                   main-axis-hole
                                   plus-hole
                                   minus-hole
                                   plus-hole-mirrored
                                   minus-hole-mirrored
                                   friction-hole-left
                                   friction-hole-right
                                   hotswap-base-shape)]
    (difference (union plate-half
                       (->> plate-half
                            (mirror [1 0 0])
                            (mirror [0 1 0]))
                       hotswap-holder))))

(def single-plate-rotated
  (let [top-wall (->> (cube (+ alps-keyswitch-height 3) 1.5 plate-thickness)
                      (translate [0
                                  (+ (/ 1.5 2) (/ alps-keyswitch-width 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube 2.8 (+ alps-keyswitch-width 3) plate-thickness)
                       (translate [(+ (/ 2.8 2) (/ alps-keyswitch-height 2))
                                   0
                                   (/ plate-thickness 2)]))

        plate-half (union top-wall left-wall )]
    (union plate-half
           (->> plate-half
                (mirror [1 0 0])
                (mirror [0 1 0])))))


;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap
  {1 (let [bl2 (/ 18.5 2)
           m (/ 17 2)
           key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                              (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                              (translate [0 0 0.05]))
                         (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                              (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                              (translate [0 0 6]))
                         (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                              (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                              (translate [0 0 12])))]
       (->> key-cap
            (translate [0 0 (+ 5 plate-thickness)])
            (color [220/255 163/255 163/255 1])))
   2 (let [bl2 (/ sa-double-length 2)
           bw2 (/ 18.25 2)
           key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                              (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                              (translate [0 0 0.05]))
                         (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                              (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                              (translate [0 0 12])))]
       (->> key-cap
            (translate [0 0 (+ 5 plate-thickness)])
            (color [127/255 159/255 127/255 1])))
   1.5 (let [bl2 (/ 18.25 2)
             bw2 (/ 28 2)
             key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                (translate [0 0 0.05]))
                           (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                (translate [0 0 12])))]
         (->> key-cap
              (translate [0 0 (+ 5 plate-thickness)])
              (color [240/255 223/255 175/255 1])))})

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 ncols))
(def rows (range 0 nrows))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))
(def column-base-angle (* β (- centercol 2)))

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [column-angle (* β (- centercol column))
        placed-shape (->> shape
                          (translate-fn [0 0 (- row-radius)])
                          (rotate-x-fn  (* α (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn  column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- row-radius)])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                (rotate-y-fn  column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))
        placed-shape-fixed (->> shape
                                (rotate-y-fn  (nth fixed-angles column))
                                (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                (translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 (+ row-radius (nth fixed-z column))])
                                (rotate-y-fn  fixed-tenting)
                                (translate-fn [0 (second (column-offset column)) 0]))]
    (->> (case column-style
          :orthographic placed-shape-ortho
          :fixed        placed-shape-fixed
                        placed-shape)
         (rotate-y-fn  tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
    (fn [angle obj] (rotate angle [1 0 0] obj))
    (fn [angle obj] (rotate angle [0 1 0] obj))
    column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))

(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [6] column)
                         (not= row lastrow) )]
           (->> single-plate
                (key-place column row)))))

(def caps
  (apply union
         (for [column columns
               row rows
               :when (not= row lastrow)]
           (->> (sa-cap (if (= column 5) 1 1))
                (key-place column row)))))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 3.5)
(def post-size 0.5)  ;Increased from .1 to .2 due to hole in top thumb cluster
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column (range 0 (dec ncols))
                row (range 0 lastrow)]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column (range 0 (dec ncols))
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl))))))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(def thumborigin
  (map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))

(defn thumb-tr-place [shape]
  (->> shape
       (rotate (deg2rad  -2) [1 0 0])
       (rotate (deg2rad -32) [0 1 0])
       (rotate (deg2rad  36) [0 0 1]) ; original 10
       (translate thumborigin)
       (translate [-18 1 5.5]))) ; original 1.5u  (translate [-12 -16 3])

; hackish
(defn thumb-unused-place [shape]
  (->> shape
       (translate thumborigin)
       (translate [8 1 -15])))

(defn thumb-tl-place [shape]
  (->> shape
       (rotate (deg2rad  -10) [1 0 0])
       (rotate (deg2rad -50) [0 1 0])
       (rotate (deg2rad  40) [0 0 1])
       (translate thumborigin)
       (translate [-27 -6 -5]))) ; original 1.5u (translate [-32 -15 -2])))

(defn thumb-mr-place [shape]
  (->> shape
       (rotate (deg2rad  -4) [1 0 0])
       (rotate (deg2rad -50) [0 1 0])
       (rotate (deg2rad  43) [0 0 1])
       (translate thumborigin)
       (translate [-16 -22 -1.5])))
(defn thumb-br-place [shape]
  (->> shape
       (rotate (deg2rad   -4) [1 0 0])
       (rotate (deg2rad -52) [0 1 0])
       (rotate (deg2rad  43) [0 0 1])
       (translate thumborigin)
       (translate [-25 -31 -16])))
(defn thumb-bl-place [shape]
  (->> shape
       (rotate (deg2rad   -12) [1 0 0])
       (rotate (deg2rad -52) [0 1 0])
       (rotate (deg2rad  42) [0 0 1])
       (translate thumborigin)
       (translate [-36 -14 -19]))) ;        (translate [-51 -25 -12])))

(defn thumb-1x-layout [shape]
  (union
   (thumb-mr-place shape)
   (thumb-br-place shape)
   (thumb-tl-place shape)
   (thumb-bl-place shape)))

(defn thumb-15x-layout [shape]
  (union
   (thumb-tr-place shape)))

(def larger-plate
  (let [plate-height (- (/ (- sa-double-length mount-height) 3) 0.5)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))]
    (union top-plate (mirror [0 1 0] top-plate))))

(def thumbcaps
  (union
   (thumb-1x-layout (sa-cap 1))
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1)))))

(def thumb
  (union
   (thumb-1x-layout single-plate)
   (thumb-15x-layout single-plate)))

(def thumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
(def thumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
(def thumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def thumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post))

(def thumb-post-bl-not-so-much
  (translate [(+ (/ mount-width -2) (* post-size 5))
              (- (/ mount-height -2) (* post-size 2))
              0]
             web-post))

(def thumb-connectors
  (union
   (triangle-hulls    ; top two
    (thumb-tl-place web-post-tr)
    (thumb-tl-place web-post-br)
    (thumb-tr-place thumb-post-tl)
    (thumb-tr-place thumb-post-bl))
   (triangle-hulls    ; bottom two
    (thumb-br-place web-post-tr)
    (thumb-br-place web-post-br)
    (thumb-mr-place web-post-tl)
    (thumb-mr-place web-post-bl))
   (triangle-hulls
    (thumb-mr-place web-post-tr)
    (thumb-mr-place web-post-br)
    (thumb-tr-place thumb-post-br))
   (triangle-hulls    ; between top row and bottom row
    (thumb-br-place web-post-tl)
    (thumb-bl-place web-post-bl)
    (thumb-br-place web-post-tr)
    (thumb-bl-place web-post-br)
    (thumb-mr-place web-post-tl)
    (thumb-tl-place web-post-bl)
    (thumb-mr-place web-post-tr)
    (thumb-tl-place web-post-br)
    (thumb-tr-place web-post-bl)
    (thumb-mr-place web-post-tr)
    (thumb-tr-place web-post-br))
   (triangle-hulls    ; top two to the middle two, starting on the left
    (thumb-tl-place web-post-bl)
    (thumb-tl-place web-post-tl)
    (thumb-bl-place web-post-tr)
    (thumb-tl-place web-post-bl)
    (thumb-bl-place web-post-br))
   (triangle-hulls    ; top two to the main keyboard, starting on the left
    (thumb-bl-place web-post-tl)
    (key-place 0 cornerrow web-post-bl)
    (thumb-tr-place web-post-tr)
    (key-place 0 cornerrow web-post-br)
    (thumb-tr-place thumb-post-tr)
    (key-place 1 cornerrow web-post-br)
    (thumb-tr-place web-post-tr)
    (thumb-unused-place web-post-tl)
    (thumb-tr-place web-post-br)
    (thumb-unused-place web-post-tl)
    (key-place 1 cornerrow web-post-br)
    (key-place 2 cornerrow web-post-bl)
    (thumb-unused-place web-post-tl)
    (key-place 2 cornerrow web-post-br)
    (key-place 3 cornerrow web-post-bl)
    #_(key-place 1 cornerrow web-post-br)
    #_(key-place 2 cornerrow web-post-bl)
    #_(key-place 2 cornerrow web-post-br)
    #_(thumb-tr-place web-post-br)
    #_(key-place 1 cornerrow web-post-br)
    #_(thumb-tr-place web-post-br)
    #_(key-place 2 lastrow web-post-tl)
    #_(thumb-unused-place web-post-tr)
    #_(key-place 2 lastrow thumb-post-bl)
    #_(key-place 2 lastrow thumb-post-bl-not-so-much)
    #_(thumb-mr-place web-post-br))
   #_(triangle-hulls
      (key-place 2 lastrow web-post-br)
      (key-place 3 lastrow web-post-bl)
      (key-place 2 lastrow web-post-tr)
      (key-place 3 lastrow web-post-tl)
      (key-place 3 cornerrow web-post-bl)
      (key-place 3 lastrow web-post-tr)
      (key-place 3 cornerrow web-post-br)
      (key-place 4 cornerrow web-post-bl))
   #_(triangle-hulls
      (key-place 1 cornerrow web-post-br)
      (key-place 2 lastrow web-post-tl)
      (key-place 2 cornerrow web-post-bl)
      (key-place 2 lastrow web-post-tr)
      (key-place 2 cornerrow web-post-br)
      (key-place 3 cornerrow web-post-bl))
   #_(triangle-hulls
      (key-place 3 lastrow web-post-tr)
      (key-place 3 lastrow web-post-br)
      (key-place 3 lastrow web-post-tr)
      (key-place 4 cornerrow web-post-bl))))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(def left-wall-x-offset 10)
(def left-wall-z-offset  3)

(defn left-key-position [row direction]
  (map -
      (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0])
      [left-wall-x-offset 0 left-wall-z-offset]) )

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))

(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness))
                            (* dy (+ wall-xy-offset wall-thickness))
                            wall-z-offset])

(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
    (hull
      (place1 post1)
      (place1 (translate (wall-locate1 dx1 dy1) post1))
      (place1 (translate (wall-locate2 dx1 dy1) post1))
      (place1 (translate (wall-locate3 dx1 dy1) post1))
      (place2 post2)
      (place2 (translate (wall-locate1 dx2 dy2) post2))
      (place2 (translate (wall-locate2 dx2 dy2) post2))
      (place2 (translate (wall-locate3 dx2 dy2) post2)))
    (bottom-hull
      (place1 (translate (wall-locate2 dx1 dy1) post1))
      (place1 (translate (wall-locate3 dx1 dy1) post1))
      (place2 (translate (wall-locate2 dx2 dy2) post2))
      (place2 (translate (wall-locate3 dx2 dy2) post2)))))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))

(def case-walls
  (union
   ; back wall
   (for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
   (for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
   (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
   ; right wall
   (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y       1 0 web-post-br))
   (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr))
   (key-wall-brace lastcol cornerrow 0 -1 web-post-br lastcol cornerrow 1 0 web-post-br)
   ; left wall
   (for [y (range 0 lastrow)]
     (union (wall-brace (partial left-key-place y 1)  -1 0 web-post
                        (partial left-key-place y -1) -1 0 web-post)
            (hull (key-place 0 y web-post-tl)
                  (key-place 0 y web-post-bl)
                  (left-key-place y  1 web-post)
                  (left-key-place y -1 web-post))))
   (for [y (range 1 lastrow)]
     (union (wall-brace (partial left-key-place (dec y) -1) -1 0 web-post
                        (partial left-key-place y  1) -1 0 web-post)
            (hull (key-place 0 y       web-post-tl)
                  (key-place 0 (dec y) web-post-bl)
                  (left-key-place y        1 web-post)
                  (left-key-place (dec y) -1 web-post))))
   (wall-brace (partial key-place 0 0) 0 1 web-post-tl
               (partial left-key-place 0 1) 0 1 web-post)
   (wall-brace (partial left-key-place 0 1) 0 1 web-post
               (partial left-key-place 0 1) -1 0 web-post)
   ; front wall
   (wall-brace (partial key-place 3 cornerrow) 0 -1 web-post-bl
               thumb-unused-place 0 -1 web-post-tl)
   #_(key-wall-brace lastcol 0 0 1 web-post-tr
                   lastcol 0 1 0 web-post-tr)
   (key-wall-brace 3 cornerrow   0 -1 web-post-bl
                   3 cornerrow 0.5 -1 web-post-br)
   (key-wall-brace 3 cornerrow 0.5 -1 web-post-br
                   4 cornerrow 0   -1 web-post-bl)
   #_(key-wall-brace 2 cornerrow   0 -1 web-post-bl
                   2 cornerrow 0.5 -1 web-post-br)
   (for [x (range 4 ncols)]
     (key-wall-brace x cornerrow 0 -1 web-post-bl
                     x cornerrow 0 -1 web-post-br))
   (for [x (range 5 ncols)]
     (key-wall-brace x       cornerrow 0 -1 web-post-bl
                     (dec x) cornerrow 0 -1 web-post-br))
    ; thumb walls
   (wall-brace thumb-unused-place 0 -1 web-post-tl
               thumb-tr-place  0 -1 web-post-br)
   (wall-brace thumb-mr-place  0 -1 web-post-br
               thumb-tr-place  0 -1 thumb-post-br)
   (wall-brace thumb-mr-place  0 -1 web-post-br
               thumb-mr-place  0 -1 web-post-bl)
   (wall-brace thumb-br-place  0 -1 web-post-br
               thumb-br-place  0 -1 web-post-bl)
   (wall-brace thumb-br-place -1  0 web-post-tl
               thumb-br-place -1  0 web-post-bl)
   (wall-brace thumb-bl-place -1  0 web-post-tl
               thumb-bl-place -1  0 web-post-bl)
  ;; thumb corners
   (wall-brace thumb-br-place -1  0 web-post-bl
               thumb-br-place  0 -1 web-post-bl)
   ; thumb tweeners
   (wall-brace thumb-mr-place  0 -1 web-post-bl
               thumb-br-place  0 -1 web-post-br)
   (wall-brace thumb-bl-place -1  0 web-post-bl
               thumb-br-place -1  0 web-post-tl)
  ;; clunky bit on the top left thumb connection  (normal connectors don't work well)
   (bottom-hull
    (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-bl-place (translate (wall-locate2 -1 0) web-post-tl))
    (thumb-bl-place (translate (wall-locate3 -1 0) web-post-tl)))
   (hull
    (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-bl-place (translate (wall-locate2 -1 0) web-post-tl))
    (thumb-bl-place (translate (wall-locate3 -1 0) web-post-tl))
    (thumb-bl-place web-post-tl))
   (hull
    (left-key-place cornerrow -1 web-post)
    (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
    (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-bl-place web-post-tl))
   (hull
    (left-key-place cornerrow -1 web-post)
    (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
    (key-place 0 cornerrow web-post-bl)
    (thumb-bl-place web-post-tl))))

  ;;### Case wall cutout ####
(def rj9-start  (map + [0 -3  0] (key-position -0.5 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def rj9-position  [(first rj9-start) (second rj9-start) 11])
(def rj9-cube   (cube 14.78 13 22.38))
(def rj9-space  (translate rj9-position rj9-cube))
(def rj9-holder (translate rj9-position
                  (difference rj9-cube
                              (union (translate [0 2 0] (cube 10.78  9 18.38))
                                     (translate [0 0 5] (cube 10.78 13  5))))))

(def aviator-start (map + [0 -5  0] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def aviator-position [(first aviator-start) (second aviator-start) 11])
(def aviator-hole (translate aviator-position
                  (rotate (deg2rad 90) [1 0 0]
                  (rotate (deg2rad 45) [0 1 0]
                  (translate [-12.5 0 0]
                  (cylinder (/ 12 2) 20))))))

(def original_usb_holder_position (key-position 1 0 (map + (wall-locate2 0 1) [0 (/ mount-height 2) 0])))
(def original_usb_holder_size [6.5 10.0 13.6])
(def original_usb_holder_thickness 4)
(def original_usb_holder
    (->> (cube (+ (first original_usb_holder_size) original_usb_holder_thickness) (second original_usb_holder_size) (+ (last original_usb_holder_size) original_usb_holder_thickness))
         (translate [(first original_usb_holder_position) (second original_usb_holder_position) (/ (+ (last original_usb_holder_size) original_usb_holder_thickness) 2)])))
(def original_usb_holder_hole
    (->> (apply cube original_usb_holder_size)
         (translate [(first original_usb_holder_position) (second original_usb_holder_position) (/ (+ (last original_usb_holder_size) original_usb_holder_thickness) 2)])))


(def usb-holder-position (key-position 1 1 (map + (wall-locate1 -2 (- 4.9 (* 0.2 nrows))) [0 (/ mount-height 2) 0])))

(def usb-holder-size [5.5 33.65 19 ])	;;5.5 33.34 18.4
(def usb-hole-size [9.5 33.65 19  ]) ;;9.5 33.34 18.4
#_(def usb-holder-size [5.5 27.8 23.4 ])	;;Dimensions for adafruit feather
#_(def usb-hole-size [9.5 33.34 23.4  ])  ;Dimensions for adafruit feather
(def usb-hole-size-left [9.5 35.6 8.0 ]) ;;9.5 35.6 8.0
(def usb-hole-size-right [6 35.6 10.0 ]) ;;6 35.6 10.0
(def usb-holder-thickness 5)
#_(def usb-holder
  (->> (difference
        (cube (+ (first usb-holder-size) usb-holder-thickness)
              (+ (second usb-holder-size) usb-holder-thickness)
              (+ (last usb-holder-size) usb-holder-thickness))
        (translate [(first usb-holder-position)
                    (second usb-holder-position)
                    (/ (+ (last usb-holder-size) usb-holder-thickness) 2)]))))
(def usb-holder
  (->> (difference
        (cube (+ (first usb-holder-size) usb-holder-thickness)
              (+ (second usb-holder-size) usb-holder-thickness)
              (+ (last usb-holder-size) usb-holder-thickness)))
       (translate [(first usb-holder-position)
                   (second usb-holder-position)
                   (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))


(def usb-holder-hole
    (->>
		(union
		(->>(apply cube usb-hole-size)
         (translate [(+ (first usb-holder-position ) 2) (second usb-holder-position) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)]))
		 (->>(apply cube usb-hole-size-left)
         (translate [(+ (first usb-holder-position ) 2) (- (second usb-holder-position) 10) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)]))
		 (->>(apply cube usb-hole-size-right)
         (translate [(+ (first usb-holder-position ) 2) (+ (second usb-holder-position) 10) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))))

(def trrs-holder-position (key-position 1.25 1.3 (map + (wall-locate1 0 (+ 7.8 (* 0.13 nrows))) [0 (/ mount-height 2) 0])))
(def trrs-holder-size [7.4 13.6 10.3 ])
(def trrs-hole-size [7.4 14.6 10.3 ])

(def trrs-hole-size-back [7. 6. 4. ])
(def trrs-hole-size-right [3.35 10])
(def trrs-holder-thickness 4)
(def trrs-holder
  (->> (difference (cube (+ (first trrs-holder-size) trrs-holder-thickness) (+ (second trrs-holder-size) trrs-holder-thickness) (+ (last trrs-holder-size) trrs-holder-thickness) )
		(->>(->> (cube 25 50 5)(rotate (/ 1.5708 -2) [1 0 0]))(translate [2 (+ 5 nrows) 5]))
  )
	  (translate [(first trrs-holder-position) (second trrs-holder-position) (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2)])
))

(def trrs-holder-hole
  (->>
   (union (->> (apply cube trrs-hole-size)
               (translate [(- (first trrs-holder-position ) 4.2) (second trrs-holder-position) (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2)]))
          (->> (apply cube trrs-hole-size-back)
               (translate [(- (first trrs-holder-position ) 4.1) (- (second trrs-holder-position) 6.5) (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2)]))
          (->> (apply   cylinder trrs-hole-size-right)
               (rotate 1.5708 [1 0 0])
               (translate [(- (first trrs-holder-position ) 3.7) (+ (second trrs-holder-position) 10) (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2)])))))

(def teensy-width 20)
(def teensy-height 12)
(def teensy-length 33)
(def teensy2-length 53)
(def teensy-pcb-thickness 2)
(def teensy-holder-width  (+ 7 teensy-pcb-thickness))
(def teensy-holder-height (+ 6 teensy-width))
(def teensy-offset-height 5)
(def teensy-holder-top-length 18)
(def teensy-top-xy (key-position 0 (- centerrow 1) (wall-locate3 -1 0)))
(def teensy-bot-xy (key-position 0 (+ centerrow 1) (wall-locate3 -1 0)))
(def teensy-holder-length (- (second teensy-top-xy) (second teensy-bot-xy)))
(def teensy-holder-offset (/ teensy-holder-length -2))
(def teensy-holder-top-offset (- (/ teensy-holder-top-length 2) teensy-holder-length))

(def teensy-holder
    (->>
        (union
          (->> (cube 3 teensy-holder-length (+ 6 teensy-width))
               (translate [1.5 teensy-holder-offset 0]))
          (->> (cube teensy-pcb-thickness teensy-holder-length 3)
               (translate [(+ (/ teensy-pcb-thickness 2) 3) teensy-holder-offset (- -1.5 (/ teensy-width 2))]))
          (->> (cube 4 teensy-holder-length 4)
               (translate [(+ teensy-pcb-thickness 5) teensy-holder-offset (-  -1 (/ teensy-width 2))]))
          (->> (cube teensy-pcb-thickness teensy-holder-top-length 3)
               (translate [(+ (/ teensy-pcb-thickness 2) 3) teensy-holder-top-offset (+ 1.5 (/ teensy-width 2))]))
          (->> (cube 4 teensy-holder-top-length 4)
               (translate [(+ teensy-pcb-thickness 5) teensy-holder-top-offset (+ 1 (/ teensy-width 2))])))
        (translate [(- teensy-holder-width) 0 0])
        (translate [-1.4 0 0])
        (translate [(first teensy-top-xy)
                    (- (second teensy-top-xy) 1)
                    (/ (+ 6 teensy-width) 2)])))

(def wire-post-height 7)
(def wire-post-overhang 3.5)
(def wire-post-diameter 2.6)
(defn wire-post [direction offset]
   (->> (union (translate [0 (* wire-post-diameter -0.5 direction) 0] (cube wire-post-diameter wire-post-diameter wire-post-height))
               (translate [0 (* wire-post-overhang -0.5 direction) (/ wire-post-height -2)] (cube wire-post-diameter wire-post-overhang wire-post-diameter)))
        (translate [0 (- offset) (+ (/ wire-post-height -2) 3) ])
        (rotate (/ α -2) [1 0 0])
        (translate [3 (/ mount-height -2) 0])))

(def wire-posts
  (union
     ;(thumb-ml-place (translate [-5 0 -2] (wire-post  1 0)))
     ;(thumb-ml-place (translate [ 0 0 -2.5] (wire-post -1 6)))
     ;(thumb-ml-place (translate [ 5 0 -2] (wire-post  1 0)))
     (for [column (range 0 lastcol)
           row (range 0 cornerrow)]
       (union
        (key-place column row (translate [-5 0 0] (wire-post 1 0)))
        (key-place column row (translate [0 0 0] (wire-post -1 6)))
        (key-place column row (translate [5 0 0] (wire-post  1 0)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;Plate mount ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn screw-insert-shape [bottom-radius top-radius height]
   (union  (cylinder [bottom-radius top-radius] height)(with-fn 55)
          (translate [0 0 (/ height 2)] (sphere top-radius))))

(defn plate-screw-insert-shape [bottom-radius height]
   (union  (cylinder bottom-radius height)(with-fn 55)))


(defn screw-insert [column row bottom-radius top-radius height]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                       (if shift-down  (key-position column row (map - (wall-locate2  0 -1) [0 (/ mount-height 2) 0]))
                        (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                                       (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0]))))) ]
    (if (>  top-radius 0.1)
      (->> (screw-insert-shape bottom-radius top-radius height)
           (translate [(first position) (second position) (/ height 2)]))
      (if (<  top-radius 0.1)
        (->> (plate-screw-insert-shape bottom-radius height)
             (translate [(first position) (second position) (/ height 2)]))))))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union #_(->> (screw-insert 0 0         bottom-radius top-radius height) (translate [6 -5 3.6]));;top left
         #_(->> (screw-insert 0 lastrow   bottom-radius top-radius height)(translate [24 0 3.6]))	;;left bottom
         #_(->> (screw-insert lastcol (+ lastrow 0.0)  bottom-radius top-radius (- height 0))(translate [-7 19.5 3.2]));;right bottom
         #_(->> (screw-insert 3 0         bottom-radius top-radius height)(translate [0 -5.5 3.6]));;middle back
         #_(->> (screw-insert lastcol 0   bottom-radius top-radius height)(translate [-3 6 3.6]))))

(def screw-insert-height 4.)		;;Was 5.8
(def screw-insert-bottom-radius (/ 4.6 2));;controls the cutout size
(def screw-insert-top-radius (/ 4 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius (+ screw-insert-height 4.4)))
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 3.6) (+ screw-insert-top-radius 2.1) (+ screw-insert-height 1.1)))

(def model-right
  (difference
   (union
    key-holes
    connectors
    thumb
    thumb-connectors
    rj9-holder
    (difference
     (union case-walls
            usb-holder)
     usb-holder-hole
     rj9-space))
   (translate [0 0 -20] (cube 350 350 40))))

(spit "things/right.scad"
      (write-scad model-right))

(spit "things/left.scad" (write-scad (mirror [-1 0 0] model-right)))


(defn -main [dum] 1)  ; dummy to make it easier to batch
