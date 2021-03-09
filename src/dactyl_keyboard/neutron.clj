(ns dactyl-keyboard.neutron
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [dactyl-keyboard.common :refer :all]))

(defn columns
  [ncols]
  (range 0 ncols))

(defn rows
  [nrows]
  (range 0 nrows))

(defn key-place
  [c column row shape]
  (let [hand-separation (+ 72 (/ (get c :configuration-hand-separation 0) 2))
        hand-rotation   (get c :configuration-hand-rotation 8)]
    ( ->> (apply-key-geometry c
                              translate
                              (fn [angle obj] (rotate angle [1 0 0] obj))
                              (fn [angle obj] (rotate angle [0 1 0] obj))
                              column row shape)
      (translate [hand-separation 0 0])
      (rotate (deg2rad hand-rotation) [0 0 1]) )))

(defn key-place-mirror
  [c column row shape]
  (->> (key-place c column row shape)
       (mirror [1 0 0])))

(defn key-holes
  "determines which keys should be generated based on the configuration."
  [c]
  (let [inner               (get c :configuration-inner-column)
        ncols               (get c :configuration-ncols)
        nrows               (get c :configuration-nrows)
        hide-last-pinky?    (get c :configuration-hide-last-pinky?)
        last-row-count      (get c :configuration-last-row-count)
        lastrow             (flastrow nrows)
        lastcol             (flastcol ncols)
        cornerrow           (fcornerrow nrows)
        last-pinky-location (fn [column row]
                              (and (= row lastrow)
                                   (= column lastcol)))
        hide-pinky          (fn [column row]
                              (not (and (= last-row-count :full)
                                        hide-last-pinky?
                                        (last-pinky-location column row))))]
    (apply union
           (for [column (columns ncols)
                 row    (rows nrows)
                 :when  (case last-row-count
                          :zero (not= row lastrow)
                          :two (or (.contains [2 3] column)
                                   (not= row lastrow))
                          :full (or (not (.contains [0 1] column)) (not= row lastrow)))
                 :when  (hide-pinky column row)
                 :when  (case inner
                          :outie (not (and (= column -1)
                                           (<= cornerrow row)))
                          true)]
             (->> (single-plate c)
                  (color [1 1 0])
                  (key-place c column row))))))

(defn wide-post-tr [use-wide-pinky?]
  (if use-wide-pinky?
    (translate [(- (/ mount-width  1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post)
    web-post-tr))
(defn wide-post-tl [use-wide-pinky?]
  (if use-wide-pinky?
    (translate [(+ (/ mount-width -1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post)
    web-post-tl))
(defn wide-post-bl [use-wide-pinky?]
  (if use-wide-pinky?
    (translate [(+ (/ mount-width -1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post)
    web-post-bl))
(defn wide-post-br [use-wide-pinky?]
  (if use-wide-pinky?
    (translate [(- (/ mount-width  1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post)
    web-post-br))

(defn connectors
  "it creates the wall which connects to each keys in the main body based
   on the configuration provided."
  [c]
  (let [inner               (get c :configuration-inner-column)
        init                (case inner
                              :outie -1
                              :normie 0
                              :innie 1)
        last-row-count      (get c :configuration-last-row-count)
        ncols               (get c :configuration-ncols)
        nrows               (get c :configuration-nrows)
        hide-last-pinky?    (get c :configuration-hide-last-pinky?)
        lastrow             (flastrow nrows)
        cornerrow           (fcornerrow nrows)
        middlerow           (fmiddlerow nrows)
        lastcol             (flastcol ncols)
        last-pinky-location (fn [column row]
                              (and (= row lastrow)
                                   (= column lastcol)))
        hide-pinky          (fn [column row]
                              (not (and (= last-row-count :full)
                                        hide-last-pinky?
                                        (last-pinky-location column row))))
        pinky-blocker (triangle-hulls (key-place c lastcol lastrow web-post-tr)
                                      (key-place c lastcol lastrow web-post-tl)
                                      (key-place c lastcol lastrow web-post-br)
                                      (key-place c lastcol lastrow web-post-bl))
        ]
    (union
     (apply
      union
      (if-not (hide-pinky lastcol lastrow)
        pinky-blocker
        ())
      (concat
      ;; row connections
       (for [column (range init (dec ncols))
             row    (range 0 (inc lastrow))
             :when  (case last-row-count
                      :zero (or (not= row lastrow)
                                (and (= row cornerrow)
                                     (= column -1)))
                      :two (or (.contains [2] column)
                               (not= row lastrow))
                      :full (not (and (= row lastrow)
                                      (.contains [-1 0 1] column))))]
         (triangle-hulls
          (key-place c (inc column) row web-post-tl)
          (key-place c column row web-post-tr)
          (key-place c (inc column) row web-post-bl)
          (if (not (and (= column -1)
                        (= row cornerrow)))
            (key-place c column row web-post-br)
            ())))

      ;; column connections
       (for [column (columns ncols)
             row    (range 0 lastrow)
             :when  (case last-row-count
                      :zero (not= row cornerrow)
                      :two (or (not= row cornerrow))
                      :full (not (and (= row cornerrow)
                                      (.contains [-1 0 1] column))))]
         (triangle-hulls
          (key-place c column row web-post-br)
          (key-place c column row web-post-bl)
          (key-place c column (inc row) web-post-tr)
          (if (not (and (= column -1)
                        (= row middlerow)))
            (key-place c column (inc row) web-post-tl)
            ())))

      ;; diagonal connections
       (for [column (range init (dec ncols))
             row    (range 0 lastrow)
             :when  (case last-row-count
                      :full (not (or (and (= row lastrow)
                                          (.contains [-1 0 1] column))
                                     (and (= row cornerrow)
                                          (.contains [-1 0 1] column))))
                      (or (not= row cornerrow)))]
         (triangle-hulls
          (key-place c column row web-post-br)
          (key-place c column (inc row) web-post-tr)
          (key-place c (inc column) row web-post-bl)
          (key-place c (inc column) (inc row) web-post-tl)))))
     (case last-row-count
       :two (triangle-hulls (key-place c 2 lastrow   web-post-tr)
                            (key-place c 3 cornerrow web-post-bl)
                            (key-place c 3 lastrow   web-post-tl)
                            (key-place c 3 cornerrow web-post-br)
                            (key-place c 3 lastrow   web-post-tr)
                            (key-place c 4 cornerrow web-post-bl)
                            (key-place c 3 lastrow   web-post-br))
       ()))))

(defn hanging-wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (hull
   (place1 post1)
   (place1 (translate (wall-locate1 dx1 dy1) post1))
   (place1 (translate (wall-locate2 dx1 dy1) post1))
   (place1 (translate (wall-locate3 dx1 dy1) post1))
   (place2 post2)
   (place2 (translate (wall-locate1 dx2 dy2) post2))
   (place2 (translate (wall-locate2 dx2 dy2) post2))
   (place2 (translate (wall-locate3 dx2 dy2) post2))))

(defn raising-wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (bottom-hull
   (place1 (translate (wall-locate2 dx1 dy1) post1))
   (place1 (translate (wall-locate3 dx1 dy1) post1))
   (place2 (translate (wall-locate2 dx2 dy2) post2))
   (place2 (translate (wall-locate3 dx2 dy2) post2))))

(defn wall-brace
  [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
   (hanging-wall-brace place1 dx1 dy1 post1 place2 dx2 dy2 post2)
   (raising-wall-brace place1 dx1 dy1 post1 place2 dx2 dy2 post2)))

(defn key-wall-brace [c x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place c x1 y1) dx1 dy1 post1
              (partial key-place c x2 y2) dx2 dy2 post2))

(defn right-wall [c]
  (let [row-count       (get c :configuration-last-row-count)
        use-wide-pinky? (get c :configuration-use-wide-pinky?)
        lastcol         (flastcol (get c :configuration-ncols))
        lastrow         (flastrow (get c :configuration-nrows))
        cornerrow       (fcornerrow (get c :configuration-nrows))]
    (union (key-wall-brace c
                           lastcol 0 0 1 (wide-post-tr use-wide-pinky?)
                           lastcol 0 1 0 (wide-post-tr use-wide-pinky?))
           (for [y (range 0 lastrow)]
             (key-wall-brace c
                             lastcol y 1 0 (wide-post-tr use-wide-pinky?)
                             lastcol y 1 0 (wide-post-br use-wide-pinky?)))
           (case row-count
             :full (key-wall-brace c
                                   lastcol lastrow 1 0 (wide-post-tr use-wide-pinky?)
                                   lastcol lastrow 1 0 (wide-post-br use-wide-pinky?))
             ())
           (for [y (range 1 lastrow)]
             (key-wall-brace c
                             lastcol (dec y) 1 0 (wide-post-br use-wide-pinky?)
                             lastcol y 1 0 (wide-post-tr use-wide-pinky?)))
           (case row-count
             :full (key-wall-brace c
                                   lastcol (dec lastrow) 1 0 (wide-post-br use-wide-pinky?)
                                   lastcol lastrow       1 0 (wide-post-tr use-wide-pinky?))
             ())
           (key-wall-brace c
                           lastcol (case row-count :full lastrow cornerrow) 0 -1 (wide-post-br use-wide-pinky?)
                           lastcol (case row-count :full lastrow cornerrow) 1  0 (wide-post-br use-wide-pinky?)))))

(defn back-wall [c]
  (let [ncols             (get c :configuration-ncols)
        lastcol           (flastcol ncols)
        inner             (get c :configuration-inner-column)
        init              (case inner
                            :outie -1
                            :normie 0
                            :innie 1)]
    (union
     (for [x (range init ncols)]
       (key-wall-brace c x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
     (for [x (range (+ init 1) ncols)]
       (key-wall-brace c x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
     (key-wall-brace c lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr))))

(defn front-wall [c]
  (let [ncols         (get c :configuration-ncols)
        nrows         (get c :configuration-nrows)
        lastrow       (flastrow nrows)
        cornerrow     (fcornerrow nrows)
        row-count     (get c :configuration-last-row-count)
       #_ thumb-tr-post #_(if (= (get c :configuration-thumb-count) :five) web-post-br thumb-post-br)]
    (union
     #_(wall-brace (partial thumb-tr-place c)  0 -1 thumb-tr-post
                 (partial (partial key-place c) 3 (case row-count :zero cornerrow lastrow))  0 -1 web-post-bl)
     (key-wall-brace c
                     2 (case row-count :zero cornerrow lastrow) 0   -1 web-post-bl
                     2 (case row-count :zero cornerrow lastrow) 0.5 -1 web-post-br)
     (key-wall-brace c
                     2 (case row-count :zero cornerrow lastrow)   0.5 -1 web-post-br
                     3 (case row-count :full lastrow   cornerrow) 0   -1 web-post-bl)
     (for [x (range 3 ncols)]
       (key-wall-brace c
                       x (case row-count :full lastrow cornerrow) 0 -1 web-post-bl
                       x (case row-count :full lastrow cornerrow) 0 -1 web-post-br))
     (for [x (range 4 ncols)]
       (key-wall-brace c
                       x       (case row-count :full lastrow cornerrow) 0 -1 web-post-bl
                       (dec x) (case row-count :full lastrow cornerrow) 0 -1 web-post-br)))))

(defn walls [c]
  (union (back-wall c)
         (right-wall c)
         (front-wall c)))

(defn bridge [c]
  (let [nrows (get c :configuration-nrows)]
    (union
     (wall-brace (partial key-place c 0 0)        0 1 web-post-tl
                 (partial key-place-mirror c 0 0) 0 1 web-post-tl)
     (for [row (range 0 (dec nrows))]
       (hull (key-place c 0 row web-post-tl)
             (key-place c 0 row web-post-bl)
             (key-place-mirror c 0 row web-post-bl)
             (key-place-mirror c 0 row web-post-tl)))
     (for [row (range 1 (dec nrows))]
       (hull (key-place c 0 row web-post-tl)
             (key-place c 0 (dec row) web-post-bl)
             (key-place-mirror c 0 (dec row) web-post-bl)
             (key-place-mirror c 0 row web-post-tl))))))

(defn model-right [c]
  (union (key-holes c)
         (connectors c)
         (walls c)))

(defn model [c]
  (union (model-right c)
         (bridge c)
         (mirror [1 0 0] (model-right c))))

(def c {:configuration-nrows                  6
        :configuration-ncols                  6
        :configuration-thumb-count            :five
        :configuration-last-row-count         :full
        :configuration-switch-type            :box
        :configuration-inner-column           :normie
        :configuration-hide-last-pinky?       false

        :configuration-alpha                  (/ pi 10)
        :configuration-pinky-alpha            (/ pi 10)
        :configuration-beta                   (/ pi 36)
        :configuration-centercol              3
        :configuration-tenting-angle          (/ pi 12)
        :configuration-rotate-x-angle         (/ pi 180)
        :configuration-hand-rotation          10
        :configuration-hand-separation        20

        :configuration-use-promicro-usb-hole? false
        :configuration-use-trrs?              false
        :configuration-use-external-holder?   false

        :configuration-use-hotswap?           false
        :configuration-thumb-offset-x         1
        :configuration-thumb-offset-y         -3
        :configuration-thumb-offset-z         7
        :configuration-custom-thumb-tenting?  false
        :configuration-custom-thumb-tenting-x (/ pi 0.5)
        :configuration-custom-thumb-tenting-y (/ pi 0.5)
        :configuration-custom-thumb-tenting-z (/ pi 0.5)
        :configuration-stagger?               true
        :configuration-stagger-index          [0 0 0]
        :configuration-stagger-middle         [0 2.8 -6.5]
        :configuration-stagger-ring           [0 0 -3]
        :configuration-stagger-pinky          [0 -13 6]
        :configuration-use-wide-pinky?        false
        :configuration-z-offset               18
        :configuration-use-wire-post?         false
        :configuration-use-screw-inserts?     false

        :configuration-show-caps?             false
        :configuration-plate-projection?      false})

(spit "things/neutron.scad"
      (write-scad (model c)))
