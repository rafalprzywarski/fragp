(ns fragp.core
  (:import (java.io File)
           (java.awt.image BufferedImage)
           (javax.imageio ImageIO))
  (:gen-class))


(defn load-bytes [filename]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream filename) out)
    (into [] (map #(if (< % 0) (+ 256 %) (+ 0 %))) (.toByteArray out))))


(defn load-uint16 [bytes off]
  (+ (bytes off)
     (bit-shift-left (bytes (+ off 1)) 8)))


(defn load-uint32 [bytes off]
  (+ (bytes off)
     (bit-shift-left (bytes (+ off 1)) 8)
     (bit-shift-left (bytes (+ off 2)) 16)
     (bit-shift-left (bytes (+ off 3)) 24)))


(defn load-string8 [bytes off]
  (apply str (map char (take 8 (drop off bytes)))))


(defn parse-entities [bytes]
  (loop [off 0
         entities {}]
    (if (>= off (count bytes))
      entities
      (let [id (load-string8 bytes off)
            size (load-uint32 bytes (+ off 12))]
        (recur
         (+ off 32 size)
         (assoc
          entities
          id {:id id,
              :unknown1 (subvec bytes (+ off 8) (+ off 12)),
              :unknown2 (subvec bytes (+ off 16) (+ off 32)),
              :bytes (subvec bytes (+ off 32) (+ off 32 size))}))))))


(defn parse-picture [entity]
  (let [bytes (:bytes entity)
        width (load-uint16 bytes 12)
        height (load-uint16 bytes 14)]
    {:width width
     :height height
     :indices (subvec bytes 16)}))


(defn parse-palette [entity]
  (vec (map vec (partition 3 (:bytes entity)))))


(defn parse-graphics [entities]
  (let [picture (parse-picture (entities "GRAPHICS"))
        palette (parse-palette (entities "PALETTE "))]
    (assoc picture :palette palette)))


(defn load-graphics [filename]
  (->> filename
       (load-bytes)
       (parse-entities)
       (parse-graphics)))


(defn scale-val [s v]
  (max 0 (min 255 (int (* s v)))))


(defn scale-rgb [s rgb]
  (into [] (map #(scale-val s %)) rgb))


(defn scale-palette [scale palette]
  (into [] (map #(scale-rgb scale %)) palette))


(defn adjust-brightness [image scale]
  (update image :palette #(scale-palette scale %)))


(defn save-picture-as-png! [{:keys [width height indices palette]} ^String filename]
  (let [img ^BufferedImage (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [index (indices (+ x (* y width)))
              rgb (get palette index)
              rgb (if rgb
                    (bit-or (rgb 2)
                            (bit-shift-left (rgb 1) 8)
                            (bit-shift-left (rgb 0) 16)
                            (bit-shift-left 0xff 24))
                    0)]
          (.setRGB img x y (unchecked-int rgb)))))
    (ImageIO/write img "png" (File. filename))))


(defn convert-256 [infile outfile]
  (-> (load-graphics infile)
      (adjust-brightness 5)
      (save-picture-as-png! outfile)))


(defn decode-sprite [bytes width total]
  (loop [bytes bytes
         size 0
         out (transient [])]
    (if (or (= size total)
            (empty? bytes))
      (persistent! (reduce conj! out (repeat (- total size) nil)))
      (let [nfill (first bytes)
            npixels (second bytes)
            nfill (if (and (zero? nfill) (zero? npixels))
                    (mod (- size) width)
                    nfill)]
        (recur (subvec bytes (+ npixels 2))
               (+ size nfill npixels)
               (reduce conj!
                       out
                       (concat (repeat nfill nil)
                               (take npixels (drop 2 bytes)))))))))


(defn parse-sprite [n entity]
  (let [bytes (:bytes entity)
        off0 (load-uint32 bytes (+ 16 12 (* n 16)))
        width (load-uint16 bytes (+ 16 8 (* n 16)))
        height (load-uint16 bytes (+ 16 10 (* n 16)))]
    {:width width
     :height height
     :indices (decode-sprite (subvec bytes off0)
                             width
                             (* width height))}))


(defn parse-sprites [entity]
  (let [bytes (:bytes entity)
        n (load-uint16 bytes 8)]
    (map #(parse-sprite % entity) (range n))))


(defn parse-graphics-sprite [n entities]
  (let [sprite (parse-sprite n (entities "GRAPHICS"))
        palette (parse-palette (entities "PALETTE "))]
    (assoc sprite :palette palette)))


(defn parse-graphics-sprites [entities]
  (let [sprites (parse-sprites (entities "GRAPHICS"))
        palette (parse-palette (entities "PALETTE "))]
    (map #(assoc % :palette palette) sprites)))


(defn load-graphics-sprite [filename n]
  (->> filename
       (load-bytes)
       (parse-entities)
       (parse-graphics-sprite n)))


(defn load-graphics-sprites [filename]
  (->> filename
       (load-bytes)
       (parse-entities)
       (parse-graphics-sprites)))


(defn convert-sprite [infile n outfile]
  (-> (load-graphics-sprite infile n)
      (adjust-brightness 5)
      (save-picture-as-png! outfile)))


(defn convert-sprites [infile outfile]
  (let [sprites (load-graphics-sprites infile)]
    (loop [sprites sprites
           i 0]
      (when sprites
        (save-picture-as-png!
         (adjust-brightness (first sprites) 5)
         (.replace outfile "#" (str i)))
        (recur (next sprites) (inc i))))))


(defn cajji []
  (convert-256 "/Volumes/Untitled/_INTRO/CAJJI.256" 0 "cajji.png"))


(defn c1 []
  (convert-sprites "/Volumes/Untitled/_C1/999.256" "c1_#.png"))


(defn c2 []
  (convert-sprites "/Volumes/Untitled/_C2/999.256" "c2_#.png"))


(defn c3 []
  (convert-sprites "/Volumes/Untitled/_C3/999.256" "c3_#.png"))


(defn c4 []
  (convert-sprites "/Volumes/Untitled/_C4/999.256" "c4_#.png"))


(defn c5 []
  (convert-sprites "/Volumes/Untitled/_C5/999.256" "c5_#.png"))


(defn c6 []
  (convert-sprites "/Volumes/Untitled/_C6/999.256" "c6_#.png"))


(defn fe-dome []
  (convert-sprites "/Volumes/Untitled/_MS/FE_DOME.256" "fe_dome_#.png"))


(defn fe-base []
  (convert-sprites "/Volumes/Untitled/_MS/FE_BASE.256" "fe_base_#.png"))


(defn fe-edge []
  (convert-sprites "/Volumes/Untitled/_MS/FE_EDGE.256" "fe_edge_#.png"))


(defn xb00 []
  (convert-sprites "/Volumes/Untitled/_P/XB00.256" "xb00_#.png"))


(defn edgemenu []
  (convert-sprites "/Volumes/Untitled/_MS/EDGEMENU.256" "edgemenu_#.png"))


(defn orange []
  (convert-sprites "/Volumes/Untitled/_MS/ORANGE.256" "orange_#.png"))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
