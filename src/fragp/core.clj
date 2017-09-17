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
  (let [img ^BufferedImage (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [index (indices (+ x (* y width)))
              rgb (palette index)
              rgb (bit-or (rgb 2) (bit-shift-left (rgb 1) 8) (bit-shift-left (rgb 0) 16))]
          (.setRGB img x y rgb))))
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
      (persistent! (reduce conj! out (repeat (- total size) 0)))
      (let [nfill (first bytes)
            npixels (second bytes)
            nfill (if (and (zero? nfill) (zero? npixels))
                    (mod (- size) width)
                    nfill)]
        (recur (subvec bytes (+ npixels 2))
               (+ size nfill npixels)
               (reduce conj!
                       out
                       (concat (repeat nfill 0)
                               (take npixels (drop 2 bytes)))))))))


(defn parse-sprite [n entity]
  (let [bytes (:bytes entity)
        off0 (load-uint32 bytes (+ 16 12 (* n 16)))
        width (load-uint16 bytes (+ 16 8 (* n 16)))
        height (load-uint16 bytes (+ 16 10 (* n 16)))]
    (prn off0 width height)
    {:width width
     :height height
     :indices (decode-sprite (subvec bytes off0)
                             width
                             (* width height))}))


(defn parse-graphics-sprite [n entities]
  (let [sprite (parse-sprite n (entities "GRAPHICS"))
        palette (parse-palette (entities "PALETTE "))]
    (assoc sprite :palette palette)))


(defn load-graphics-sprite [filename n]
  (->> filename
       (load-bytes)
       (parse-entities)
       (parse-graphics-sprite n)))


(defn convert-sprite [infile n outfile]
  (-> (load-graphics-sprite infile n)
      (adjust-brightness 5)
      (save-picture-as-png! outfile)))


(defn cajji []
  (convert-256 "/Volumes/Untitled/_INTRO/CAJJI.256" 0 "cajji.png"))


(defn c1 [n]
  (convert-sprite "/Volumes/Untitled/_C1/999.256" n (str "c1_" n ".png")))


(defn c2 []
  (convert-sprite "/Volumes/Untitled/_C2/999.256" 0 "c2.png"))


(defn c3 []
  (convert-sprite "/Volumes/Untitled/_C3/999.256" 0 "c3.png"))


(defn c4 []
  (convert-sprite "/Volumes/Untitled/_C4/999.256" 0 "c4.png"))


(defn c5 []
  (convert-sprite "/Volumes/Untitled/_C5/999.256" 0 "c5.png"))


(defn c6 []
  (convert-sprite "/Volumes/Untitled/_C6/999.256" 0 "c6.png"))


(defn fe-dome [n]
  (convert-sprite "/Volumes/Untitled/_MS/FE_DOME.256" n (str "fe_dome_" n ".png")))


(defn fe-base []
  (convert-sprite "/Volumes/Untitled/_MS/FE_BASE.256" 0 "fe_base.png"))


(defn fe-edge []
  (convert-sprite "/Volumes/Untitled/_MS/FE_EDGE.256" 0 "fe_edge.png"))


(defn xb00 [n]
  (convert-sprite "/Volumes/Untitled/_P/XB00.256" n (str "xb00_" n ".png")))


(defn edgemenu [n]
  (convert-sprite "/Volumes/Untitled/_MS/EDGEMENU.256" n (str "edgemenu_" n ".png")))


(defn orange [n]
  (convert-sprite "/Volumes/Untitled/_MS/ORANGE.256" n (str "orange_" n ".png")))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
