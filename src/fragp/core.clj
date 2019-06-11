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


(defn load-string4 [bytes off]
  (apply str (map char (take 4 (drop off bytes)))))


(defn load-cstring [bytes off len]
  (->> bytes
       (drop off)
       (take len)
       (take-while pos?)
       (map char)
       (apply str)))


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

(defn parse-video-entities [bytes total]
  (loop [off 0
         entities (transient [])]
    (if (>= off total)
      (persistent! entities)
      (let [id (load-string4 bytes off)
            size (load-uint32 bytes (+ off 4))]
        (recur (+ off size)
               (conj! entities {:id id
                                :bytes (subvec bytes (+ off 8) (+ off size))}))))))


(defn parse-picture [entity]
  (let [bytes (:bytes entity)
        width (load-uint16 bytes 12)
        height (load-uint16 bytes 14)]
    {:width width
     :height height
     :indices (subvec bytes 16)}))


(defn parse-palette [entity]
  (vec (map vec (partition 3 (map #(* 4 %) (:bytes entity))))))


(defn parse-graphics [entities]
  (let [picture (parse-picture (entities "GRAPHICS"))
        palette (parse-palette (entities "PALETTE "))]
    (assoc picture :palette palette)))


(defn load-graphics [filename]
  (->> filename
       (load-bytes)
       (parse-entities)
       (parse-graphics)))


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


(defn decode-video-rle [bytes]
  (loop [bytes bytes
         csize 0
         out (transient [])]
    (if (or (empty? bytes) (zero? (first bytes)))
      {:bytes (persistent! out)
       :rle-size (if (empty? bytes) csize (inc csize))}
      (let [n (first bytes)]
        (if (> n 127)
          (let [nfill (- 256 n)
                index (second bytes)]
            (recur (subvec bytes 2)
                   (+ csize 2)
                   (reduce conj! out (repeat nfill index))))
          (recur (subvec bytes (inc n))
                 (+ csize (inc n))
                 (reduce conj! out (take n (drop 1 bytes)))))))))


(defn upscale-2x [pixels width]
  (persistent! (reduce (fn [out row]
                         (reduce (fn [out p]
                                   (-> out
                                       (conj! p)
                                       (conj! p)))
                                 out
                                 (concat row row)))
                       (transient [])
                       (partition width pixels))))


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


(defn parse-video-head [bytes]
  {:id "Head"
   :unknown1 (load-uint16 bytes 0)
   :width (load-uint16 bytes 2)
   :height (load-uint16 bytes 4)
   :unknown2 (subvec bytes 6)})


(defn parse-video-inte [bytes {:keys [width height]}]
  (let [palette (decode-video-rle (subvec bytes 4))
        inte (decode-video-rle (subvec bytes (+ 4 (:rle-size palette))))]
    {:width width
     :height height
     :unknown1 (subvec bytes 0 4)
     :palette (parse-palette palette)
     :indices (upscale-2x (:bytes inte) (/ width 2))}))


(defn parse-video-frame [bytes {:keys [width height]}]
  {:width width
   :height height
   :indices (vec (concat (subvec bytes 0x5116) (repeat (* width height) nil)))})


(defn parse-video [bytes]
  (let [total (load-uint32 bytes 4)
        entities (parse-video-entities (subvec bytes 8 total) (- total 8))
        head (parse-video-head (:bytes (first entities)))]
                                        ;(prn (map #(dissoc (assoc % :size (count (:bytes %))) % :bytes) entities))
    (parse-video-inte (:bytes (nth entities 1)) head)
                                        ; (prn (count (decode-inte-image (subvec (:bytes (nth entities 2)) 0x3a60))))
                                        ; (parse-video-inte (subvec (:bytes (nth entities 2)) 0x3a60) head)
    ))


; 0x3A60 first pixel of the frame
; 8899

; 5D - 01011101
; 5F   4B   5F   5D
; 0100 0101
; 1011 1101
; 0101 0101
; 1111 1111

(defn load-video [filename]
  (->> filename
       (load-bytes)
       (parse-video)))


(defn load-palette [filename]
  (-> filename
       (load-bytes)
       (parse-entities)
       (get "PALETTE ")
       (parse-palette)))


(defn convert-sprite [infile n outfile]
  (-> (load-graphics-sprite infile n)
      (save-picture-as-png! outfile)))


(defn convert-sprites [infile outfile]
  (let [sprites (load-graphics-sprites infile)]
    (loop [sprites sprites
           i 0]
      (when sprites
        (let [sprite (first sprites)]
          (when (and (pos? (:width sprite)) (pos? (:height sprite)))
            (save-picture-as-png!
             sprite
             (.replace outfile "#" (str i))))
          (recur (next sprites) (inc i)))))))


(defn parse-archive [bytes]
  (let [data-offset (load-uint32 bytes 20)
        nentries (/ (- data-offset 24) 24)]
    {:name (load-cstring bytes 0 20)
     :entries (map (fn [idx]
                     (let [base (+ 24 (* idx 24))]
                       {:name (load-cstring bytes base 16)
                        :offset (load-uint32 bytes (+ base 16))
                        :size (load-uint32 bytes (+ base 20))}))
                   (range nentries))}))


(defn load-archive [filename]
  (-> filename
      load-bytes
      parse-archive))


(defn explode-archive
  ([bytes entries]
   (doseq [{:keys [name offset size]} entries]
     (clojure.java.io/copy (byte-array (subvec bytes offset (+ offset size))) (clojure.java.io/file name))))
  ([filename]
   (let [bytes (load-bytes filename)
         archive (parse-archive bytes)]
     (explode-archive bytes (:entries archive)))))


(defn cajji []
  (convert-256 "/Volumes/Untitled/_INTRO/CAJJI.256" "cajji.png"))


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


(defn c2-002 []
  (let [frame (load-video "/Volumes/Untitled/_C2/001.VID")]
    (save-picture-as-png! frame "c2_001_0.png")))

(defn cajji-vid []
  (let [frame (load-video "/Volumes/Untitled/_INTRO/CAJJI.VID")]
    (save-picture-as-png! frame "cajji_vid_1c.png")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
