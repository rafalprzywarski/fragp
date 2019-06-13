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
   :frame-count (load-uint16 bytes 6)
   :unknown2 (subvec bytes 8)})


(defn parse-video-inte [bytes {:keys [width height]}]
  (let [palette (decode-video-rle (subvec bytes 4))
        inte (decode-video-rle (subvec bytes (+ 4 (:rle-size palette))))]
    {:width width
     :height height
     :unknown1 (subvec bytes 0 4)
     :palette (parse-palette palette)
     :indices (upscale-2x (:bytes inte) (/ width 2))}))


(defn s4-to-int [n]
  (if (< n 8) n (- n 16)))


(defn decode-s4-pair [b]
  [(s4-to-int (bit-and 0xf b))
   (s4-to-int (bit-shift-right b 4))])


(defn decode-fram-indices [blocks pixels {:keys [width height] prev-indices :indices}]
  (let [gdx (unchecked-byte (blocks 0))
        gdy (unchecked-byte (blocks 1))
        blocks (subvec blocks 2)
        decode-dxdy (fn [blocks]
                      [(decode-s4-pair (blocks 0)) (subvec blocks 1)])
        decode-mask (fn [blocks has-dxdy dx dy]
                      (if (and has-dxdy (zero? dx) (zero? dy))
                        [0xffff blocks]
                        (if (= dx -8)
                          (condp = dy
                            6 [(bit-shift-left (blocks 0) 8) (subvec blocks 1)]
                            -3 [(+ 0xff00 (blocks 0)) (subvec blocks 1)] ; difference between 5 and 0?
                            [(+ (bit-shift-left (blocks 1) 8) (blocks 0)) (subvec blocks 2)]) ; 0 and 5
                          [(+ (bit-shift-left (blocks 1) 8) (blocks 0)) (subvec blocks 2)])))
        mask-from-dxdy (fn [has-dxdy dx dy]
                         (if (and has-dxdy (zero? dx) (zero? dy))
                           0xffff
                           0x0000))
        get-pixel (fn [x y] (try (prev-indices (+ x (* y width))) (catch IndexOutOfBoundsException e nil)))
;        get-pixel (fn [x y] (prev-indices (+ x (* y width))))
        set-pixel (fn [indices x y p] (assoc! indices (+ x (* y width)) p))
        decode-filler (fn [dx pixels]
                        (if (= dx -8)
                          [(first pixels) (next pixels)]
                          [nil pixels]))
        decode-pixel (fn [indices pixels x y dx dy is-new filler]
                       (if is-new
                         [(set-pixel indices x y (first pixels)) (next pixels)]
                         [(set-pixel indices x y (or filler (get-pixel (+ x dx) (+ y dy)))) pixels]))
        decode-block4x4 (fn [indices pixels x y dx dy mask filler]
                          (let [[indices pixels] (decode-pixel indices pixels x y dx dy (bit-test mask 15) filler)
                                [indices pixels] (decode-pixel indices pixels (+ x 1) y dx dy (bit-test mask 14) filler)
                                [indices pixels] (decode-pixel indices pixels (+ x 2) y dx dy (bit-test mask 13) filler)
                                [indices pixels] (decode-pixel indices pixels (+ x 3) y dx dy (bit-test mask 12) filler)
                                y (inc y)
                                [indices pixels] (decode-pixel indices pixels x y dx dy (bit-test mask 11) filler)
                                [indices pixels] (decode-pixel indices pixels (+ x 1) y dx dy (bit-test mask 10) filler)
                                [indices pixels] (decode-pixel indices pixels (+ x 2) y dx dy (bit-test mask 9) filler)
                                [indices pixels] (decode-pixel indices pixels (+ x 3) y dx dy (bit-test mask 8) filler)
                                y (inc y)
                                [indices pixels] (decode-pixel indices pixels x y dx dy (bit-test mask 7) filler)
                                [indices pixels] (decode-pixel indices pixels (+ x 1) y dx dy (bit-test mask 6) filler)
                                [indices pixels] (decode-pixel indices pixels (+ x 2) y dx dy (bit-test mask 5) filler)
                                [indices pixels] (decode-pixel indices pixels (+ x 3) y dx dy (bit-test mask 4) filler)
                                y (inc y)
                                [indices pixels] (decode-pixel indices pixels x y dx dy (bit-test mask 3) filler)
                                [indices pixels] (decode-pixel indices pixels (+ x 1) y dx dy (bit-test mask 2) filler)
                                [indices pixels] (decode-pixel indices pixels (+ x 2) y dx dy (bit-test mask 1) filler)
                                [indices pixels] (decode-pixel indices pixels (+ x 3) y dx dy (bit-test mask 0) filler)]
                            [indices pixels]))
        indices (transient prev-indices)]
    (loop [blocks blocks
           pixels pixels
           x 0
           y 0]
      (if (or (empty? blocks)
              (= y height))
        (do
          (println "block bytes left:" (count blocks) "pixel bytes left:" (count pixels))
          (persistent! indices))
        (let [prefix (mapv #(format "%02X" %) (take 16 blocks))
             ;  _ (when (= [x y] [40 0]) (println prefix))
              [dx dy] (decode-s4-pair (blocks 0))
              cb (blocks 1)
              blocks (subvec blocks 2)
              [[dx0 dy0] blocks] (if (bit-test cb 6) (decode-dxdy blocks) [[0 0] blocks])
              [mask0 blocks] (if (bit-test cb 7) (decode-mask blocks (bit-test cb 6) dx0 dy0) [(mask-from-dxdy (bit-test cb 6) dx0 dy0) blocks])
              [[dx1 dy1] blocks] (if (bit-test cb 4) (decode-dxdy blocks) [[0 0] blocks])
              [mask1 blocks] (if (bit-test cb 5) (decode-mask blocks (bit-test cb 4) dx1 dy1) [(mask-from-dxdy (bit-test cb 4) dx1 dy1) blocks])
              [[dx2 dy2] blocks] (if (bit-test cb 2) (decode-dxdy blocks) [[0 0] blocks])
              [mask2 blocks] (if (bit-test cb 3) (decode-mask blocks (bit-test cb 2) dx2 dy2) [(mask-from-dxdy (bit-test cb 2) dx2 dy2) blocks])
              [[dx3 dy3] blocks] (if (bit-test cb 0) (decode-dxdy blocks) [[0 0] blocks])
              [mask3 blocks] (if (bit-test cb 1) (decode-mask blocks (bit-test cb 0) dx3 dy3) [(mask-from-dxdy (bit-test cb 0) dx3 dy3) blocks])
              _ (if (= dx0 -8) (println dy0 x y prefix))
              _ (if (= dx1 -8) (println dy1 x y prefix))
              _ (if (= dx2 -8) (println dy2 x y prefix))
              _ (if (= dx3 -8) (println dy3 x y prefix))
              [filler0 pixels] (decode-filler dx0 pixels)
              [indices pixels] (decode-block4x4 indices pixels x y (+ gdx dx dx0) (+ gdy dy dy0) mask0 filler0)
              [filler1 pixels] (decode-filler dx1 pixels)
              [indices pixels] (decode-block4x4 indices pixels (+ x 4) y (+ gdx dx dx1) (+ gdy dy dy1) mask1 filler1)
              [filler2 pixels] (decode-filler dx2 pixels)
              [indices pixels] (decode-block4x4 indices pixels x (+ y 4) (+ gdx dx dx2) (+ gdy dy dy2) mask2 filler2)
              [filler3 pixels] (decode-filler dx3 pixels)
              [indices pixels] (decode-block4x4 indices pixels (+ x 4) (+ y 4) (+ gdx dx dx3) (+ gdy dy dy3) mask3 filler3)
              [x y] (if (< (+ x 8) width) [(+ x 8) y] [0 (+ y 8)])]
          (recur blocks pixels x y))))))


(defn parse-video-fram [bytes {:keys [width height palette] :as prev-frame}]
  (let [blocks (decode-video-rle (subvec bytes 2))
        pixels (decode-video-rle (subvec bytes (+ 2 (:rle-size blocks))))]
    {:unknown1 (subvec bytes 0 2)
     :width width
     :height height
     :palette palette
     :indices (decode-fram-indices (:bytes blocks) (:bytes pixels) prev-frame)}))


(defn parse-video [bytes]
  (let [total (load-uint32 bytes 4)
        entities (parse-video-entities (subvec bytes 8 total) (- total 8))
        head (parse-video-head (:bytes (first entities)))]
    (loop [entities (next entities)
           prev-frame nil
           frames (transient [])]
      (let [entity (first entities)
            id (:id entity)]
        (condp = id
          "Inte" (recur (next entities) (parse-video-inte (:bytes entity) head) frames)
          "Fram" (let [frame (parse-video-fram (:bytes entity) prev-frame)]
                   (recur (next entities) frame (conj! frames frame)))
          "Same" (recur (next entities) prev-frame (conj! frames prev-frame))
          "Stop" (persistent! frames))))))


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


(defn save-frames! [frames ^String filename]
  (doseq [[idx frame] (map-indexed (fn [i f] [i f]) frames)]
    (save-picture-as-png! frame (.replace filename "#" (str idx)))))


(defn c2-002 []
  (save-frames! (load-video "/Volumes/Untitled/_C2/001.VID") "c2_001_#.png"))


(defn cajji-vid []
  (save-frames! (load-video "/Users/rp/Projects/FragilePlayground/Fragile Allegiance.boxer/C.harddisk/FRAGILE/_INTRO/CAJJI.VID") "cajji_vid_#.png"))


(defn cajji-orig-vid []
  (save-frames! (load-video "/Volumes/Untitled/_INTRO/CAJJI.VID") "cajji_orig_vid_#.png"))


(defn st00 []
  (save-frames! (load-video "/Volumes/Untitled/_SCITEK/ST00.VID") "st00_#.png"))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
