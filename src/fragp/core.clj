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
        decode-mask (fn [blocks has-mask has-dxdy dx dy]
                      (cond (and has-dxdy (zero? dx) (zero? dy)) [0xffff blocks]
                            (= dx -8) (condp = dy
                                        7 [(+ 0xff (bit-shift-left (blocks 0) 8)) (subvec blocks 1)]
                                        3 [(+ 0xff (bit-shift-left (blocks 0) 8)) (subvec blocks 1)]
                                        6 [(bit-shift-left (blocks 0) 8) (subvec blocks 1)]
                                        2 [(bit-shift-left (blocks 0) 8) (subvec blocks 1)]
                                        4 [(+ (bit-shift-left (blocks 1) 8) (blocks 0)) (subvec blocks 2)]
                                        5 [(+ (bit-shift-left (blocks 1) 8) (blocks 0)) (subvec blocks 2)]
                                        1 [(+ (bit-shift-left (blocks 1) 8) (blocks 0)) (subvec blocks 2)]
                                        0 [(+ (bit-shift-left (blocks 1) 8) (blocks 0)) (subvec blocks 2)]
                                        -1 [0xffff blocks]
                                        -2 [0xff00 blocks]
                                        -3 [(+ 0xff00 (blocks 0)) (subvec blocks 1)]
                                        -4 [(+ 0xff00 (blocks 0)) (subvec blocks 1)]
                                        -5 [0x00ff blocks]
                                        -6 [0x0000 blocks]
                                        -8 [0x0000 blocks]
                                        -7 [(blocks 0) (subvec blocks 1)])
                            has-mask [(+ (bit-shift-left (blocks 1) 8) (blocks 0)) (subvec blocks 2)]
                            :else [0x0000 blocks]))
        get-pixel (fn [x y] (prev-indices (+ x (* y width))))
        set-pixel (fn [indices x y p] (assoc! indices (+ x (* y width)) p))
        decode-block-dxdy (fn [blocks]
                            (let [[dx dy] (decode-s4-pair (blocks 0))
                                  blocks (subvec blocks 1)
                                  [dx blocks] (if (= dx -8) [(unchecked-byte (blocks 0)) (subvec blocks 1)] [dx blocks])
                                  [dy blocks] (if (= dy -8) [(unchecked-byte (blocks 0)) (subvec blocks 1)] [dy blocks])]
                              [dx dy blocks]))
        decode-filler (fn [dx dy pixels]
                        (if (and (= dx -8) (not= dy -8))
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
          (when (or (pos? (count blocks)) (pos? (count pixels)))
            (println "DECODING ERROR:" "block bytes left:" (count blocks) "pixel bytes left:" (count pixels)))
          (flush)
          (persistent! indices))
        (let [prefix (take 16 blocks)
              [dx dy blocks] (decode-block-dxdy blocks)
              cb (blocks 0)
              blocks (subvec blocks 1)
              [[dx0 dy0] blocks] (if (bit-test cb 6) (decode-dxdy blocks) [[0 0] blocks])
              [mask0 blocks] (decode-mask blocks (bit-test cb 7) (bit-test cb 6) dx0 dy0)
              [[dx1 dy1] blocks] (if (bit-test cb 4) (decode-dxdy blocks) [[0 0] blocks])
              [mask1 blocks] (decode-mask blocks (bit-test cb 5) (bit-test cb 4) dx1 dy1)
              [[dx2 dy2] blocks] (if (bit-test cb 2) (decode-dxdy blocks) [[0 0] blocks])
              [mask2 blocks] (decode-mask blocks (bit-test cb 3) (bit-test cb 2) dx2 dy2)
              [[dx3 dy3] blocks] (if (bit-test cb 0) (decode-dxdy blocks) [[0 0] blocks])
              _ (when (or (and (= dy0 -8) (not= dx0 -8))
                          (and (= dy1 -8) (not= dx1 -8))
                          (and (= dy2 -8) (not= dx2 -8))
                          (and (= dy3 -8) (not= dx3 -8)))
                  (println "UNHANDLED SEQUENCE:" (mapv #(format "%02X" %) prefix)))
              [mask3 blocks] (decode-mask blocks (bit-test cb 1) (bit-test cb 0) dx3 dy3)
              [filler0 pixels] (decode-filler dx0 dy0 pixels)
              [indices pixels] (decode-block4x4 indices pixels x y (+ gdx dx dx0) (+ gdy dy dy0) mask0 filler0)
              [filler1 pixels] (decode-filler dx1 dy1 pixels)
              [indices pixels] (decode-block4x4 indices pixels (+ x 4) y (+ gdx dx dx1) (+ gdy dy dy1) mask1 filler1)
              [filler2 pixels] (decode-filler dx2 dy2 pixels)
              [indices pixels] (decode-block4x4 indices pixels x (+ y 4) (+ gdx dx dx2) (+ gdy dy dy2) mask2 filler2)
              [filler3 pixels] (decode-filler dx3 dy3 pixels)
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


(defn st17 []
  (save-frames! (load-video "/Volumes/Untitled/_SCITEK/ST17.VID") "st17_#.png"))


(def all-vids
  ["/Volumes/Untitled//_C1/001.VID"
   "/Volumes/Untitled//_C1/002.VID"
   "/Volumes/Untitled//_C1/003.VID"
   "/Volumes/Untitled//_C1/004.VID"
   "/Volumes/Untitled//_C1/005.VID"
   "/Volumes/Untitled//_C1/006.VID"
   "/Volumes/Untitled//_C1/007.VID"
   "/Volumes/Untitled//_C1/008.VID"
   "/Volumes/Untitled//_C1/009.VID"
   "/Volumes/Untitled//_C1/010.VID"
   "/Volumes/Untitled//_C1/011.VID"
   "/Volumes/Untitled//_C1/012.VID"
   "/Volumes/Untitled//_C1/013.VID"
   "/Volumes/Untitled//_C1/014.VID"
   "/Volumes/Untitled//_C1/015.VID"
   "/Volumes/Untitled//_C1/016.VID"
   "/Volumes/Untitled//_C2/001.VID"
   "/Volumes/Untitled//_C2/002.VID"
   "/Volumes/Untitled//_C2/003.VID"
   "/Volumes/Untitled//_C2/004.VID"
   "/Volumes/Untitled//_C2/005.VID"
   "/Volumes/Untitled//_C2/006.VID"
   "/Volumes/Untitled//_C2/008.VID"
   "/Volumes/Untitled//_C2/009.VID"
   "/Volumes/Untitled//_C2/010.VID"
   "/Volumes/Untitled//_C2/011.VID"
   "/Volumes/Untitled//_C2/012.VID"
   "/Volumes/Untitled//_C2/013.VID"
   "/Volumes/Untitled//_C2/014.VID"
   "/Volumes/Untitled//_C2/015.VID"
   "/Volumes/Untitled//_C2/016.VID"
   "/Volumes/Untitled//_C2/017.VID"
   "/Volumes/Untitled//_C2/018.VID"
   "/Volumes/Untitled//_C2/019.VID"
   "/Volumes/Untitled//_C2/020.VID"
   "/Volumes/Untitled//_C3/1A1.VID"
   "/Volumes/Untitled//_C3/1A2.VID"
   "/Volumes/Untitled//_C3/1A3.VID"
   "/Volumes/Untitled//_C3/2A1.VID"
   "/Volumes/Untitled//_C3/2A2.VID"
   "/Volumes/Untitled//_C3/2A3.VID"
   "/Volumes/Untitled//_C3/2B1.VID"
   "/Volumes/Untitled//_C3/2B2.VID"
   "/Volumes/Untitled//_C3/2B3.VID"
   "/Volumes/Untitled//_C3/3A1.VID"
   "/Volumes/Untitled//_C3/3A2.VID"
   "/Volumes/Untitled//_C3/3A3.VID"
   "/Volumes/Untitled//_C3/4A1.VID"
   "/Volumes/Untitled//_C3/4A2.VID"
   "/Volumes/Untitled//_C3/4A3.VID"
   "/Volumes/Untitled//_C3/5A2.VID"
   "/Volumes/Untitled//_C3/6A1.VID"
   "/Volumes/Untitled//_C3/6A2.VID"
   "/Volumes/Untitled//_C3/6A3.VID"
   "/Volumes/Untitled//_C3/7A1.VID"
   "/Volumes/Untitled//_C3/7A2.VID"
   "/Volumes/Untitled//_C3/7A3.VID"
   "/Volumes/Untitled//_C3/7B1.VID"
   "/Volumes/Untitled//_C3/7B2.VID"
   "/Volumes/Untitled//_C3/7B3.VID"
   "/Volumes/Untitled//_C3/8A1.VID"
   "/Volumes/Untitled//_C3/8A2.VID"
   "/Volumes/Untitled//_C3/8A3.VID"
   "/Volumes/Untitled//_C4/001.VID"
   "/Volumes/Untitled//_C4/002.VID"
   "/Volumes/Untitled//_C4/003.VID"
   "/Volumes/Untitled//_C4/004.VID"
   "/Volumes/Untitled//_C4/005.VID"
   "/Volumes/Untitled//_C4/006.VID"
   "/Volumes/Untitled//_C4/007.VID"
   "/Volumes/Untitled//_C4/008.VID"
   "/Volumes/Untitled//_C4/009.VID"
   "/Volumes/Untitled//_C4/010.VID"
   "/Volumes/Untitled//_C4/011.VID"
   "/Volumes/Untitled//_C4/012.VID"
   "/Volumes/Untitled//_C4/013.VID"
   "/Volumes/Untitled//_C4/014.VID"
   "/Volumes/Untitled//_C4/015.VID"
   "/Volumes/Untitled//_C4/016.VID"
   "/Volumes/Untitled//_C4/017.VID"
   "/Volumes/Untitled//_C4/018.VID"
   "/Volumes/Untitled//_C4/019.VID"
   "/Volumes/Untitled//_C4/020.VID"
   "/Volumes/Untitled//_C5/000.VID"
   "/Volumes/Untitled//_C5/001.VID"
   "/Volumes/Untitled//_C5/002.VID"
   "/Volumes/Untitled//_C5/003.VID"
   "/Volumes/Untitled//_C5/004.VID"
   "/Volumes/Untitled//_C5/005.VID"
   "/Volumes/Untitled//_C5/006.VID"
   "/Volumes/Untitled//_C5/007.VID"
   "/Volumes/Untitled//_C5/008.VID"
   "/Volumes/Untitled//_C5/010.VID"
   "/Volumes/Untitled//_C5/011.VID"
   "/Volumes/Untitled//_C5/012.VID"
   "/Volumes/Untitled//_C5/013.VID"
   "/Volumes/Untitled//_C5/016.VID"
   "/Volumes/Untitled//_C5/017.VID"
   "/Volumes/Untitled//_C5/018.VID"
   "/Volumes/Untitled//_C5/019.VID"
   "/Volumes/Untitled//_C5/022.VID"
   "/Volumes/Untitled//_C5/023.VID"
   "/Volumes/Untitled//_C5/024.VID"
   "/Volumes/Untitled//_C5/027.VID"
   "/Volumes/Untitled//_C5/028.VID"
   "/Volumes/Untitled//_C5/029.VID"
   "/Volumes/Untitled//_C5/032.VID"
   "/Volumes/Untitled//_C5/033.VID"
   "/Volumes/Untitled//_C5/034.VID"
   "/Volumes/Untitled//_C5/035.VID"
   "/Volumes/Untitled//_C5/036.VID"
   "/Volumes/Untitled//_C5/037.VID"
   "/Volumes/Untitled//_C5/038.VID"
   "/Volumes/Untitled//_C6/001.VID"
   "/Volumes/Untitled//_C6/002.VID"
   "/Volumes/Untitled//_C6/003.VID"
   "/Volumes/Untitled//_C6/004.VID"
   "/Volumes/Untitled//_C6/005.VID"
   "/Volumes/Untitled//_C6/006.VID"
   "/Volumes/Untitled//_C6/007.VID"
   "/Volumes/Untitled//_C6/008.VID"
   "/Volumes/Untitled//_C6/009.VID"
   "/Volumes/Untitled//_C6/010.VID"
   "/Volumes/Untitled//_C6/011.VID"
   "/Volumes/Untitled//_C6/012.VID"
   "/Volumes/Untitled//_C6/013.VID"
   "/Volumes/Untitled//_C6/014.VID"
   "/Volumes/Untitled//_C6/015.VID"
   "/Volumes/Untitled//_C6/016.VID"
   "/Volumes/Untitled//_C6/017.VID"
   "/Volumes/Untitled//_C6/018.VID"
   "/Volumes/Untitled//_INTRO/CAJJI.VID"
   "/Volumes/Untitled//_MA/PB00.VID"
   "/Volumes/Untitled//_MA/PB01.VID"
   "/Volumes/Untitled//_MA/PB02.VID"
   "/Volumes/Untitled//_MA/PB03.VID"
   "/Volumes/Untitled//_MA/PB04.VID"
   "/Volumes/Untitled//_MA/PB05.VID"
   "/Volumes/Untitled//_MA/PB06.VID"
   "/Volumes/Untitled//_MA/PB07.VID"
   "/Volumes/Untitled//_MA/PB08.VID"
   "/Volumes/Untitled//_MA/PB09.VID"
   "/Volumes/Untitled//_MA/PB10.VID"
   "/Volumes/Untitled//_MA/PB11.VID"
   "/Volumes/Untitled//_MA/PB12.VID"
   "/Volumes/Untitled//_MA/PB13.VID"
   "/Volumes/Untitled//_MA/PB14.VID"
   "/Volumes/Untitled//_MA/PB15.VID"
   "/Volumes/Untitled//_MA/PB16.VID"
   "/Volumes/Untitled//_MA/PB17.VID"
   "/Volumes/Untitled//_MA/PB18.VID"
   "/Volumes/Untitled//_MA/PB19.VID"
   "/Volumes/Untitled//_MA/PB20.VID"
   "/Volumes/Untitled//_MA/PB21.VID"
   "/Volumes/Untitled//_MA/PB22.VID"
   "/Volumes/Untitled//_MA/PB23.VID"
   "/Volumes/Untitled//_MA/PB24.VID"
   "/Volumes/Untitled//_MA/PB25.VID"
   "/Volumes/Untitled//_MA/PB26.VID"
   "/Volumes/Untitled//_MA/PB27.VID"
   "/Volumes/Untitled//_MA/PB28.VID"
   "/Volumes/Untitled//_MA/PB29.VID"
   "/Volumes/Untitled//_MA/PB30.VID"
   "/Volumes/Untitled//_MA/PB31.VID"
   "/Volumes/Untitled//_MA/PB32.VID"
   "/Volumes/Untitled//_MA/PB33.VID"
   "/Volumes/Untitled//_MA/PB34.VID"
   "/Volumes/Untitled//_MA/PB35.VID"
   "/Volumes/Untitled//_MA/PB36.VID"
   "/Volumes/Untitled//_MA/PB37.VID"
   "/Volumes/Untitled//_MA/PB38.VID"
   "/Volumes/Untitled//_MA/PB39.VID"
   "/Volumes/Untitled//_MA/PH00.VID"
   "/Volumes/Untitled//_MA/PH01.VID"
   "/Volumes/Untitled//_MA/PH02.VID"
   "/Volumes/Untitled//_MA/PH03.VID"
   "/Volumes/Untitled//_MA/PH04.VID"
   "/Volumes/Untitled//_MA/PH05.VID"
   "/Volumes/Untitled//_MA/PH06.VID"
   "/Volumes/Untitled//_MA/PH07.VID"
   "/Volumes/Untitled//_MA/PH08.VID"
   "/Volumes/Untitled//_MA/PH09.VID"
   "/Volumes/Untitled//_MA/PH10.VID"
   "/Volumes/Untitled//_MA/PH11.VID"
   "/Volumes/Untitled//_MA/PH12.VID"
   "/Volumes/Untitled//_MA/PH13.VID"
   "/Volumes/Untitled//_MA/PH14.VID"
   "/Volumes/Untitled//_MA/PH15.VID"
   "/Volumes/Untitled//_MA/PM00.VID"
   "/Volumes/Untitled//_MA/PM01.VID"
   "/Volumes/Untitled//_MA/PM02.VID"
   "/Volumes/Untitled//_MA/PM03.VID"
   "/Volumes/Untitled//_MA/PM04.VID"
   "/Volumes/Untitled//_MA/PM05.VID"
   "/Volumes/Untitled//_MA/PM06.VID"
   "/Volumes/Untitled//_MA/PM07.VID"
   "/Volumes/Untitled//_MA/PM08.VID"
   "/Volumes/Untitled//_MA/PM09.VID"
   "/Volumes/Untitled//_MA/PM10.VID"
   "/Volumes/Untitled//_MA/PS00.VID"
   "/Volumes/Untitled//_MA/PS01.VID"
   "/Volumes/Untitled//_MA/PS02.VID"
   "/Volumes/Untitled//_MA/PS03.VID"
   "/Volumes/Untitled//_MA/PS04.VID"
   "/Volumes/Untitled//_MA/PS05.VID"
   "/Volumes/Untitled//_MA/PS06.VID"
   "/Volumes/Untitled//_MA/PS08.VID"
   "/Volumes/Untitled//_MA/PS09.VID"
   "/Volumes/Untitled//_MA/ST00.VID"
   "/Volumes/Untitled//_MA/ST01.VID"
   "/Volumes/Untitled//_MA/ST05.VID"
   "/Volumes/Untitled//_MA/ST06.VID"
   "/Volumes/Untitled//_MA/ST07.VID"
   "/Volumes/Untitled//_MA/ST09.VID"
   "/Volumes/Untitled//_MA/ST11.VID"
   "/Volumes/Untitled//_MA/ST12.VID"
   "/Volumes/Untitled//_MA/ST14.VID"
   "/Volumes/Untitled//_MA/ST15.VID"
   "/Volumes/Untitled//_MA/ST20.VID"
   "/Volumes/Untitled//_MA/ST30.VID"
   "/Volumes/Untitled//_MA/ST31.VID"
   "/Volumes/Untitled//_MA/ST34.VID"
   "/Volumes/Untitled//_MA/ST35.VID"
   "/Volumes/Untitled//_SCITEK/ST00.VID"
   "/Volumes/Untitled//_SCITEK/ST01.VID"
   "/Volumes/Untitled//_SCITEK/ST02.VID"
   "/Volumes/Untitled//_SCITEK/ST03.VID"
   "/Volumes/Untitled//_SCITEK/ST04.VID"
   "/Volumes/Untitled//_SCITEK/ST05.VID"
   "/Volumes/Untitled//_SCITEK/ST06.VID"
   "/Volumes/Untitled//_SCITEK/ST07.VID"
   "/Volumes/Untitled//_SCITEK/ST08.VID"
   "/Volumes/Untitled//_SCITEK/ST09.VID"
   "/Volumes/Untitled//_SCITEK/ST10.VID"
   "/Volumes/Untitled//_SCITEK/ST11.VID"
   "/Volumes/Untitled//_SCITEK/ST12.VID"
   "/Volumes/Untitled//_SCITEK/ST13.VID"
   "/Volumes/Untitled//_SCITEK/ST14.VID"
   "/Volumes/Untitled//_SCITEK/ST15.VID"
   "/Volumes/Untitled//_SCITEK/ST16.VID"
   "/Volumes/Untitled//_SCITEK/ST17.VID"
   "/Volumes/Untitled//_SCITEK/ST18.VID"
   "/Volumes/Untitled//_SCITEK/ST19.VID"
   "/Volumes/Untitled//_SCITEK/ST20.VID"
   "/Volumes/Untitled//_SCITEK/ST21.VID"
   "/Volumes/Untitled//_SCITEK/ST22.VID"
   "/Volumes/Untitled//_SCITEK/ST23.VID"
   "/Volumes/Untitled//_SCITEK/ST24.VID"
   "/Volumes/Untitled//_SCITEK/ST25.VID"
   "/Volumes/Untitled//_SCITEK/ST26.VID"
   "/Volumes/Untitled//_SCITEK/ST27.VID"
   "/Volumes/Untitled//_SCITEK/ST28.VID"
   "/Volumes/Untitled//_SCITEK/ST29.VID"
   "/Volumes/Untitled//_SCITEK/ST30.VID"
   "/Volumes/Untitled//_SCITEK/ST31.VID"
   "/Volumes/Untitled//_SCITEK/ST32.VID"
   "/Volumes/Untitled//_SCITEK/ST33.VID"
   "/Volumes/Untitled//_SCITEK/ST34.VID"
   "/Volumes/Untitled//_SCITEK/ST35.VID"
   "/Volumes/Untitled//_ZOOM/AC01.VID"
   "/Volumes/Untitled//_ZOOM/AC02.VID"
   "/Volumes/Untitled//_ZOOM/AC03.VID"
   "/Volumes/Untitled//_ZOOM/AC04.VID"
   "/Volumes/Untitled//_ZOOM/AC05.VID"
   "/Volumes/Untitled//_ZOOM/AC06.VID"
   "/Volumes/Untitled//_ZOOM/AC07.VID"
   "/Volumes/Untitled//_ZOOM/AC08.VID"
   "/Volumes/Untitled//_ZOOM/AC09.VID"
   "/Volumes/Untitled//_ZOOM/AC10.VID"
   "/Volumes/Untitled//_ZOOM/AC11.VID"
   "/Volumes/Untitled//_ZOOM/AC12.VID"
   "/Volumes/Untitled//_ZOOM/AC13.VID"
   "/Volumes/Untitled//_ZOOM/AC14.VID"
   "/Volumes/Untitled//_ZOOM/AC15.VID"
   "/Volumes/Untitled//_ZOOM/AC16.VID"
   "/Volumes/Untitled//_ZOOM/AC17.VID"
   "/Volumes/Untitled//_ZOOM/AC18.VID"
   "/Volumes/Untitled//_ZOOM/AC19.VID"
   "/Volumes/Untitled//_ZOOM/AC20.VID"
   "/Volumes/Untitled//_ZOOM/AC21.VID"
   "/Volumes/Untitled//_ZOOM/AC22.VID"
   "/Volumes/Untitled//_ZOOM/AC23.VID"
   "/Volumes/Untitled//_ZOOM/AC24.VID"
   "/Volumes/Untitled//_ZOOM/AC25.VID"
   "/Volumes/Untitled//_ZOOM/AC26.VID"
   "/Volumes/Untitled//_ZOOM/AR01.VID"
   "/Volumes/Untitled//_ZOOM/AR02.VID"
   "/Volumes/Untitled//_ZOOM/AR03.VID"
   "/Volumes/Untitled//_ZOOM/AR04.VID"
   "/Volumes/Untitled//_ZOOM/AR05.VID"
   "/Volumes/Untitled//_ZOOM/AR06.VID"
   "/Volumes/Untitled//_ZOOM/AR07.VID"
   "/Volumes/Untitled//_ZOOM/AR08.VID"
   "/Volumes/Untitled//_ZOOM/AR09.VID"
   "/Volumes/Untitled//_ZOOM/AR10.VID"
   "/Volumes/Untitled//_ZOOM/AR11.VID"
   "/Volumes/Untitled//_ZOOM/AR12.VID"
   "/Volumes/Untitled//_ZOOM/AR13.VID"
   "/Volumes/Untitled//_ZOOM/AR14.VID"
   "/Volumes/Untitled//_ZOOM/AR15.VID"
   "/Volumes/Untitled//_ZOOM/AR16.VID"
   "/Volumes/Untitled//_ZOOM/AR17.VID"
   "/Volumes/Untitled//_ZOOM/AR18.VID"
   "/Volumes/Untitled//_ZOOM/AR19.VID"
   "/Volumes/Untitled//_ZOOM/AR20.VID"
   "/Volumes/Untitled//_ZOOM/AR21.VID"
   "/Volumes/Untitled//_ZOOM/AR22.VID"
   "/Volumes/Untitled//_ZOOM/AR23.VID"
   "/Volumes/Untitled//_ZOOM/BR01.VID"
   "/Volumes/Untitled//_ZOOM/BR02.VID"
   "/Volumes/Untitled//_ZOOM/BR03.VID"
   "/Volumes/Untitled//_ZOOM/BR04.VID"
   "/Volumes/Untitled//_ZOOM/BR05.VID"
   "/Volumes/Untitled//_ZOOM/BR06.VID"
   "/Volumes/Untitled//_ZOOM/BR07.VID"
   "/Volumes/Untitled//_ZOOM/BR08.VID"
   "/Volumes/Untitled//_ZOOM/BR09.VID"
   "/Volumes/Untitled//_ZOOM/BR10.VID"
   "/Volumes/Untitled//_ZOOM/BR11.VID"
   "/Volumes/Untitled//_ZOOM/BR12.VID"
   "/Volumes/Untitled//_ZOOM/BR13.VID"
   "/Volumes/Untitled//_ZOOM/BR14.VID"
   "/Volumes/Untitled//_ZOOM/BR15.VID"
   "/Volumes/Untitled//_ZOOM/BR16.VID"
   "/Volumes/Untitled//_ZOOM/BR17.VID"
   "/Volumes/Untitled//_ZOOM/BR18.VID"
   "/Volumes/Untitled//_ZOOM/BR19.VID"
   "/Volumes/Untitled//_ZOOM/BR20.VID"
   "/Volumes/Untitled//_ZOOM/BR21.VID"
   "/Volumes/Untitled//_ZOOM/BR22.VID"
   "/Volumes/Untitled//_ZOOM/BR23.VID"
   "/Volumes/Untitled//_ZOOM/MK01.VID"
   "/Volumes/Untitled//_ZOOM/MK02.VID"
   "/Volumes/Untitled//_ZOOM/MK03.VID"
   "/Volumes/Untitled//_ZOOM/MK04.VID"
   "/Volumes/Untitled//_ZOOM/MK05.VID"
   "/Volumes/Untitled//_ZOOM/MK06.VID"
   "/Volumes/Untitled//_ZOOM/MK07.VID"
   "/Volumes/Untitled//_ZOOM/MK08.VID"
   "/Volumes/Untitled//_ZOOM/MK09.VID"
   "/Volumes/Untitled//_ZOOM/MK10.VID"
   "/Volumes/Untitled//_ZOOM/MK11.VID"
   "/Volumes/Untitled//_ZOOM/MK12.VID"
   "/Volumes/Untitled//_ZOOM/MK13.VID"
   "/Volumes/Untitled//_ZOOM/MK14.VID"
   "/Volumes/Untitled//_ZOOM/MK15.VID"
   "/Volumes/Untitled//_ZOOM/MK16.VID"
   "/Volumes/Untitled//_ZOOM/MK17.VID"
   "/Volumes/Untitled//_ZOOM/MK18.VID"
   "/Volumes/Untitled//_ZOOM/MK19.VID"
   "/Volumes/Untitled//_ZOOM/MK20.VID"
   "/Volumes/Untitled//_ZOOM/MK21.VID"
   "/Volumes/Untitled//_ZOOM/MN01.VID"
   "/Volumes/Untitled//_ZOOM/MN02.VID"
   "/Volumes/Untitled//_ZOOM/MN03.VID"
   "/Volumes/Untitled//_ZOOM/MN04.VID"
   "/Volumes/Untitled//_ZOOM/MN05.VID"
   "/Volumes/Untitled//_ZOOM/MN06.VID"
   "/Volumes/Untitled//_ZOOM/MN07.VID"
   "/Volumes/Untitled//_ZOOM/MN08.VID"
   "/Volumes/Untitled//_ZOOM/MN09.VID"
   "/Volumes/Untitled//_ZOOM/MN10.VID"
   "/Volumes/Untitled//_ZOOM/MN11.VID"
   "/Volumes/Untitled//_ZOOM/MN12.VID"
   "/Volumes/Untitled//_ZOOM/MN13.VID"
   "/Volumes/Untitled//_ZOOM/MN14.VID"
   "/Volumes/Untitled//_ZOOM/MN15.VID"
   "/Volumes/Untitled//_ZOOM/MN16.VID"
   "/Volumes/Untitled//_ZOOM/MN17.VID"
   "/Volumes/Untitled//_ZOOM/MN18.VID"
   "/Volumes/Untitled//_ZOOM/MN19.VID"
   "/Volumes/Untitled//_ZOOM/MN20.VID"
   "/Volumes/Untitled//_ZOOM/MN21.VID"
   "/Volumes/Untitled//_ZOOM/RI01.VID"
   "/Volumes/Untitled//_ZOOM/RI02.VID"
   "/Volumes/Untitled//_ZOOM/RI03.VID"
   "/Volumes/Untitled//_ZOOM/RI04.VID"
   "/Volumes/Untitled//_ZOOM/RI05.VID"
   "/Volumes/Untitled//_ZOOM/RI06.VID"
   "/Volumes/Untitled//_ZOOM/RI07.VID"
   "/Volumes/Untitled//_ZOOM/RI08.VID"
   "/Volumes/Untitled//_ZOOM/RI09.VID"
   "/Volumes/Untitled//_ZOOM/RI10.VID"
   "/Volumes/Untitled//_ZOOM/RI11.VID"
   "/Volumes/Untitled//_ZOOM/RI12.VID"
   "/Volumes/Untitled//_ZOOM/RI13.VID"
   "/Volumes/Untitled//_ZOOM/RI14.VID"
   "/Volumes/Untitled//_ZOOM/RI15.VID"
   "/Volumes/Untitled//_ZOOM/RI16.VID"
   "/Volumes/Untitled//_ZOOM/RI17.VID"
   "/Volumes/Untitled//_ZOOM/RI18.VID"
   "/Volumes/Untitled//_ZOOM/RI19.VID"
   "/Volumes/Untitled//_ZOOM/RI20.VID"
   "/Volumes/Untitled//_ZOOM/RI21.VID"
   "/Volumes/Untitled//_ZOOM/RI22.VID"
   "/Volumes/Untitled//_ZOOM/RI23.VID"])


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
