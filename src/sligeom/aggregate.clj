(ns sligeom.aggregate
  (:use [slimath core vec matrix]
        [sligeom core transform bounding intersect])
  (:import [sligeom.transform Transform]
           [sligeom.bounding BBox]
           [sligeom.intersect Ray]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defrecord Grid [^BBox bounds divisions voxels])

(defn divisions-for 
  "compute a heuristically optimal divisions for a bounding box for nprims primitives"
  [^long nprims ^BBox bounds]
  (let [longest-nvoxels (* 3.0 (Math/pow nprims (/ 3.0)))
        nvoxels-per-unit (/ longest-nvoxels (longest bounds))]
    [(* nvoxels-per-unit (.width bounds))
     (* nvoxels-per-unit (.height bounds))
     (* nvoxels-per-unit (.depth bounds))]))

(defn clamp-divisions [[dx dy dz :as d]]
  [(int (clamp dx 1 64)) 
   (int (clamp dy 1 64))
   (int (clamp dz 1 64))])

(defn grid [^BBox bounds ^long nprims]
  (let [[dx dy dz :as ds] (-> nprims (divisions-for bounds) clamp-divisions)
        vs (vec (repeat (* dx dy dz) []))] 
    (->Grid bounds ds vs)))

(defn voxel-size 
  "cartesian size of a voxel in grid g"
  [^Grid g]
  (-> g
      :bounds
      bbox-size
      (v3div (:divisions g))))

(defn voxel-idx 
  "voxel position to flat array index"
  [[x y z] ^Grid grid ]
  (let [[w h _] (:divisions grid) ]  
    (+ (* z (* w h))
     (* y w)
     x)))

(defn point-to-voxel 
  "get xyz voxel index corresponding to a point"
[[^double px ^double py ^double pz :as p] ^Grid g]
(-> p
    (v3sub (:minp (:bounds g)))
    (v3div (voxel-size g))
    point3
    v3int))

(defn voxel-to-point 
  "cartesian point corresponding to voxel index xyz"
  [[^long vx ^long vy ^long vz _ :as v] ^Grid grid]
  (-> v
      (v3mul (voxel-size grid))
      (v3add (:minp (:bounds grid)))
      point3))

(defn voxel-clamp
  "Clamp a voxel-point to a grid"
  [[^long vx ^long vy ^long vz :as v] ^Grid grid]
  (v3clamp v (:minp (:bounds grid)) (:maxp (:bounds grid))))

(defn voxel-bbox "Compute the voxels spanned" 
  [ ^BBox bounds ^Grid grid]
  (bbox (-> bounds :minp (voxel-clamp grid) (point-to-voxel grid) point3)
        (-> bounds :maxp (voxel-clamp grid) (point-to-voxel grid) point3)))

(defn voxel-bbox-seq 
  "seq of voxels spanned by bounds"
  [^BBox bounds ^Grid grid]
  (let [bbox' (voxel-bbox bounds grid)
        [x0 y0 z0] (:minp bbox')
        [x1 y1 z1] (map inc (:maxp bbox'))]
    (for [z (range z0 z1) y (range y0 y1) x (range x0 x1)] [x y z])))

(defn grid-add
  "Add an (bounded) object to a grid."
  [object ^Grid grid]
  (->> (voxel-bbox-seq (bounding-box object) grid)
       (map #(voxel-idx % grid)) 
       (reduce (fn [coll k] (update-in coll [k] conj object)) (:voxels grid))
       (assoc grid :voxels)))

;; (reduce (fn [coll x] (grid-add x coll)) (test-grid) 
;; [ (bbox (point3 -2 -1 0) (point3 2 3 4)) (sphere 2.0) (sphere 6.0)])

(defn- ray-enters-at [^Ray r ^Grid g]
  "compute where r enters the grid g"
  (if (contains-point? (:bounds g) (:origin r)) 
    0.0
    (intersect (:bounds g) r)))

(defn grid-seq
  "sequence the cells intersecting the ray r"
  [^Grid grid ^Ray r]
  (if-let [enter-t (ray-enters-at r grid)]
    (let [o (ray-at r enter-t)
          d (point3 (v3max (:direction r) (vector3 eps eps eps)))
          [nx ny nz :as n] (:divisions grid)
          [sx sy sz :as s] (v3sign (:direction r))

          v0 (-> (v3sub n (point3 1 1 1))
                 (v3min (point-to-voxel o grid )))

          v1 (v3add v0 (v3max s [0 0 0]))

          tmax (-> (voxel-to-point v1 grid)
                   (v3sub o)
                   (v3div d)
                   v3abs)
          [tdx tdy tdz :as td] (-> (voxel-size grid)
                                   (v3div d)
                                   v3abs)
          
          step-fx (fn step [[^long x ^long y ^long z] 
                            [^double tmx ^double tmy ^double tmz]]
                    (if (and (< x nx) (>= x 0)
                             (< y ny) (>= y 0)
                             (< z nz) (>= z 0)) 
                      (cons [x y z]
                            (lazy-seq
                             (if (< tmx tmy)
                               (if (< tmx tmz)                                 
                                 (step [(+ x sx) y z]
                                       [(+ tmx tdx) tmy tmz])
                                 (step [x y (+ z sz)]
                                       [tmx tmy (+ tmz tdz)]))
                               (if (< tmy tmz)
                                 (step [x (+ y sy) z]
                                       [tmx (+ tmy tdy) tmz])
                                 (step [x y (+ z sz)]
                                       [tmx tmy (+ tmz tdz)])))))))]
  (step-fx v0 tmax))))
