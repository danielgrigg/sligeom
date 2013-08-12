(ns sligeom.aggregate
  (:use [slimath core vec matrix]
        [sligeom core transform bounding intersect])
  (:import [sligeom.transform Transform]
           [sligeom.bounding BBox]
           [sligeom.intersect Ray]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defrecord Grid3 [^BBox bounds divisions voxels])

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

(defn grid3 [^BBox bounds ^long nprims]
  (->Grid3 bounds (-> nprims
                      (divisions-for bounds)
                      clamp-divisions)
           nil))

(defn voxel-size 
  "cartesian size of a voxel in grid g"
  [^Grid3 g]
  (-> g
      :bounds
      bbox-size
      (v3div (:divisions g))))

(defn point-to-voxel 
  "get xyz voxel index corresponding to a point"
[^Grid3 g [^double px ^double py ^double pz :as p]]
(-> p
    (v3sub (:minp (:bounds g)))
    (v3div (voxel-size g))
    point3
    v3int))

(defn voxel-to-point 
  "cartesian point corresponding to voxel index xyz"
  [^Grid3 grid [^long vx ^long vy ^long vz _ :as v]]
  (-> v
      (v3mul (voxel-size grid))
      (v3add (:minp (:bounds grid)))
      point3))

(defn voxel-clamp 
  "Clamp a voxel-point to a grid"
  [^Grid3 grid [^long vx ^long vy ^long vz :as v]]
  (-> v
      (v3clamp (:minp (:bounds grid)) (:maxp (:bounds grid)))
      v3int))

(defn voxel-bbox "Compute the voxels spanned" 
  [^Grid3 grid ^BBox bounds]
  (bbox (point-to-voxel grid (voxel-clamp grid (:minp bounds)))
        (point-to-voxel grid (voxel-clamp grid (:maxp bounds)))))

(defn voxel-bbox-seq 
  "seq of voxels spanned by bounds"
  [^Grid3 grid ^BBox bounds]
  (let [bbox' (voxel-bbox grid bounds)
        [ x0 y0 z0] (:minp bbox')
        [x1 y1 z1] (map inc (:maxp bbox'))]
    (for [z (range z0 z1) y (range y0 y1) x (range x0 x1)] [x y z])))

(defn- ray-enters-at [^Grid3 g ^Ray r]
  "compute where r enters the grid g"
  (if (contains-point? (:bounds g) (:origin r)) 
    0.0
    (intersect (:bounds g) r)))

(defn grid3-seq
  "sequence the cells intersecting the ray r"
  [^Grid3 grid ^Ray r]
  (if-let [enter-t (ray-enters-at grid r)]
    (let [o (ray-at r enter-t)
          d (point3 (v3clamp (:direction r)))
          [nx ny nz :as n] (:divisions grid)
          [sx sy sz :as s] (v3sign (:direction r))

          v0 (-> (v3sub n (point3 1 1 1))
                 (v3min (point-to-voxel grid o)))

          v1 (v3add v0 (v3max s [0 0 0]))

          tmax (-> (voxel-to-point grid v1)
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
