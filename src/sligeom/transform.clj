(ns sligeom.transform
    (:use [slimath core vec matrix]))

(defprotocol Transformable
  (transform [this T] "Transform the object by T"))

(deftype Transform [transform inverse]
  Object
  (toString [this]
    (let [M (.transform this)]
      (str "\n" (m4str M)))))

(defn ^Transform inverse "Get the inverse transform of T" [^Transform T]
  (Transform. (.inverse T) (.transform T)))

(defn ^Transform matrix "Create a transform from a 4x4 matrix" [M]
  (Transform. M (m4inverse M)))

(defn ^Transform ident "Identity transformation" []
  (matrix (m4identity)))

(defn ^Transform compose   
  "Compose a sequence of transforms"
  ([^Transform m] m)
  ([^Transform m1 ^Transform m2]
     (Transform. (m4mul (.transform m1) (.transform m2))
                (m4mul (.inverse m2) (.inverse m1))))
  ([^Transform x ^Transform y & xs] 
     (loop [m (m4mul (.transform x) (.transform y)) ms xs]
       (if-not (seq ms)
         (matrix m)
         (let [^Transform m2 (.transform (first ms))]
           (recur (m4mul m m2) (rest ms)))))))

(defn ^Transform translate "Translation" [^double tx ^double ty ^double tz]
  (Transform. (matrix4 1. 0. 0. tx
                       0. 1. 0. ty
                       0. 0. 1. tz
                       0. 0. 0. 1.)
              (matrix4 1. 0. 0. (- tx)
                       0. 1. 0. (- ty)
                       0. 0. 1. (- tz)
                       0. 0. 0. 1.)))

(defn ^Transform scale "Scale" [^double sx ^double sy ^double sz]
  (Transform. (matrix4 sx 0. 0. 0.
                       0. sy 0. 0.
                       0. 0. sz 0.
                       0. 0. 0. 1.)
              (matrix4 (/ sx) 0. 0. 0.
                       0. (/ sy) 0. 0.
                       0. 0. (/ sz) 0.
                       0. 0. 0. 1.)))

(defn ^Transform rotate 
  "Rotation about an axis" 
  [[^double x ^double y ^double z :as axis] ^double rads]
  (let [c (Math/cos rads)
        s (Math/sin rads)
        n (v3normalize axis)
        x (double (n 0))
        y (double (n 1))
        z (double (n 2))
        M (matrix4 (+ c (* (- 1. c) x x))
                   (- (* (- 1. c) x y) (* z s))
                   (+ (* (- 1. c) x z) (* y s))
                   0.
                   (+ (* (- 1. c) x y) (* z s))
                   (+ c (* (- 1. c) y y))
                   (- (* (- 1. c) y z) (* x s))
                   0.
                   (- (* (- 1. c) x z) (* y s))
                   (+ (* (- 1. c) y z) (* x s))
                   (+ c (* (- 1. c) z z))
                   0.
                   0. 0. 0. 1.)]
    (Transform. M (m4transpose M))))

(def ^Transform rotate-x (partial rotate (vec3 1.0 0.0 0.0)))
(def ^Transform rotate-y (partial rotate (vec3 0.0 1.0 0.0)))
(def ^Transform rotate-z (partial rotate (vec3 0.0 0.0 1.0)))

(defn ^Transform ortho
  "Orthographic transform"
  [{:keys [left right bottom top near far]}]
  (let [M (matrix4 (/ 2. (- right left)) 0.  0.  (- (/ (+ right left) (- right left)))
                   0.  (/ 2. (- top bottom)) 0.  (- (/ (+ top bottom) (- top bottom)))
                   0.  0.  (/ 2. (- far near)) (- (/ (+ far near) (- far near)))
                   0.  0.  0.  1.)]
    (Transform. M (m4transpose M)))) 

(defn ^Transform perspective 
  "Perspective transform"
  [& {:keys [fov-rads aspect near far]}]
  (let [t (* near (Math/tan (* fov-rads 0.5)))
        b (- t)
        l (* b aspect)
        r (- l)
        M (matrix4 
            (/ (* 2. near) (- r l)) 0. (/ (+ r l) (- r l)) 0.
            0. (/ (* 2. near) (- t b)) (/ (+ t b) (- t b)) 0.
            0. 0. (- (/ (+ far near) (- far near))) (- (/ (* 2. far near) (- far near)))
            0. 0. -1. 0.)]
    (matrix M)))

(defn project "project homogenous point to r3" [p]
  (if (zero? (p 3))
    p
    (v4muls p (/ (p 3)))))

(defn ^double angle-of-view [^double plane-width ^double plane-distance]
  (* 2.0 (Math/atan (/ plane-width (* 2.0 plane-distance)))))

(defn transform-point 
  "Transform p by T" 
  [^Transform T [^double px ^double py ^double pz _ :as p]] 
  (m4mulv (.transform T) p))

(defn transform-vector 
  "Transform v by T" 
  [^Transform T [^double vx ^double vy ^double vz _ :as v]] 
  (m4mulv (.transform T) v))

(defn transform-normal 
  "Transform n by T" 
  [^Transform T [^double nx ^double ny ^double nz _ :as n]]
  (m34mulv (m4transpose (.inverse T)) n))
