(in-ns 'sligeom.core)

(deftype Transform [transform inverse]
  Object
  (toString [this]
    (let [M (map vec (.transform this))]
      (str "\n" (clojure.string/join "\n" M)))))

(defn ^Transform inverse "Get the inverse transform of T" [^Transform T]
  (Transform. (.inverse T) (.transform T)))

(defn ^Transform matrix "Create a transform from a 4x4 matrix" [M]
  (Transform. M (m4inverse M)))

(defn ^Transform identity-transform "Identity transformation" []
  (matrix (m4identity)))

(defmacro compose "Compose a sequence of transforms" [& xs]
  `(matrix (m4mul ~@(for [x xs] `(.transform ~x)))))

(defn ^Transform translate "Translation" [^double tx ^double ty ^double tz]
  (Transform. [[1. 0. 0. tx]
               [0. 1. 0. ty]
               [0. 0. 1. tz]
               [0. 0. 0. 1.]]
              [[1. 0. 0. (- tx)]
               [0. 1. 0. (- ty)]
               [0. 0. 1. (- tz)]
               [0. 0. 0. 1.]]))

(defn ^Transform scale "Scale" [^double sx ^double sy ^double sz]
  (Transform. [[sx 0. 0. 0.]
               [0. sy 0. 0.]
               [0. 0. sz 0.]
               [0. 0. 0. 1.]]
              [[(/ sx) 0. 0. 0.]
               [0. (/ sy) 0. 0.]
               [0. 0. (/ sz) 0.]
               [0. 0. 0. 1.]]))
  
(defn ^Transform rotate-x "Rotation about x-axis" [^double rads]
  (let [c (Math/cos rads)
        s (Math/sin rads)
        M [[1. 0. 0. 0.]
           [0. c (- s) 0.]
           [0. s c 0.]
           [0. 0. 0. 1.]]]
        (Transform. M (m4transpose M))))

(defn ^Transform rotate-y "Rotation about y-axis" [^double rads]
  (let [c (Math/cos rads)
        s (Math/sin rads)
        M [[c 0. s 0.]
            [0. 1. 0. 0.]
            [(- s) 0. c 0.]
            [0. 0. 0. 1.]]]
    (Transform. M (m4transpose M))))

(defn rotate-z "Rotation about z-axis" [^double rads]
  (let [c (Math/cos rads)
        s (Math/sin rads)
        M [[c (- s) 0. 0.]
           [s c 0. 0.]
           [0. 0. 1. 0.]
           [0. 0. 0. 1.]]]
    (Transform. M (m4transpose M))))

(defn ^Transform rotate-axis "Rotation about an axis" [^double rads axis]
  (let [c (Math/cos rads)
        s (Math/sin rads)
        n (v3normalize axis)
        x (double (n 0))
        y (double (n 1))
        z (double (n 2))
        M [[(+ c (* (- 1. c) x x))
            (- (* (- 1. c) x y) (* z s))
            (+ (* (- 1. c) x z) (* y s))
            0.]
           [(+ (* (- 1. c) x y) (* z s))
            (+ c (* (- 1. c) y y))
            (- (* (- 1. c) y z) (* x s))
            0.]
            [(- (* (- 1. c) x z) (* y s))
            (+ (* (- 1. c) y z) (* x s))
            (+ c (* (- 1. c) z z))
            0.]
           [0. 0. 0. 1.]]]
    (Transform. M (m4transpose M))))

(defn ^Transform ortho [l r b t n f]
  (let [M [[(/ 2. (- r l)) 0. 0. (- (/ (+ r l) (- r l)))]
           [0. (/ 2. (- t b)) 0. (- (/ (+ t b) (- t b)))]
           [0. 0. (/ 2. (- f n)) (- (/ (+ f n) (- f n)))]
           [0. 0. 0. 1.]]]
    (Transform. M (m4transpose M)))) 

(defn ^Transform perspective 
"Perspective transform"
[^double fov_rads ^double aspect ^double n ^double f]
  (let [t (* n (Math/tan (* fov_rads 0.5)))
        b (- t)
        l (* b aspect)
        r (- l)
        M [[(/ (* 2. n) (- r l)) 0. (/ (+ r l) (- r l)) 0.]
           [0. (/ (* 2. n) (- t b)) (/ (+ t b) (- t b)) 0.]
           [0. 0. (- (/ (+ f n) (- f n))) (- (/ (* 2. f n) (- f n)))]
           [0. 0. -1. 0.]]]
    (matrix M)))

(defn project 
  "project homogenous point to r3"
  [p]
  (if (zero? (p 3))
    p
    (v4muls p (/ (p 3)))))

(defn ^double angle-of-view [^double plane-width ^double plane-distance]
  (* 2.0 (Math/atan (/ plane-width (* 2.0 plane-distance)))))

(defn point3 "Construct a point3" [^double x ^double y ^double z]
  [x y z 1.0])

(defn vector3 "Construct a vector3" [^double x ^double y ^double z]
  [x y z 0.0])

(defn normal "Construct a normal vector" 
  [^double x ^double y ^double z]
  [x y z 0.0])

(defn transform-point "Transform p by T" [p ^Transform T ] 
  (m4vmul (.transform T) p))
(defn transform-vector "Transform v by T" [v ^Transform T] 
  (m4vmul (.transform T) v))
(defn transform-normal "Transform n by T" [n ^Transform T] 
  (m34vmul (m4transpose (.inverse T)) n))
