(in-ns 'sligeom.core)

(deftype Plane [position normal]
  RayIntersection
  (intersect [this _r]
    (let [^Ray r _r
          rdotn (vdot3 (.direction r) normal)]
      (if (> (numeric/abs rdotn) eps-small)
        (let [t (/ (vdot3 (vsub3 position (.origin r)) normal) rdotn)]
          (if (> (.maxt r) t (.mint r)) t)))))

  Bounded
  (bounding-box [this]
    ;; Bounding-box for an infinite plane...
    (bbox))
  Object 
  (toString [this] (str position " " normal)))

(defn plane [position normal] (Plane. position normal))


