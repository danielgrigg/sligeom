(in-ns 'sligeom.core)

(defprotocol Transformable
  (transform [this T] "Transform the object by T"))

(defprotocol Bounded
  (bounding-box [this] "Compute the AABB"))

(definterface Bounding
  (^double width [])
  (^double height [])
  (^double depth []))


(deftype Ray [origin direction ^double mint ^double maxt]
  Transformable
  (transform [this T]
    (Ray. (transform-point (.origin this) T)
          (transform-vector (.direction this) T)
          mint
          maxt))
            
  Object
  (toString [this]
    (apply str
           (.origin this) " "
           (.direction this) " "
           (.mint this) " " 
           (.maxt this) " ")))

(defn ray-at [^Ray r ^double t]
  (v4add (.origin r) (v4muls (.direction r) t)))

(defn ray-interval "Confine the ray r to an interval"
  ([^Ray r ^double mint ^double maxt]
     (Ray. (.origin r) (.direction r) mint maxt))
  ([^Ray r ^double maxt]
     (Ray. (.origin r) (.direction r) (.mint r) maxt)))

(defn ^Ray ray
  ([[ox oy oz ow :as origin] [dx dy dz dw :as direction]]
     (Ray. origin direction eps infinity))
  ([[ox oy oz ow :as origin] [dx dy dz dw :as direction] mint maxt]
     (Ray. origin direction mint maxt)))


