(ns sligeom.aggregate-test
  (:use midje.sweet
        [sligeom core bounding intersect aggregate]))

(fact "`voxel-size` voxel size given divisions"
      (voxel-size (->Grid3 (bbox (point3 2 3 5) (point3 7 13 9))
                  [2 5 4]))
      => [2.5 2.0 1.0])

(fact "`point-to-voxel` for a point"
      (point-to-voxel (->Grid3 (bbox (point3 2 3 5) (point3 7 13 9)) 
                        [5 5 5])
                        [2 3 5]) 
      => [0 0 0]
                  
      (point-to-voxel (->Grid3 (bbox (point3 2 3 5) (point3 7 13 9))
                        [2 5 4])
                        [3 8 9])
      => [0 2 4])

(fact "`voxel-to-point`"
      (voxel-to-point (->Grid3 (bbox (point3 2 3 5) (point3 7 13 9)) 
                        [5 5 5 1])
                        [0 0 0])
      => [2.0 3.0 5.0]
                  
      (voxel-to-point (->Grid3 (bbox (point3 2 3 5) (point3 7 13 9))
                        [2 5 4])
                        [0 2 4])
      => [2.0 7.0 9.0])

(fact "`voxel-seq3` produces a grid-walk seq"
      (grid3-seq (->Grid3 (bbox (point3 0 0 0) (point3 10 10 10)) [4 5 6]) 
                  (ray (point3 0 0 0) (vector3 1 1 1)))
      => [[0 0 0] [0 0 1] [0 1 1] [1 1 1] [1 1 2] [1 2 2] [1 2 3] 
          [2 2 3] [2 3 3] [2 3 4] [3 3 4] [3 4 4] [3 4 5]])