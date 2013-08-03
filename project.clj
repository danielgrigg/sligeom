(defproject sligeom "0.2.0"
  :description "Geometric functions"
  :url "http://sliplanesoftware.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "LATEST"]
                 [slimath "LATEST"]
                 [org.clojure/math.numeric-tower "LATEST"]]
  :profiles {:dev {:dependencies [[midje "1.5.0"]]}}
  :jvm-opts ["-Xmx2048M"])
