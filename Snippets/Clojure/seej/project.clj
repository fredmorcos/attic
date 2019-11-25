(defproject seej "0.1.0-SNAPSHOT"
  :description "Symbolic Execution Engine for Java"
  :url "N/A"
  :license {:name "Proprietary" :url ""}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.github.javaparser/javaparser-symbol-solver-core "3.14.5"]
                 [com.github.javaparser/javaparser-core-serialization "3.14.5"]
                 [com.github.javaparser/javaparser-core "3.14.5"]]
  :main seej.core
  :aot [seej.core]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
