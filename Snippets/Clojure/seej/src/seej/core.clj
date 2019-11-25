(ns seej.core
  (:gen-class))

(import 'com.github.javaparser.ParserConfiguration
        'com.github.javaparser.ParserConfiguration$LanguageLevel
        'com.github.javaparser.JavaParser
        'com.github.javaparser.ParseResult
        'com.github.javaparser.ast.CompilationUnit
        'com.github.javaparser.ast.body.CallableDeclaration)

(defn print-problems [result]
  (println "Problems:")
  (doseq [problem (.getProblems result)]
    (println "  " (.getMessage problem))))

(defn create-config []
  (let [config (new ParserConfiguration)]
    (.setLanguageLevel config ParserConfiguration$LanguageLevel/JAVA_8)
    config))

(defn print-functions [comp-unit]
  (println "Functions:")
  (doseq [func (.findAll comp-unit CallableDeclaration)]
    (println "  " (.getIdentifier (.getName func)))))

(defn -main [filename function]
  (println "Hello, this is Seej.")
  (println "  Filename:" filename)
  (println "  Function:" function)

  (let [config (create-config)
        parser (new JavaParser config)]
    (with-open [input-file (clojure.java.io/reader filename)]
      (let [result (.parse parser input-file)]
        (if (.isSuccessful result)
          (print-functions (.get (.getResult result)))
          (print-problems result))))))

    ;;     parser (new JavaParser config)]
    ;; (do (.setLanguageLevel config ParserConfiguration$LanguageLevel/JAVA_8)
    ;;     (with-open [input-file (clojure.java.io/reader filename)]
    ;;       (println (.parse parser input-file))))))
