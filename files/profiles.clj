{:user {:plugins [[lein-ancient "0.7.0"]
                  [lein-pprint "1.3.2"]]}
 :repl {:global-vars {*print-length* 100}
        :middleware [cider-nrepl.plugin/middleware
                     refactor-nrepl.plugin/middleware]
        :plugins [[cider/cider-nrepl "RELEASE"]
                  [refactor-nrepl/refactor-nrepl "RELEASE"]]}}

