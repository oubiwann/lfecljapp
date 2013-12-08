(defproject cljnode "0.1.0"
  :jvm-opts ["-Ddev_env=dev" "-Dnode=cljnode@127.0.0.1" "-Dmbox=mboxname"
             "-Dcookie=cookie" "-Depmd_port=15000"]
  :description "clojure + erlang"
  :url "http://vonmo.com/projects/cljnode"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :resource-paths [ ]
  :dependencies [ [org.clojure/clojure "1.5.1"]
                  [org.clojure/tools.logging "0.2.4"]
                  [org.slf4j/slf4j-log4j12 "1.7.1"]
                  [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                     javax.jms/jms
                                                     com.sun.jmdk/jmxtools
                                                     com.sun.jmx/jmxri]]
                  [com.ericsson.otp.erlang/otperlang "1.5.3"]]
  :main ^:skip-aot cljnode.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
