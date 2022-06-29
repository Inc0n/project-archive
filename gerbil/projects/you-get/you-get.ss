
(import :std/getopt
        :std/sugar  ;; chain
        :std/format)

(import :gerbil/gambit/ports ;; read-line
        :gerbil/gambit/exceptions)

(import :you-get/common)

(export main)

(def +file-name+ "you-get")

(_gx#load-expander!)

(def (opt-get table key)
  (hash-get table key))
(defalias opt-get? hash-get)

(def (session gopt)
  "provides the repl interface for `you-get'"
  (chain (read-line)
	(string-split <> #\space)
	(you-get-eval gopt <>)))

(def (you-get-eval gopt args)
  (let ((values cmd opt) (getopt-parse gopt args))
    (case cmd
      ((get) (you-get (opt-get opt 'url)
					  (opt-get? opt 'info)
					  (opt-get? opt 'dir)))
	  ((session) (session gopt))
      ((help) (getopt-display-help-topic gopt
										 (opt-get? opt 'command)
										 +file-name+)))))

(def (main . args)
  (def you-get-cmd
    (command 'get help: "you-get the url resource"
             (argument 'url help: "url to you-get")
             (flag 'info "-i" "--info" help: "get the info for the url")
             (option 'dir "-d" "--dir" help: "set output directory")))
  (def session-cmd
	(command 'session help: "you-get repl"))
  (def help-cmd
    (command 'help help: "display this help message"
             (optional-argument 'command value: string->symbol)))
  (def gopt (getopt you-get-cmd
					session-cmd
                    help-cmd))
  (try
   (you-get-eval gopt args)
   (catch (getopt-error? exn)
     (getopt-display-help exn +file-name+ (current-error-port))
     (exit 1))
   (catch (e)
     (display-exception e (current-error-port))
     (exit 2))))