;;;; -*- mode: lisp-data; coding: utf-8 -*-
;;;; cyclops.sexp -- the clnet gate for the lab end-to-end rig (README.md
;;;; beside this file): clnet's deploy/cyclops.sexp with the forge, the
;;;; redirect and the lab's hosting door named on the rig's own network.
;;;; The gate holds no API key here, so the lab's reviewer is never
;;;; reached and every hosting request is held for a person.
(:port 80
 :utility-port 9191
 :debug? nil
 :llm-gate
 (:upstream "https://api.anthropic.com/v1/messages"
  :model "claude-opus-5-5" :efforts ("low") :max-tokens 8000
  :session-header "X-Image-Lab-Session"
  :pot-name "community" :pot-seed-credits 500 :pot-max-credits 9900 :pot-low-credits 100
  :credit-markup 1.3
  :notify-name "lab e2e pot" :notify-sessions? nil
  :topup-amounts ((1000 1000) (2000 2500)) :topup-name "lab e2e pot" :currency "usd"
  :unit-names ("mite" "mites")
  :allow-from ("172." "10." "192.168."))
 :git-gate
 (:forge "http://anvil:9095"
  :redirect-uri "http://cyclops/_git/callback"
  :allow-from ("172." "10." "192.168.")
  :deploy-url "http://lab:9080/prompt-lab/api/hosting/request")
 :rules
 ((:path "/llm/image-messages" :llm-gate t)
  (:path "/llm/lab-messages" :llm-gate t
   :session-header "X-Prompt-Lab-Session" :wallet-header "X-Prompt-Lab-Wallet"
   :efforts ("low" "medium") :max-tokens 16000)
  (:path "/_git" :git-gate t)))
